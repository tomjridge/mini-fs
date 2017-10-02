open Minifs

(* in-mem impl ------------------------------------------------------ *)

module Fid : sig
  type fid = int  (* FIXME hide *)
  val fid0 : fid
  val inc_fid : fid -> fid
end = struct
  type fid=int
  let fid0=0
let  inc_fid x = x+1
end
include Fid
 
module Did : sig
  type did = int (* FIXME hide *)
  val root_did : did
  val inc_did : did -> did
end = struct
  type did = int
  let root_did = 0
  let inc_did x = x+1 
end
include Did

module Map_fid = Map.Make(
  struct 
    type t = fid let compare: t->t->int = Pervasives.compare 
  end)


module Map_did = Map.Make(
  struct 
    type t = did let compare: t->t->int = Pervasives.compare 
  end)


module Map_string = Map.Make(
  struct 
    type t = string let compare: t->t->int = Pervasives.compare 
  end)


module Set_string = Set.Make(
  struct
    type t = string let compare: t->t->int = Pervasives.compare 
  end)


type id = Fid of fid | Did of did


type dir = id Map_string.t


type fs_t = {
  files: string Map_fid.t;
  max_fid: fid;
  dirs: dir Map_did.t;
  max_did: did;
}


let empty_dir = Map_string.empty


let init_fs = {
  files=Map_fid.empty;
  max_fid=fid0;
  dirs=(Map_did.empty |> Map_did.add root_did empty_dir);
  max_did=root_did;
}


type t = fs_t


type dh = did * string list (* inefficient *)


type fd = fid


type path = string


type buffer = bytes  (* or cstruct? *)



(* monad ops -------------------------------------------------------- *)


module X_ = Mk_state_passing(struct type w = fs_t end)
let ( >>= ) = X_.( >>= )
let return = X_.return


(* following could be parameters *)
let with_fs (f:fs_t -> fs_t) : (unit,'m)m_ = 
  X_.with_state f (return ())

let with_fs' (f:fs_t -> 'a * fs_t) : ('a,'m)m_ = 
  X_.with_state' f (fun a -> return a)


type ('e,'m) extra_ops = {
  err: 'a. 'e -> ('a,'m)m_;
  new_did: unit -> (did,'m)m_;
  new_fid: unit -> (fid,'m)m_;
}



(* fs ops ----------------------------------------------------------- *)


let is_fid = function
  | Fid _ -> true
  | _ -> false


let is_did x = not (is_fid x)


let mk_ops ~extra () = 

  let resolve_did did = 
    with_fs' (fun s ->
        (Map_did.find did s.dirs,s))  (* ASSUME did valid *)
  in

  let resolve_name ~dir ~name : id option = 
    match Map_string.find name dir with
    | exception _ -> None
    | x -> Some x
  in

  let rec resolve_names_1 ~parent_id ~names = 
    resolve_did parent_id >>= fun parent -> resolve_names_2 ~parent_id ~parent ~names 
  and resolve_names_2 ~parent_id ~parent ~names = 
    match names with
    | [] -> return @@ (parent_id,None)
    | name::names -> 
      begin
        resolve_name ~dir:parent ~name |> function
        | None -> (
            match names with
            | [] -> return @@ (parent_id,None)
            | _ -> extra.err @@ `S __LOC__)
        | Some (Fid fid) -> (
            match names with 
            | [] -> return @@ (parent_id,Some (Fid fid))
            | _ -> extra.err @@ `S __LOC__)
        | Some (Did did) -> (
            match names with
            | [] -> return @@ (parent_id,Some (Did did))
            | _ -> resolve_names_1 ~parent_id:did ~names)
      end
  in


  let resolve_path : path -> (did * id option,'m) m_ = fun p ->     
    String.split_on_char '/' p |> fun names ->
    (* remove head "" since paths are absolute, and any trailing "" *)
    assert(List.hd names = "");
    let names = List.tl names in
    assert(names <> []);
    let names = Tjr_list.(if last names = "" then butlast names else names) in
    (* not sure about special casing root *)
    if names = [] 
    then return @@ (root_did,Some (Did root_did)) 
    else resolve_names_1 ~parent_id:root_did ~names
  in


  let resolve_dir_path (path:path) : (did,'m) m_ = 
    resolve_path path >>= function
    | (_,Some (Did did)) -> return did 
    | _ -> extra.err (`S __LOC__)
  in


  let resolve_file_path (path:path) : (fid,'m) m_ = 
    resolve_path path >>= function
    | (_,Some (Fid fid)) -> return fid 
    | _ -> extra.err (`S __LOC__)
  in

  let root : path = "/" in


  (* FIXME or just allow unlink with no expectation of the kind? *)
  let unlink ~parent ~name = 
    resolve_dir_path parent >>= fun parent ->
    with_fs 
      (fun s ->
         s.dirs |> fun dirs ->
         Map_did.find parent dirs |> fun pdir ->
         Map_string.find name pdir |> fun entry ->
         (* FIXME here and elsewhere we need to take care about find etc when key not present *)
         Map_string.remove name pdir |> fun pdir ->
         Map_did.add parent pdir dirs |> fun dirs ->
         {s with dirs})
    >>= fun () -> return ()
  in


  let mkdir ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun parent ->
    extra.new_did () >>= fun (did:did) -> 
    with_fs 
      (fun s -> 
         s.dirs |> fun dirs ->
         (* add new empty dir to dirs *)
         Map_did.add did empty_dir dirs |> fun dirs ->
         (* add name to parent *)
         Map_did.find parent s.dirs |> fun pdir ->
         Map_string.add name (Did did) pdir |> fun pdir ->
         (* update parent in dirs *)
         Map_did.add parent pdir dirs |> fun dirs ->
         {s with dirs})
    >>= fun () -> return () (* did *)
  in


  let mk_dh ~did es = (did,es) in


  let opendir path = 
    resolve_dir_path path >>= fun did ->
    with_fs' @@ fun s ->
    s.dirs |> fun dirs ->
    Map_did.find did dirs |> fun dir ->
    Map_string.bindings dir |> List.map fst |> fun names ->
    (mk_dh ~did names,s)      
  in


  let readdir dh = dh |> function (did,es) -> return (es,finished) in


  let closedir dh = return () in (* FIXME should we record which rd are valid? ie not closed *)


  let create ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun parent ->
    extra.new_fid () >>= fun (fid:fid) -> 
    with_fs 
      (fun s -> 
         s.dirs |> fun dirs ->
         Map_did.find parent dirs |> fun pdir ->
         Map_string.add name (Fid fid) pdir |> fun pdir ->
         Map_did.add parent pdir dirs |> fun dirs ->
         {s with dirs})
    >>= fun () -> return () (* fid *)
  in


  let mk_fd (fid:fid) = fid in


  let open_ path = 
    resolve_file_path path >>= fun fid -> 
    fid |> mk_fd |> return 
  in

  let pread ~fd ~foff ~length ~buffer ~boff = 
    let fid = fd in
    with_fs' @@ fun s ->
    s.files |> fun map ->
    Map_fid.find fid map |> fun (contents:string) ->
    Bytes.blit_string contents foff buffer boff length;
    (length,s)
  in

  let pwrite ~fd ~foff ~length ~buffer ~boff = 
    let fid = fd in
    with_fs' @@ fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    let contents = Bytes.of_string contents in
    Bytes.blit buffer boff contents foff length;  (* FIXME extend contents *)
    Bytes.to_string contents |> fun contents ->
    Map_fid.add fid contents files |> fun files ->
    (length,{s with files})
  in

  let close fd = return () in (* FIXME record which are open? *)


  let truncate ~path ~length = 
    resolve_file_path path >>= fun fid ->
    with_fs' @@ fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    let contents' = Bytes.create length in
    Bytes.blit_string contents 0 contents' 0 length;
    Bytes.to_string contents' |> fun contents ->
    Map_fid.add fid contents files |> fun files ->
    ((),{s with files})
  in


  let stat_file path = 
    resolve_file_path path >>= fun fid ->
    with_fs' @@ fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    String.length contents |> fun sz ->
    ({ sz },s)
  in


  let kind path : (st_kind,'m) m_ = 
    resolve_path path >>= fun (_,id) ->    
    id |> function 
    | None -> extra.err @@ `No_such_entry
    | Some x -> x |> function
      | Fid fid -> return (`File:st_kind)
      | Did did -> return (`Dir:st_kind)
  in


  let reset () = return () in



  let _ = wf_ops 
      ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
      ~create ~open_ ~pread ~pwrite ~close ~truncate 
      ~stat_file ~kind ~reset    
  in

  mk_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset

let _ = mk_ops  (* NOTE the error cases are captured in the type *)


exception E of [ `No_such_entry | `S of string ]

type w = fs_t
type ww = w -> w
type 'a m = ('a -> ww) -> ww
let err e = 
  fun (g: 'a -> ww) ->
  fun w ->
    raise (E e)

let new_did () = 
  fun (g:did -> ww) ->
  fun w ->
    let w' = { w with max_did=(inc_did w.max_did) } in
    let did = w'.max_did in
    g did w'

let new_fid () =
  fun (g:fid -> ww) ->
  fun w ->
    let w' = { w with max_fid=(inc_fid w.max_fid) } in
    let fid = w'.max_fid in
    g fid w'
      
let extra = { err; new_did; new_fid }


let ops = mk_ops ~extra ()  (* NOTE the error cases are captured in the type *)

let _ = ops


(* imperative ------------------------------------------------------- *)

module W_ = struct
  type w = fs_t
end

module Mk_state_passing_ = Mk_state_passing(W_)

let ref_ = ref init_fs

let imperative_ops = Mk_state_passing_.mk_imperative_ops ops ref_
