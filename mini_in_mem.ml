open Mini_pervasives
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
  let root_did = 1234
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



(* type buffer = bytes  (* or cstruct? *) *)
type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t


(* monad ops -------------------------------------------------------- *)


(* state passing with error *)

type ('e,'w,'m) monad_ops = {
  return: 'a. 'a -> ('a,'m)m_;
  bind: 'a 'b. ('a,'m)m_ -> ('a -> ('b,'m)m_) -> ('b,'m)m_;
  err: 'a. 'e -> ('a,'m)m_;
}

type ('e,'m) extra_ops = {
  new_did: unit -> (did,'m)m_;
  new_fid: unit -> (fid,'m)m_;
  with_fs: 'a. (t -> 'a * t) -> ('a,'m)m_;  (* ASSUME-ed not to raise exception *)
}



(* fs ops ----------------------------------------------------------- *)


let is_fid = function
  | Fid _ -> true
  | _ -> false


let is_did x = not (is_fid x)


(* logging, within the monad *)

(* note the with_fs in the following forces 'm = ww *)
let mk_ops ~monad_ops ~extra = 

  let (bind,return,err) = (monad_ops.bind,monad_ops.return,monad_ops.err) in
  let ( >>= ) = bind in

  let resolve_did did = 
    extra.with_fs @@ fun s ->
    match Map_did.find did s.dirs with
    | exception e -> failwith "!!!fatal error mim.l136"  (* ASSUME did valid *)
    | dir -> (dir,s)
  in

  let _ = resolve_did in

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
            | _ -> err @@ `Error_no_entry name) (* FIXME give full path *)
        | Some (Fid fid) -> (
            match names with 
            | [] -> return @@ (parent_id,Some (Fid fid))
            | _ -> err @@ `Error_not_directory)
        | Some (Did did) -> (
            match names with
            | [] -> return @@ (parent_id,Some (Did did))
            | _ -> resolve_names_1 ~parent_id:did ~names)
      end
  in


  let _ = resolve_names_1 in

  let resolve_path : path -> (did * id option,'m) m_ = fun p -> 
    extra.with_fs (fun s -> 
        print_endline @@ "# resolve_path "^p ^ " l167";
        ignore(String.contains p '/' || "!!!fatal resolve_path.l176" |> fun s ->
              print_endline s; failwith s);
        String.split_on_char '/' p |> fun names ->

        (* remove head "" since paths are absolute, and any trailing "" *)
        ignore(List.hd names = "" || 
               "!!!fatal: hd names not empty mim.l176" |> fun s ->
              print_endline s; failwith s);
        let names = List.tl names in

        ignore(names <> [] || 
               "!!!fatal: assertion failure: names=[] mim.l178" |> fun s ->
               print_endline s; failwith s);
        let names = Tjr_list.(if last names = "" then butlast names else names) in
        (* not sure about special casing root *)
        names,s) >>= fun names ->
    if names = [] 
    then return @@ (root_did,Some (Did root_did)) 
    else resolve_names_1 ~parent_id:root_did ~names
  in

  let _ = resolve_path in


  let resolve_dir_path (path:path) : (did,'m) m_ = 
    resolve_path path >>= function
    | (_,Some (Did did)) -> return did 
    | _ -> err (`Error_not_directory)
  in


  let resolve_file_path (path:path) : (fid,'m) m_ = 
    resolve_path path >>= function
    | (_,Some (Fid fid)) -> return fid 
    | _ -> err (`Error_not_file)
  in

  let root : path = "/" in


  (* FIXME or just allow unlink with no expectation of the kind? *)
  let unlink ~parent ~name = 
    resolve_dir_path parent >>= fun parent ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        Map_did.find parent dirs |> fun pdir ->
        Map_string.find name pdir |> fun entry ->
        (* FIXME here and elsewhere we need to take care about find etc when key not present *)
        Map_string.remove name pdir |> fun pdir ->
        Map_did.add parent pdir dirs |> fun dirs ->
        (),{s with dirs})
  in

  let _ = unlink in 


  let mkdir ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun parent ->
    extra.new_did () >>= fun (did:did) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        (* add new empty dir to dirs *)
        Map_did.add did empty_dir dirs |> fun dirs ->
        (* add name to parent *)
        Map_did.find parent s.dirs |> fun pdir ->
        Map_string.add name (Did did) pdir |> fun pdir ->
        (* update parent in dirs *)
        Map_did.add parent pdir dirs |> fun dirs ->
        (),{s with dirs})
  in


  let mk_dh ~did es = (did,es) in


  let opendir path = 
    resolve_dir_path path >>= fun did ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        Map_did.find did dirs |> fun dir ->
        Map_string.bindings dir |> List.map fst |> fun names ->
        (mk_dh ~did names,s))
  in


  let readdir dh = dh |> function (did,es) -> return (es,finished) in


  let closedir dh = return () in (* FIXME should we record which rd are valid? ie not closed *)


  let create ~parent ~name : (unit,'m) m_ = 
    extra.with_fs (fun s ->
        Printf.printf "# create p(%s) n(%s) l251\n" parent name;
        (),s) >>= fun () ->
    resolve_dir_path parent >>= fun parent ->
    print_endline @@ "# l253";
    extra.new_fid () >>= fun (fid:fid) -> 
    print_endline @@ "# l255";
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        Map_did.find parent dirs |> fun pdir ->
        Map_string.add name (Fid fid) pdir |> fun pdir ->
        Map_did.add parent pdir dirs |> fun dirs ->
        {s with dirs} |> fun s ->
        s.files |> fun files -> 
        Map_fid.add fid "" files |> fun files ->
        print_endline @@ "# l265";
        (),{s with files})
  in


  let mk_fd (fid:fid) = fid in


  let open_ path = 
    resolve_file_path path >>= fun fid -> 
    fid |> mk_fd |> return 
  in

  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    let fid = fd in
    extra.with_fs (fun s ->
        s.files |> fun map ->
        Map_fid.find fid map |> fun (contents:string) ->
        blit_string_to_bigarray ~src:contents ~soff:foff ~len:length ~dst:buffer ~doff:boff;
        (length,s))
  in

  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    let fid = fd in
    extra.with_fs (fun s ->
        s.files |> fun files ->
        Map_fid.find fid files |> fun contents ->
        let contents = Bytes.of_string contents in
        blit_bigarray_to_bytes ~src:buffer ~soff:boff ~len:length ~dst:contents ~doff:foff; 
        (* FIXME extend contents *)
        Bytes.to_string contents |> fun contents ->
        Map_fid.add fid contents files |> fun files ->
        (length,{s with files}))
  in


  let close fd = return () in (* FIXME record which are open? *)


  let truncate ~path ~length = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        Map_fid.find fid files |> fun contents ->
        let contents' = Bytes.create length in
        Bytes.blit_string contents 0 contents' 0 length;
        Bytes.to_string contents' |> fun contents ->
        Map_fid.add fid contents files |> fun files ->
        ((),{s with files}))
  in


  let stat_file path = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        Map_fid.find fid files |> fun contents ->
        String.length contents |> fun sz ->
        ({ sz },s))
  in


  let kind path : (st_kind,'m) m_ = 
    resolve_path path >>= fun (_,id) ->    
    id |> function 
    | None -> err @@ `Error_no_entry path
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


(* instantiate monad ------------------------------------------------ *)


type exn_ = [ 
    | `Error_no_entry of string 
    | `Error_not_directory
    | `Error_not_file
] [@@deriving yojson]

module X_ = Mk_state_passing_with_error(struct
    type w = fs_t
    type e = exn_
end)


open X_

let monad_ops = X_.{
  return;
  bind=X_.( >>= );
  err;
}


let with_fs (f:fs_t -> 'a * fs_t) : ('a,'m)m_ = 
  X_.with_state'' f (fun a -> return a)


let new_did () = with_fs (fun w ->
    let w' = { w with max_did=(inc_did w.max_did) } in
    let did = w'.max_did in
    did,w')

let new_fid () = with_fs (fun w ->
    let w' = { w with max_fid=(inc_fid w.max_fid) } in
    let fid = w'.max_fid in
    fid,w')

      
let extra = { new_did; new_fid; with_fs }


let ops = mk_ops ~monad_ops ~extra  (* NOTE the error cases are captured in the type *)

let _ = ops


(* imperative ------------------------------------------------------- *)

(*
module W_ = struct
  type w = fs_t
end

module Mk_state_passing_ = Mk_state_passing(W_)

let ref_ = ref init_fs

let (run,imperative_ops) = 
  Mk_state_passing_.mk_imperative_ops ops ref_ @@ fun ~run ~ops -> (run,ops)
*)


(* logging ---------------------------------------------------------- *)

open Msgs

let log_return (x : ('a,ww)m_) : ('a,ww)m_ = 
  fun (k:'a -> ww) ->
  fun (w:exn_ option * fs_t) ->
    x k w |> fun (e,fs) ->
    let tid = thread_id () in
    match e with 
    | None -> 
      Printf.printf "# thread %d returning mim.l434\n" tid;
      (e,fs)
    | Some e ->
      exn__to_yojson e |> Yojson.Safe.pretty_to_string |> fun e' ->
      Printf.printf "# log_return thread %d returning exception %s mim.log_return.l438\n" tid e';
      (Some e,fs)

let _ = log_return

let log_call c : (unit,ww)m_ = 
  with_fs (fun s -> 
      let tid = thread_id () in
      c |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string
      |> fun c -> 
      Printf.printf "# log_call thread_id(%d) call(%s) mim.log_call.l421\n" tid c;
      (),s)  

(* FIXME this logging depends on the exact nature and semantics of the
   monad; here we assume no exceptions are thrown, ie working with e
   option * w *)
let log (c:msg_from_client) : ('a,ww) m_ -> ('a,ww) m_ = fun m ->
  log_call c >>= (fun () -> m |> log_return)



(* raising exceptions ----------------------------------------------- *)

(*

exception E of exn_


let err e = 
  fun (g: 'a -> ww) ->
  fun w ->
    raise (E e)


*)