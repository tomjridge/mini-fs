open Tjr_map
open Mini_pervasives
open Minifs

(* in-mem impl ------------------------------------------------------ *)

module Fid : sig
  type fid = int[@@deriving yojson]  (* FIXME hide *)
  val fid0 : fid
  val inc_fid : fid -> fid
end = struct
  type fid=int[@@deriving yojson]
  let fid0=0
let  inc_fid x = x+1
end
include Fid
 
module Did : sig
  type did = int[@@deriving yojson] (* FIXME hide *)
  val root_did : did
  val inc_did : did -> did
end = struct
  type did = int[@@deriving yojson]
  let root_did = 1234
  let inc_did x = x+1 
end
include Did

module Map_fid = Tjr_map.Make(
  struct 
    type t = fid let compare: t->t->int = Pervasives.compare 
  end)


module Map_did = Tjr_map.Make(
  struct 
    type t = did let compare: t->t->int = Pervasives.compare 
  end)


module Map_string = Tjr_map.Make(
  struct 
    type t = string let compare: t->t->int = Pervasives.compare 
  end)


module Set_string = Tjr_set.Make(
  struct
    type t = string let compare: t->t->int = Pervasives.compare 
  end)


type id = Fid of fid | Did of did[@@deriving yojson]

type dir_carrier = id Map_string.Map_.t
type dir_ops = (string,id,dir_carrier) map_ops
let dir_ops : dir_ops = Map_string.map_ops

type files_carrier = string Map_fid.Map_.t
type files_ops = (fid,string,files_carrier) map_ops
let files_ops : files_ops = Map_fid.map_ops

type dirs_carrier = dir_carrier Map_did.Map_.t
type dirs_ops = (did,dir_carrier,dirs_carrier) map_ops
let dirs_ops : dirs_ops = Map_did.map_ops

type fs_t = {
  files: files_carrier;
  max_fid: fid;
  dirs: dirs_carrier;
  max_did: did;
}

module Fs_json = struct
  (* an easily-jsonable version *)
  type fs = {
    files: (fid * string) list;
    max_fid: fid;
    dirs: (string * id) list;
    max_did: did;
  } [@@deriving yojson]
end

let empty_dir = dir_ops.map_empty

let init_fs () = {
  files=files_ops.map_empty;
  max_fid=fid0;
  dirs=(dirs_ops.map_empty |> dirs_ops.map_add root_did empty_dir);
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
  internal_err: 'a. string -> ('a,'m)m_;
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
    extra.with_fs (fun s ->
        (dirs_ops.map_find did s.dirs,s)) >>= fun r -> 
    match r with 
    | None -> extra.internal_err "resolve_did, did not valid mim.l154"
    | Some dir -> return dir
  in

  let _ = resolve_did in

  let resolve_name ~dir ~name : id option = dir_ops.map_find name dir in

  let rec resolve_names_1 ~parent_id ~names : (did * id option,'m) m_ = 
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
    (if not @@ String.contains p '/' 
     then extra.internal_err "resolve_path, no slash, mim.l189" 
     else 
       String.split_on_char '/' p |> fun names ->
       if not (List.hd names = "")
       then extra.internal_err "resolve_path, no leading slash, mim.194"
       else 
         let names = List.tl names in
         if names=[] 
         then extra.internal_err "resolve_path, names empty, impossible, mim.198"
         else 
           let names = Tjr_list.(if last names = "" then butlast names else names) in
           (* not sure about special casing root *)
           if names = [] 
           then return @@ (root_did,Some (Did root_did)) 
           else resolve_names_1 ~parent_id:root_did ~names)
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
    resolve_dir_path parent >>= fun pid ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_find pid dirs |> fun pdir ->
        match pdir with 
        | None -> `Internal "impossible, parent not found, mim.233",s
        | Some pdir -> 
          dir_ops.map_find name pdir |> fun entry ->
          match entry with
          | None -> `Internal "no name in pdir, mim.237",s
          | Some entry -> 
            dir_ops.map_remove name pdir |> fun pdir ->
            dirs_ops.map_add pid pdir dirs |> fun dirs ->
            `Ok,{s with dirs}) >>= function 
    | `Ok -> return ()
    | `Internal s -> extra.internal_err s
  in

  let _ = unlink in 


  let mkdir ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun pid ->
    extra.new_did () >>= fun (did:did) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        (* add new empty dir to dirs *)
        dirs_ops.map_add did empty_dir dirs |> fun dirs ->
        (* add name to parent *)
        dirs_ops.map_find pid s.dirs |> fun pdir ->
        match pdir with 
        | None -> `Internal "impossible, parent not found mim.l259",s
        | Some pdir -> 
          dir_ops.map_add name (Did did) pdir |> fun pdir ->
          (* update parent in dirs *)
          dirs_ops.map_add pid pdir dirs |> fun dirs ->
          `Ok,{s with dirs}) >>= function
    | `Ok -> return ()
    | `Internal s -> extra.internal_err s
  in


  let mk_dh ~did es = (did,es) in


  let opendir path = 
    resolve_dir_path path >>= fun did ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_find did dirs |> function
        | None -> `Internal "opendir, impossible, dir not found mim.278",s
        | Some dir -> 
          dir_ops.map_bindings dir |> List.map fst |> fun names ->
          `Ok(mk_dh ~did names),s) >>= function
    | `Ok dh -> return dh
    | `Internal s -> extra.internal_err s
  in


  let readdir dh = dh |> function (did,es) -> return (es,finished) in


  let closedir dh = return () in (* FIXME should we record which rd are valid? ie not closed *)


  let create ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun parent ->
    extra.new_fid () >>= fun (fid:fid) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        dirs_ops.map_find parent dirs |> function
        | None -> `Internal "create, impossible, dir not found mim.299",s
        | Some pdir ->
          dir_ops.map_add name (Fid fid) pdir |> fun pdir ->
          dirs_ops.map_add parent pdir dirs |> fun dirs ->
          {s with dirs} |> fun s ->
          s.files |> fun files -> 
          files_ops.map_add fid "" files |> fun files ->
          `Ok,{s with files}) >>= function
    | `Internal s -> extra.internal_err s
    | `Ok -> return ()
  in


  let mk_fd (fid:fid) = fid in


  let open_ path = 
    resolve_file_path path >>= fun fid -> 
    fid |> mk_fd |> return 
  in

  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    let fid = fd in
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "pread, impossible, no file, mim.325",s
        | Some(contents:string) ->
          blit_string_to_bigarray 
            ~src:contents ~soff:foff ~len:length 
            ~dst:buffer ~doff:boff;
          (`Ok length,s)) >>= function
    | `Internal s -> extra.internal_err s
    | `Ok l -> return l
  in

  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    let fid = fd in
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "pwrite, impossible, no file, mim.339",s
        | Some contents ->
          let contents = Bytes.of_string contents in
          blit_bigarray_to_bytes 
            ~src:buffer ~soff:boff ~len:length ~dst:contents ~doff:foff; 
          (* FIXME extend contents *)
          Bytes.to_string contents |> fun contents ->
          files_ops.map_add fid contents files |> fun files ->
          (`Ok length,{s with files})) >>= function
    | `Internal s -> extra.internal_err s
    | `Ok l -> return l
  in


  let close fd = return () in (* FIXME record which are open? *)


  let truncate ~path ~length = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "file not found mim.362",s
        | Some contents ->
          let contents' = Bytes.create length in
          Bytes.blit_string contents 0 contents' 0 length;
          Bytes.to_string contents' |> fun contents ->
          files_ops.map_add fid contents files |> fun files ->
          `Ok (),{s with files}) >>= function
    | `Ok () -> return ()
    | `Internal s -> extra.internal_err s
  in


  let stat_file path = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "file not found mim.379",s
        | Some contents ->
          String.length contents |> fun sz ->
          `Ok { sz },s) >>= function
    | `Ok stat -> return stat
    | `Internal s -> extra.internal_err s
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

open Dn_monad

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
*)
