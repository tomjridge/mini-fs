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

module Map_int = Tjr_map.Make(
  struct 
    type t = int let compare: t->t->int = Pervasives.compare 
  end)


module Set_string = Tjr_set.Make(
  struct
    type t = string let compare: t->t->int = Pervasives.compare 
  end)


type id = Fid of fid | Did of did[@@deriving yojson]


type name_map_carrier = id Map_string.Map_.t
type nm_ops = (string,id,name_map_carrier) map_ops
let nm_ops = Map_string.map_ops

type dir_with_parent = {
  name_map:name_map_carrier;
  parent:did
}

let (dir_empty,dir_find,dir_add,dir_remove,dir_bindings) = 
  let dir_empty ~parent = { name_map=nm_ops.map_empty; parent } in
  let dir_find k t = nm_ops.map_find k t.name_map in
  let dir_add k v t = {t with name_map=nm_ops.map_add k v t.name_map} in
  let dir_remove k t = {t with name_map=nm_ops.map_remove k t.name_map} in
  let dir_bindings t = nm_ops.map_bindings t.name_map in
  (dir_empty,dir_find,dir_add,dir_remove,dir_bindings)  

type files_carrier = string Map_fid.Map_.t
type files_ops = (fid,string,files_carrier) map_ops
let files_ops : files_ops = Map_fid.map_ops

type dirs_carrier = dir_with_parent Map_did.Map_.t
type dirs_ops = (did,dir_with_parent,dirs_carrier) map_ops
let dirs_ops : dirs_ops = Map_did.map_ops

type dh = int

type dhandles_carrier = string list Map_int.Map_.t
type dhandles_ops = (dh,string list,dhandles_carrier) map_ops
let dhandles_ops : dhandles_ops = Map_int.map_ops

type fs_t = {
  files: files_carrier;
  max_fid: fid;
  dirs: dirs_carrier;
  max_did: did;
  dir_handles: dhandles_carrier;
  max_dh: dh;
}

module X_ = struct
  (* an easily-jsonable version *)
  type fs' = {
    files: (fid * string) list;
    max_fid: fid;
    dirs: (did * ((string*id) list * did) ) list;
    max_did: did;
    (* FIXME dhandles *)
  } [@@deriving yojson]

  let from_fs (fs:fs_t) = {
    files=files_ops.map_bindings fs.files;
    max_fid=fs.max_fid;
    dirs=
      dirs_ops.map_bindings fs.dirs 
      |> List.map (fun (did,dir) -> 
          dir.name_map |> nm_ops.map_bindings |> fun bs ->
          (did,(bs,dir.parent)));
    max_did=fs.max_did;
  }
end

let fs_to_json fs = X_.(
  from_fs fs |> fs'_to_yojson |> Yojson.Safe.pretty_to_string)

let empty_dir ~parent = dir_empty ~parent

let init_fs = {
  files=files_ops.map_empty;
  max_fid=fid0;
  dirs=(dirs_ops.map_empty |> dirs_ops.map_add root_did (empty_dir ~parent:root_did));
  max_did=root_did;
  dir_handles=dhandles_ops.map_empty;
  max_dh=0;
}


type fd = fid


type path = string


(* type buffer = bytes  (* or cstruct? *) *)
type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t


(* monad ops -------------------------------------------------------- *)


type ('e,'m) extra_ops = {
  new_did: unit -> (did,'m)m_;
  new_fid: unit -> (fid,'m)m_;
  with_fs: 'a. (fs_t -> 'a * fs_t) -> ('a,'m)m_;  (* ASSUME-ed not to raise exception *)
  internal_err: 'a. string -> ('a,'m)m_;
  dirs_add: did -> dir_with_parent -> (unit,'m)m_;
  is_parent: parent:did -> child:dir_with_parent -> (bool,'m)m_;
}



(* fs ops ----------------------------------------------------------- *)


let is_fid = function
  | Fid _ -> true
  | _ -> false


let is_did x = not (is_fid x)


(* logging, within the monad *)

(* note the with_fs in the following forces 'm = ww *)
let mk_ops ~monad_ops ~extra = 

  let (bind,return,err) = Step_monad.(monad_ops.bind,monad_ops.return,monad_ops.err) in
  let ( >>= ) = bind in

  let resolve_did did = 
    extra.with_fs (fun s ->
        (dirs_ops.map_find did s.dirs,s)) >>= fun r -> 
    match r with 
    | None -> extra.internal_err "resolve_did, did not valid mim.l154"
    | Some dir -> return dir
  in

  let _ = resolve_did in

  let resolve_name ~dir ~name : id option = dir_find name dir in

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
    | _ -> err `Error_not_directory
  in


  let resolve_file_path (path:path) : (fid,'m) m_ = 
    resolve_path path >>= function
    | (_,Some (Fid fid)) -> return fid 
    | _ -> err `Error_not_file
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
          dir_find name pdir |> fun entry ->
          match entry with
          | None -> `Error_no_entry,s
          | Some entry -> 
            dir_remove name pdir |> fun pdir ->
            dirs_ops.map_add pid pdir dirs |> fun dirs ->
            `Ok,{s with dirs}) >>= function 
    | `Ok -> return ()
    | `Internal s -> extra.internal_err s
    | `Error_no_entry -> err @@ `Error_no_entry "unlink, mim.272"
  in

  let _ = unlink in 


  (* FIXME check is already exists etc *)
  let mkdir ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun pid ->
    extra.new_did () >>= fun (did:did) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        (* add new empty dir to dirs *)
        dirs_ops.map_add did (empty_dir ~parent:pid) dirs |> fun dirs ->
        (* add name to parent *)
        dirs_ops.map_find pid s.dirs |> fun pdir ->
        match pdir with 
        | None -> `Internal "impossible, parent not found mim.l259",s
        | Some pdir -> 
          dir_add name (Did did) pdir |> fun pdir ->
          (* update parent in dirs *)
          dirs_ops.map_add pid pdir dirs |> fun dirs ->
          `Ok,{s with dirs}) >>= function
    | `Ok -> return ()
    | `Internal s -> extra.internal_err s
  in


  (* let mk_dh ~did es = (did,es) in *)


  let opendir path = 
    resolve_dir_path path >>= fun did ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_find did dirs |> function
        | None -> `Internal "opendir, impossible, dir not found mim.278",s
        | Some dir -> 
          dir_bindings dir |> List.map fst |> fun names ->
          1+s.max_dh |> fun dh ->
          s.dir_handles |> fun handles ->
          dhandles_ops.map_add dh names handles |> fun dir_handles ->
          {s with dir_handles; max_dh=dh } |> fun s ->                            
          `Ok(dh),s) >>= function
    | `Ok dh -> return dh
    | `Internal s -> extra.internal_err s
  in


  let readdir dh = 
    extra.with_fs (fun s ->
        s.dir_handles |> fun handles ->
        dhandles_ops.map_find dh handles |> function
        | None -> `Internal "readdir, impossible, dh not found mim.328",s
        | Some xs -> `Ok xs,s) >>= function
    | `Ok xs -> return (xs,finished)
    | `Internal s -> extra.internal_err s
  in


  let closedir dh = return () in (* FIXME should we record which rd are valid? ie not closed *)


  let create ~parent ~name : (unit,'m) m_ = 
    resolve_dir_path parent >>= fun parent ->
    extra.new_fid () >>= fun (fid:fid) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        dirs_ops.map_find parent dirs |> function
        | None -> `Internal "create, impossible, parent did not found mim.299",s
        | Some pdir ->
          dir_add name (Fid fid) pdir |> fun pdir ->
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
          (* don't try to read more bytes than available *)
          let max_length = String.length contents - foff in
          let length = min max_length length in
          let error_case = 
            foff+length > String.length contents ||  (* should be impossible *)
            boff+length > Bigarray.Array1.dim buffer  (* FIXME fuse prevents this? *)
          in
          match error_case with
          | true -> `Internal "pread, invalid blit arguments, mim.342",s
          | false -> 
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
          (* convert contents to bytes, and extend if necessary *)
          let contents = 
            if foff+length > String.length contents 
            then 
              Bytes.create (foff+length) |> fun contents' ->
              blit_string_to_bytes
                ~src:contents ~soff:0 ~len:(String.length contents) 
                ~dst:contents' ~doff:0;
              contents'
            else Bytes.of_string contents 
          in
          let error_case = 
            foff+length > Bytes.length contents ||   (* shouldn't happen *)
            boff+length > Bigarray.Array1.dim buffer  (* presumably fuse prevents this *)
          in
          match error_case with
          | true -> `Internal "pwrite, invalid blit arguments, mim.357",s
          | false -> 
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


  (* FIXME ddir and sdir may be the same, so we need to be careful to
     always use indirection via did *)
  let rename ~spath ~sname ~dpath ~dname = 
    resolve_dir_path spath >>= fun sdir_did ->
    resolve_dir_path dpath >>= fun ddir_did ->
    resolve_did sdir_did >>= fun sdir ->
    resolve_name ~dir:sdir ~name:sname |> function
    | None -> err `Error_no_src_entry
    | Some resolved_sname -> 
      resolve_did ddir_did >>= fun ddir ->
      resolve_name ~dir:ddir ~name:dname |> fun resolved_dname ->
      let insert_and_remove id =
        match sdir_did = ddir_did with
        | true -> 
          dir_add dname id ddir |> fun ddir ->
          dir_remove sname ddir |> fun ddir ->
          extra.dirs_add ddir_did ddir >>= fun () ->
          return ()
        | false -> 
          dir_add dname id ddir |> fun ddir ->
          dir_remove sname sdir |> fun sdir ->
          extra.dirs_add sdir_did sdir >>= fun () ->
          extra.dirs_add ddir_did ddir >>= fun () ->
          return ()
      in
      match resolved_sname,resolved_dname with
      | Fid fid,None -> insert_and_remove (Fid fid)
      | Fid fid1,Some(Fid fid2) -> 
        if fid1=fid2 
        then return ()
        else insert_and_remove (Fid fid1)
      | Fid fid,Some(Did did) ->
        extra.internal_err "FIXME rename f to d, d should be empty?"
      | Did sdid,None -> 
        (* check not root *)
        if ddir_did=root_did 
        then err `Error_attempt_to_rename_root
        (* check not subdir *)
        else extra.is_parent ~parent:sdid ~child:ddir >>= (function
            | true -> err `Error_attempt_to_rename_to_subdir
            | false -> 
              (* FIXME other checks *)
              resolve_did sdid >>= fun sdir -> 
              (* new directory id *)
              extra.new_did() >>= fun did ->
              (* record new directory with updated parent *)
              extra.dirs_add did { sdir with parent=ddir_did } >>= fun () ->
              insert_and_remove (Did did))
      | Did sdid,Some(Fid fid) -> 
        err `Error_attempt_to_rename_dir_over_file  (* FIXME correct ? *)
      | Did sdid,Some(Did ddid) ->
        if sdid=ddid 
        then return ()
        else extra.internal_err "FIXME rename d to d, dst should be empty?"
  in

  let truncate ~path ~length = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "file not found mim.362",s
        | Some contents ->
          let contents' = Bytes.create length in
          Bytes.blit_string contents 0 contents' 0 (min (Bytes.length contents) length);
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
        | None -> `Internal "file fid not found mim.379",s
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

  mk_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset

let _ = mk_ops  (* NOTE the error cases are captured in the type *)


(* instantiate monad ------------------------------------------------ *)

open Mini_error

open Step_monad

type t = { 
  thread_error_state: exn_ option;  (* for current call *)
  internal_error_state: string option;
  fs: fs_t
}

let init_t = {
  thread_error_state=None;
  internal_error_state=None;
  fs=init_fs
}

module Y_ = struct
  open X_
  type t' = {
    thread_error_state: exn_ option;  (* for current call *)
    internal_error_state: string option;
    fs:fs' 
  } [@@deriving yojson]

  let from_t (t:t) = {
    thread_error_state=t.thread_error_state;
    internal_error_state=t.internal_error_state;
    fs=X_.from_fs t.fs
  }
end

let t_to_string t = Y_.(
    t |> from_t |> t'_to_yojson |> Yojson.Safe.pretty_to_string)

let with_fs (f:fs_t -> 'a * fs_t) : ('a,'m)m_ = 
  with_state (fun w -> f w.fs |> fun (x,fs') -> x,{w with fs=fs'}) (fun a -> return a)

let _ = with_fs

let new_did () = with_fs (fun fs ->
    { fs with max_did=(inc_did fs.max_did) } |> fun fs' ->
    let did = fs'.max_did in
    did,fs')

let new_fid () = with_fs (fun fs ->
    { fs with max_fid=(inc_fid fs.max_fid) } |> fun fs' ->
    let fid = fs'.max_fid in
    fid,fs')

let _ = new_fid

let internal_err s : ('a,'m)m_ = 
  Step(fun w -> 
      ({ w with internal_error_state=(Some s)}, fun () -> 
          "Fatal error: attempt to step internal errror state mim.449\n" |> fun s ->
          print_endline s;
          failwith s))

let _ = internal_err


let dirs_find k =
  with_fs (fun s ->
      s.dirs |> fun dirs ->
      dirs_ops.map_find k dirs |> fun v ->
      v,s)

  (* FIXME we could maintain a list of parents when resolving, of course *)
let is_parent ~parent ~child = 
  let open Step_monad in
  let rec is_p (child:dir_with_parent) = 
    if child.parent = parent then return true
    else if child.parent = root_did then return false
    else is_p' child.parent
  and is_p' (child:did) =
    dirs_find child >>= function
    | None ->  internal_err "did not valid, mim.418"
    | Some child -> is_p child
  in
  is_p child

let dirs_add k v =
  let open Step_monad in
  with_fs (fun s ->
      s.dirs |> fun dirs ->
      dirs_ops.map_add k v dirs |> fun dirs ->
      (),{s with dirs})

let extra = { new_did; new_fid; with_fs; internal_err; is_parent; dirs_add }

let err e : ('a,'m)m_ = 
  Step(fun w -> 
      ({ w with thread_error_state=(Some e)}, fun () -> 
          "Fatal error: attempt to step thread errror state mim.460\n" |> fun s ->
          print_endline s;
          failwith s))

let monad_ops = Step_monad.{bind; return; err}

let ops = mk_ops ~monad_ops ~extra  (* NOTE the error cases are captured in the type *)

let _ = ops

(* running ---------------------------------------------------------- *)

let is_exceptional w = w.thread_error_state <> None || w.internal_error_state <> None

let log_state = ref false

let rec run w (x:('a,'m)m_) = 
  match is_exceptional w with
  | true -> `Exceptional w
  | false -> 
    match x with
    | Finished a -> `Finished(a,w)
    | Step f -> 
      f w |> fun (w',rest) ->
      (if !log_state then Printf.printf "run mim.511: result state: %s\n" (t_to_string w'));
      if is_exceptional w' 
      then `Exceptional w' 
      else run w' (rest())


(* imperative ------------------------------------------------------- *)


let imp_run ref_ : t run = {
  run=(fun x -> run (!ref_) x |> function
    | `Exceptional w -> 
      "Run resulted in exceptional state" |> fun s ->
      print_endline s;
      failwith s
    | `Finished(a,w) -> 
      ref_:=w;
      a)
}
  

let mk_imperative_ops ~ref_ =
  Minifs.mk_imperative_ops ~run:(imp_run ref_) ~ops 


(* logging ---------------------------------------------------------- *)


(* FIXME should also log exceptional returns *)
let log msg (x:('a,'m) m_) = 
  let call = msg |> Msgs.msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string in
  let rec log_return = function
    | Finished a -> 
      Printf.printf "call %s returns\n" call;
      Finished a
    | Step(f) -> Step(
        fun w -> f w |> fun (w',rest) -> (w',fun () -> log_return (rest())))
  in
  let log_call_and_return = Step(
      fun w -> 
        Printf.printf "call %s starts\n" call;
        w,fun () -> log_return x)
  in
  log_call_and_return

let log_op = Mini_log.{ log }
