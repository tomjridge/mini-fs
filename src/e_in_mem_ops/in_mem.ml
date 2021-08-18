(** Construct an in-memory version of the filesystem ops; don't open *)

(* FIXME more documentation *)

open Tjr_map
open Minifs_intf


module Int_base_types = Int_base_types
open Int_base_types


(** {2 Misc} *)

(** NOTE hidden defn of resolve *)
(**/**)

(* NOTE we specialize this later *)
let resolve = Tjr_path_resolution.resolve
(**/**)

module Map_int = Stdlib.Map.Make(
  struct 
    type t = int let compare: t->t->int = Stdlib.compare 
  end)


module Set_string = Stdlib.Set.Make(
  struct
    type t = string let compare: t->t->int = Stdlib.compare 
  end)


module Map_string = Stdlib.Map.Make(
  struct 
    type t = string let compare: t->t->int = Stdlib.compare 
  end)


let time = Unix.time (* 1s resolution *)

let mk_times () = 
  time () |> fun t ->
  ({ atim=t;mtim=t} : times)

let fdata_ops = Tjr_buffer.mk_buf_ops()

type path = string



(** {2 File and directory ids} *)

(* file ids *)
module Fid : sig
  type fid = int[@@deriving yojson]  (* FIXME hide *)
  val fid0 : fid
  val inc_fid : fid -> fid
end = struct
  type fid=int[@@deriving yojson]
  let fid0=0
  let  inc_fid x = x+1
end
open Fid
type fid = Fid.fid


(* dir ids *)
module Did : sig
  type did = int[@@deriving yojson] (* FIXME hide *)
  val root_did : did
  val inc_did : did -> did
end = struct
  type did = int[@@deriving yojson]
  let root_did = 1234
  let inc_did x = x+1 
end
open Did
type did = Did.did



(** {2 Other basic types: dir_entry, file_times_and_data } *)

module Symlink_type = struct
  type sid = unit
  type symlink = string [@@deriving yojson]
end
open Symlink_type

(* type id = Fid of fid | Did of did [@@deriving yojson] *)
(* entries are either files, dirs, or symlinks; for symlinks we just
   record the string data in the entry *)
module Dir_entry = struct
  type dir_entry = Fid of fid | Did of did | Symlink of symlink[@@deriving yojson]
  let is_fid = function
    | Fid _ -> true
    | _ -> false


  let is_did = function
    | Did _ -> true 
    | _ -> false

  let is_symlink _x = function
    | Symlink _ -> true
    | _ -> false
end
open Dir_entry
type dir_entry = Dir_entry.dir_entry = Fid of fid | Did of did | Symlink of symlink





type file_times_and_data = {
  times: Times.times; 
  data: Tjr_buffer.buf
}


(** {2 Dirs as maps from string} *)

(* a name map - from strings to dir entries *)
(* type name_map_carrier = dir_entry Map_string.t *)
type dents_carrier = dir_entry Map_string.t

type dents_ops = (string,dir_entry,dents_carrier) map_ops
(* let dents_ops : dents_ops = Map_string.map_ops *)
module Dents_ops = Map_string

type dir_with_parent = {
  name_map :dents_carrier;
  times    :Times.times;
  parent   :did
}

let (dir_empty,dir_find,dir_add,dir_remove,dir_bindings) = 
  let dir_empty ~times ~parent = { name_map=Dents_ops.empty; times; parent } in
  let dir_find k t = Dents_ops.find_opt k t.name_map in
  let dir_add k v t = {t with name_map=Dents_ops.add k v t.name_map} in
  let dir_remove k t = {t with name_map=Dents_ops.remove k t.name_map} in
  let dir_bindings t = Dents_ops.bindings t.name_map in
  (dir_empty,dir_find,dir_add,dir_remove,dir_bindings)  

let empty_dir ~parent ~times = dir_empty ~parent ~times


module Map_did = Tjr_map.Make_map_ops(
  struct 
    type k = did
    type v = dir_with_parent
    let k_cmp: k->k->int = Stdlib.compare 
  end)

type dirs_carrier = Map_did.t
type dirs_ops = (did,dir_with_parent,dirs_carrier) map_ops
let dirs_ops : dirs_ops = Map_did.map_ops


(** {2 Global files map from fid} *)

module Map_fid = Tjr_map.Make_map_ops(
  struct 
    type k = fid 
    type v = file_times_and_data
    let k_cmp: k->k->int = Stdlib.compare 
  end)

type files_carrier = Map_fid.t
type files_ops = (fid,file_times_and_data,files_carrier) map_ops
let files_ops : files_ops = Map_fid.map_ops


(** {2 Dir handles} *)

(** This is needed because we access dir handles in stages: openddir,
   readdir, closedir; so we have to maintain information about open
   dhs *)

type dhandles_carrier = string list Map_int.t
type dhandles_ops = (dh,string list,dhandles_carrier) map_ops
(* let dhandles_ops : dhandles_ops = Map_int.map_ops *)
module Dhandles_ops = Map_int


(** {2 Filesystem state} *)

(** NOTE symlinks are stored directly as a dir entry *)

type fsys = {
  files: files_carrier;
  max_fid: fid;
  dirs: dirs_carrier;
  max_did: did;
  dir_handles: dhandles_carrier;
  max_dh: dh;
}

(* list some of the map functions to work with fs *)
let lookup_did did fs = 
  fs.dirs |> fun map ->
  Map_did.map_ops.find_opt did map

module Fsys_to_json = struct
  (* an easily-jsonable version *)
  type fs' = {
    files: (fid * string) list;
    max_fid: fid;
    dirs: (
      did * (* id of dir *)
      (
        (string*dir_entry) list * (* entries *)
        did  (* parent *)
      ) 
    ) list;
    max_did: did;
    (* FIXME dhandles *)
  } [@@deriving yojson]

  (* FIXME meta not included *)
  let from_fs (fs:fsys) = {
    files=files_ops.bindings fs.files |> List.map (fun (fid,c) -> 
        (fid,fdata_ops.to_string c.data));
    max_fid=fs.max_fid;
    dirs=
      dirs_ops.bindings fs.dirs 
      |> List.map (fun (did,dir) -> 
          dir.name_map |> Dents_ops.bindings |> fun bs ->
          (did,(bs,dir.parent)));
    max_did=fs.max_did;
  }

  let fsys_to_json fs = (
      from_fs fs |> fs'_to_yojson |> Yojson.Safe.pretty_to_string)

end

let init_fsys = {
  files=files_ops.empty;
  max_fid=fid0;
  dirs=(dirs_ops.empty 
        |> dirs_ops.add root_did (empty_dir ~times:(mk_times()) ~parent:root_did));
  max_did=root_did;
  dir_handles=Dhandles_ops.empty;
  max_dh=0;
}


(** {2 Extra monadic operations} *)

(** These operations are needed to build the ops; in turn, they can be created via the [with_fs] type. FIXME perhaps generalize over fsys type *)

type 't extra_ops = {
  (* err: 'a. 'e -> (('a,'e)r_,'t) m;  (* FIXME? how can we handle all errors? *) *)
  new_did: unit -> (did,'t) m;
  new_fid: unit -> (fid,'t) m;
  with_fs: 'a. (fsys -> 'a * fsys) -> ('a,'t) m;  (* ASSUME-ed not to raise exception *)
  internal_err: 'a. string -> ('a,'t) m;
  dirs_add: did -> dir_with_parent -> (unit,'t) m;
  is_ancestor: parent:did -> child:dir_with_parent -> (bool,'t) m
}


(** {2 Main functionality} *)

open Tjr_path_resolution
open Tjr_path_resolution.Intf
(* we reuse the path_resolution library FIXME *)

let mk_ops ~monad_ops ~(extra_ops: 't extra_ops) = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let err e = return (Error e) in
  let ( >>=| ) a b = a >>= function
    | Ok a -> b a
    | Error e -> return (Error e)
  in

  let resolve_did did = 
    extra_ops.with_fs (fun s ->
      (dirs_ops.find_opt did s.dirs,s)) >>= fun r -> 
    match r with 
    | None -> extra_ops.internal_err "resolve_did, did not valid mim.l154"
    | Some dir -> return dir
  in
  let _ : did -> (dir_with_parent,'t) m = resolve_did in


  (* NOTE needs a dir_with_parent to resolve .. *)
  let resolve_name ~(dir_with_parent:dir_with_parent) ~name : dir_entry option = 
    dir_find name dir_with_parent 
  in

  let dir_entry_option_to_resolve_result eopt : (fid,did,sid) resolved_comp = 
    match eopt with
    | None -> RC_missing
    | Some x -> x |> function
      | Fid fid -> RC_file fid
      | Did did -> RC_dir did
      | Symlink s -> RC_sym ((),s)
  in

  (* path resolution *)
  let resolve = 
    let root = root_did in
    (* paths are always absolute when coming from fuse, and via the
       api... FIXME assert this? *)
    let resolve_comp (did:did) (comp:string) : ((fid,did,sid)resolved_comp,'t) m = 
      (* we want to use resolve_name, but this works with dir_with_parent *)
      extra_ops.with_fs (fun fs -> 
          Map_did.map_ops.find_opt did fs.dirs |> function
          | None -> (
            log_now __LOC__;
            exit_1 "impossible, invalid did")
          | Some dwp -> 
            resolve_name ~dir_with_parent:dwp ~name:comp 
            |> dir_entry_option_to_resolve_result |> fun x -> (x,fs))
    in
    let _ = resolve_comp in
    let fs_ops = { root; resolve_comp } in
    Tjr_path_resolution.resolve ~monad_ops ~fs_ops
  in


  let resolve_path ~follow_last_symlink path = 
    (* paths from Fuse should satisfy this; FIXME what if we are not using fuse? *)
    assert(String_util.starts_with_slash path); 
    let cwd = root_did in
    resolve ~follow_last_symlink ~cwd path >>= function
    | Ok r -> return (Ok r)
    | Error _e -> err `Error_path_resolution  (* FIXME be more careful here? *)
  in


  (* FIXME not sure about Always in following two funs *)
  let resolve_dir_path (path:path) : ((did,'e3)result,'t) m = 
    resolve_path ~follow_last_symlink:`Always path >>= function 
    | Ok { result=(Dir did);_ } -> return (Ok did)
    | _ -> err `Error_not_directory
  in


  let resolve_file_path (path:path) : ((fid,'e4)result,'t) m = 
    resolve_path ~follow_last_symlink:`Always path >>= function
    | Ok { result=(File fid); _ } -> return (Ok fid)
    | _ -> err `Error_not_file
  in


  let root : path = "/" in


  (* FIXME or just allow unlink with no expectation of the kind? FIXME
     add a "follow" flag? optional? FIXME how does parent/name interact
     with symlinks? FIXME perhaps keep parent/name, but allow another
     layer ot deal with follow; FIXME not sure about `If_trailing_slash  *)
  let unlink path = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let { parent_id=pid; comp=name; result=_; trailing_slash=_ } = rpath in
    begin
      extra_ops.with_fs (fun s ->
          s.dirs |> fun dirs ->
          dirs_ops.find_opt pid dirs |> function
          | None -> `Internal "impossible, parent not found, mim.233",s
          | Some pdir -> 
            dir_find name pdir |> function
            | None -> `Error_no_entry,s
            | Some _entry -> 
              (* FIXME with hardlinks, tims can change for other link *)
              dir_remove name pdir |> fun pdir ->
              dirs_ops.add pid pdir dirs |> fun dirs ->
              `Ok,{s with dirs}) 
      >>= (function 
          | `Ok -> return (Ok ())
          | `Internal s -> extra_ops.internal_err s
          | `Error_no_entry -> err @@ `Error_no_entry)
    end
  in

  let _ = unlink in 


  (* FIXME meta changes for parent and child *)
  let mkdir path : ((unit,'e5)result,'t) m = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let { parent_id=pid; comp=name; result; trailing_slash=_ } = rpath in
    match result with 
    | Missing -> (
        begin
          let times = mk_times () in
          extra_ops.new_did () >>= fun (did:did) -> 
          extra_ops.with_fs (fun s -> 
              s.dirs |> fun dirs ->
              (* add new empty dir to dirs *)
              dirs_ops.add did (empty_dir ~times ~parent:pid) dirs |> fun dirs ->
              (* add name to parent *)
              dirs_ops.find_opt pid s.dirs |> fun pdir ->
              match pdir with 
              | None -> `Internal "impossible, parent not found mim.l259",s
              | Some pdir -> 
                dir_add name (Did did) pdir |> fun pdir ->
                (* update parent meta *)
                {pdir with times} |> fun pdir ->
                (* update parent in dirs *)
                dirs_ops.add pid pdir dirs |> fun dirs ->
                `Ok,{s with dirs})
        end
        >>= function
        | `Ok -> return (Ok ())
        | `Internal s -> extra_ops.internal_err s)
    | _ -> err @@ `Error_exists
  in

  let _ = mkdir in

  (* let mk_dh ~did es = (did,es) in *)


  (* FIXME update atim *)
  let opendir path = 
    resolve_dir_path path >>=| fun did ->
    begin
      extra_ops.with_fs (fun s ->
          s.dirs |> fun dirs ->
          dirs_ops.find_opt did dirs |> function
          | None -> `Internal "opendir, impossible, dir not found mim.278",s
          | Some dir -> 
            dir_bindings dir |> List.map fst |> fun names ->
            1+s.max_dh |> fun dh ->
            s.dir_handles |> fun handles ->
            Dhandles_ops.add dh names handles |> fun dir_handles ->
            {s with dir_handles; max_dh=dh } |> fun s ->                            
            `Ok(dh),s)
    end
    >>= function
    | `Ok dh -> return (Ok dh)
    | `Internal s -> extra_ops.internal_err s
  in


  (* FIXME atim *)
  let readdir dh = 
    extra_ops.with_fs (fun s ->
        s.dir_handles |> fun handles ->
        Dhandles_ops.find_opt dh handles |> function
        | None -> `Internal "readdir, impossible, dh not found mim.328",s
        | Some xs -> `Ok xs,s) 
    >>= function
    | `Ok xs -> return (Ok(xs,{finished=true}))
    | `Internal s -> extra_ops.internal_err s
  in


  let closedir _dh = return (Ok()) in 
  (* FIXME should we record which rd are valid? ie not closed *)


  (* default size preallocated for a file *)
  let internal_len = 1024 in 


  let create path : ((unit,'e6)result,'t) m = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let { parent_id=parent; comp=name; result=_; trailing_slash=_ } = rpath in
    extra_ops.new_fid () >>= fun (fid:fid) -> 
    let times = mk_times() in
    extra_ops.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        dirs_ops.find_opt parent dirs |> function
        | None -> `Internal "create, impossible, parent did not found mim.299",s
        | Some pdir ->
          dir_add name (Fid fid) pdir |> fun pdir ->
          (* update pdir meta *)
          {pdir with times} |> fun pdir ->
          dirs_ops.add parent pdir dirs |> fun dirs ->
          {s with dirs} |> fun s ->
          s.files |> fun files ->
          let data = fdata_ops.create ~internal_len 0 in
          files_ops.add fid {times;data} files |> fun files ->
          `Ok,{s with files}) 
    >>= function
    | `Internal s -> extra_ops.internal_err s
    | `Ok -> return (Ok())
  in


  let mk_fd (fid:fid) = fid (* ExtUnix.All.file_descr_of_int fid *) in

  let fd2int fd = fd (* ExtUnix.All.int_of_file_descr fd *) in

  (* FIXME atim *)
  let open_ path = 
    resolve_file_path path >>=| fun fid -> 
    fid |> mk_fd |> fun fd -> return (Ok fd)
  in

  (* FIXME must account for reading beyond end of file; FIXME atim *)
  let pread ~fd ~foff ~len ~(buf:ba_buf) ~boff = 
    (* buf_size_check length; *)
    let fid = fd2int fd in
    extra_ops.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.find_opt fid files |> function
        | None -> `Internal "pread, impossible, no file, mim.325",s
        | Some(f) ->
          let clen = fdata_ops.len f.data in
          (* NOTE we have some flexibility to choose length *)
          assert(len >= 0);
          assert(foff >= 0);
          assert(boff >= 0);
          assert(boff+len <= Tjr_buffer.Biga.length buf);
          (* following assures that foff+length<=clen *)
          let length = if foff > clen then 0 else min len (clen - foff) in
          match () with
          (* NOTE foff may be > clen, but then length is 0 *)
          | _ when length=0 -> (`Ok 0,s)
          | _ -> 
            fdata_ops.blit_buf_to_bigarray 
              ~src:f.data ~soff:foff ~len:length 
              ~dst:buf ~doff:boff;
            (`Ok length,s)) 
    >>= function
    | `Internal s -> extra_ops.internal_err s
    | `Ok l -> return (Ok l)
  in


  let pwrite ~fd ~foff ~len ~(buf:ba_buf) ~boff = 
    (* buf_size_check length; *)
    extra_ops.with_fs (fun s ->
        let fid = fd2int fd in
        let times = mk_times() in
        s.files |> fun files ->
        files_ops.find_opt fid files |> function
        | None -> `Internal "pwrite, impossible, no file, mim.339",s
        | Some f ->
          let blen = Tjr_buffer.Biga.length buf in
          assert(len >= 0);
          assert(foff >= 0);
          assert(boff >= 0);
          assert(boff+len <= blen);
          (* we allow foff beyond EOF *)
          let contents = 
            if foff+len > fdata_ops.len f.data 
            then fdata_ops.resize (foff+len) f.data
            else f.data
          in
          assert(foff+len <= fdata_ops.len contents);
          fdata_ops.blit_bigarray_to_buf
            ~src:buf ~soff:boff ~len ~dst:contents ~doff:foff 
          |> fun data ->
          (files_ops.add fid {f with data;times} files)[@ ocaml.warning "-23"] |> fun files ->
          (`Ok len,{s with files}))
    >>= function
    | `Internal s -> extra_ops.internal_err s
    | `Ok l -> return (Ok l)  
  in


  let close _fd = return (Ok ()) in (* FIXME record which are open? *)


  (* FIXME ddir and sdir may be the same, so we need to be careful to
     always use indirection via did *)
  let rename spath dpath = 
    let follow_last_symlink = `Always in
    begin
      resolve_path ~follow_last_symlink spath >>= function Error e -> err e | Ok spath ->
        resolve_path ~follow_last_symlink dpath >>= function Error e -> err e | Ok dpath ->          
          resolve_did spath.parent_id >>= fun sdir ->
          match spath.result with 
          | Missing -> err `Error_no_src_entry
          | _ -> 
            let scomp = spath.comp in
            resolve_did dpath.parent_id >>= fun ddir ->
            let dcomp = dpath.comp in
            let times = mk_times() in
            let insert_and_remove id =
              match spath.parent_id = dpath.parent_id with
              | true -> 
                dir_add dcomp id ddir |> fun ddir ->
                dir_remove scomp ddir |> fun ddir ->
                {ddir with times} |> fun ddir ->
                extra_ops.dirs_add dpath.parent_id ddir >>= fun () ->
                return (Ok ())
              | false -> 
                dir_add dcomp id ddir |> fun ddir ->
                {ddir with times} |> fun ddir ->
                dir_remove scomp sdir |> fun sdir ->
                {sdir with times} |> fun sdir ->
                extra_ops.dirs_add spath.parent_id sdir >>= fun () ->
                extra_ops.dirs_add dpath.parent_id ddir >>= fun () ->
                return (Ok ())
            in
            match spath.result,dpath.result with
            (* FIMXE following for symlinks *)
            | Sym _,_ | _,Sym _ -> (
              log_now __LOC__;
              exit_1 "FIXME shouldn't happen - follow=`Always")
            | File fid,Missing -> insert_and_remove (Fid fid)
            | File fid1,File fid2 -> 
              if fid1=fid2 
              then return (Ok ())
              else insert_and_remove (Fid fid1)
            | File _fid,Dir _did ->
              extra_ops.internal_err "FIXME rename f to d, d should be empty?"
            | Dir sdid,Missing -> 
              (* check not root *)
              if sdid=root_did 
              then err `Error_attempt_to_rename_root
              (* check not subdir *)
              else extra_ops.is_ancestor ~parent:sdid ~child:ddir >>= (function
                  | true -> err `Error_attempt_to_rename_to_subdir
                  | false -> 
                    (* FIXME other checks *)
                    resolve_did sdid >>= fun sdir -> 
                    {sdir with times} |> fun sdir ->
                    (* new directory id *)
                    extra_ops.new_did() >>= fun did ->
                    (* record new directory with updated parent *)
                    extra_ops.dirs_add did { sdir with parent=dpath.parent_id } >>= fun () ->
                    insert_and_remove (Did did))
            | Dir _sdid,File _fid -> 
              err `Error_attempt_to_rename_dir_over_file  (* FIXME correct ? *)
            | Dir sdid,Dir ddid ->
              if sdid=ddid 
              then return (Ok ())
              else extra_ops.internal_err "FIXME rename d to d, dst should be empty?"
            | Missing ,_ -> (
                log_now __LOC__;
                exit_1 "impossible")
    end
  in


  (* FIXME truncate parent name; FIXME also stat *)
  let truncate path length = 
    resolve_file_path path >>=| fun fid ->
    extra_ops.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.find_opt fid files |> function
        | None -> `Internal "file not found mim.362",s
        | Some f ->
          fdata_ops.resize length f.data  |> fun data ->
          let times = mk_times() in
          files_ops.add fid ({f with data;times}[@ocaml.warning "-23"]) files |> fun files ->
          `Ok (),{s with files}) 
    >>= function
    | `Ok () -> return (Ok ())
    | `Internal s -> extra_ops.internal_err s
  in


  let dummy_symlink_times = ({ atim=0.0; mtim=0.0 } : times) in

  (* FIXME here and elsewhere atim is not really dealt with *)
  let stat path = 
    let open Stat_record in
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let { parent_id=_pid; comp=_name; result; trailing_slash=_ } = rpath in
    begin
      match result with
      | Missing -> return `Error_no_entry
      | File fid ->
        extra_ops.with_fs (fun s ->
            s.files |> fun files ->
            files_ops.find_opt fid files |> function
            | None -> `Internal "file fid not found mim.379",s
            | Some f ->          
              fdata_ops.len f.data |> fun sz ->
              `Ok { sz;times=f.times;kind=`File },s)
      | Dir did -> 
        extra_ops.with_fs (fun s ->
            s.dirs |> fun dirs ->
            dirs_ops.find_opt did dirs |> function
            | None -> `Internal "dir did not found mim.651",s
            | Some (f:dir_with_parent) ->          
              let sz = 1 in (* for dir? FIXME size of dir *)
              f.times |> fun times ->
              `Ok { sz;times;kind=`Dir },s)
      | Sym ((),s) -> 
        let sz = String.length s in
        let times = dummy_symlink_times in
        let kind = `Symlink in
        return @@ `Ok { sz;times;kind }
    end
    >>= function
    | `Ok stat -> return (Ok stat)
    | `Error_no_entry -> return (Error `Error_no_entry)
    | `Internal s -> extra_ops.internal_err s
  in


  let symlink contents path =
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let { parent_id=pid; comp=name; result; trailing_slash=_ } = rpath in
    begin
      let times = mk_times() in
      extra_ops.with_fs (fun s ->
          match result with
          | Missing -> (
              s.dirs |> fun dirs ->
              dirs_ops.find_opt pid dirs |> function
              | None -> `Internal "impossible",s
              | Some pdir -> 
                dir_add name (Symlink contents) pdir |> fun pdir ->
                (* update pdir times *)
                {pdir with times} |> fun pdir ->
                dirs_ops.add pid pdir dirs |> fun dirs ->
                {s with dirs} |> fun s ->
                `Ok,s)
          | _ -> `Error_exists,s)
    end
    >>= function
    | `Ok -> return (Ok ())
    | `Error_exists -> return (Error `Error_exists)
    | `Internal s -> extra_ops.internal_err s
  in


  let readlink path = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let { parent_id=_pid; comp=_name; result; trailing_slash=_ } = rpath in
    begin
      extra_ops.with_fs (fun s ->
          match result with
          | Sym ((),str) -> (`Ok str,s)
          | _ -> `Error_not_symlink,s)
    end
    >>= function
    | `Ok s -> return (Ok s)
    | `Error_not_symlink -> return (Error `Error_not_symlink)
    | `Internal s -> extra_ops.internal_err s
  in

  let reset () = return () in

  let ops : (_,_,_)ops = { 
    root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; symlink; readlink; reset }
  in
  ops

let _ = mk_ops



(** {2 State-passing instance FIXME tidy} *)

(** Like fsys, but track any errors that occur, and maybe raise them from the monad *)
type fsystem = { 
(*  thread_error_state: exn_ option;  (* for current call *) *)
  internal_error_state: string option;
  fsys: fsys
}


let init_fsystem = {
(*  thread_error_state=None; *)
  internal_error_state=None;
  fsys=init_fsys
}

(*
module In_mem_monad = struct
  open Tjr_monad
  type 'a m = ( 'a, t) Tjr_step_monad.m
  let bind,return = bind,return
  let run w a = 
    let halt w = w.internal_error_state <> None in
    Tjr_step_monad.Extra_Ops.run_with_halt ~halt w a
    
end
include In_mem_monad
*)

(* easily json-able FIXME used? *)

(** NOTE hidden [Y_] module *)

(**/**)
module Y_ = struct
  open Fsys_to_json
  type t' = {
    internal_error_state: string option;
    fs:fs' 
  } [@@deriving yojson]

  let from_t (t:fsystem) = {
    internal_error_state=t.internal_error_state;
    fs=Fsys_to_json.from_fs t.fsys
  }
end
(**/**)

let fsystem_to_string t = Y_.(
    t |> from_t |> t'_to_yojson |> Yojson.Safe.pretty_to_string)



(** State-passing helper functions type; used to construct [extra_ops] *)
type 't with_fs = {
  with_fs: 'a. (fsys -> 'a * fsys) -> ('a,'t) m;
  internal_err: 'a. string -> ('a,'t) m
}

let mk_extra_ops ~monad_ops ~(with_fs:'t with_fs) = 

  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  
  (* following was Extra_core.{with_fs,internal_err} = extra_core, but there was a ppx bug https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/18 *)
  let with_fs,internal_err = (with_fs.with_fs,with_fs.internal_err) in

  let new_did () = with_fs (fun fs ->
      { fs with max_did=(inc_did fs.max_did) } |> fun fs' ->
      let did = fs'.max_did in
      did,fs')
  in

  let new_fid () = with_fs (fun fs ->
      { fs with max_fid=(inc_fid fs.max_fid) } |> fun fs' ->
      let fid = fs'.max_fid in
      fid,fs')
  in

  let _ = new_fid in


  let dirs_find k =
    with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.find_opt k dirs |> fun v ->
        v,s)
  in

  (* FIXME we could maintain a list of parents when resolving, of course *)
  let is_ancestor ~parent ~child = 
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
  in

  let dirs_add k v =
    with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.add k v dirs |> fun dirs ->
        (),{s with dirs})
  in

  let extra = { new_did; new_fid; with_fs; internal_err; is_ancestor; dirs_add } in
  extra


let mk_ops ~monad_ops ~with_fs : (fid,did,'t)ops = 
  let extra_ops = mk_extra_ops ~monad_ops ~with_fs in
  mk_ops ~monad_ops ~extra_ops


(* state-passing instance ------------------------------------------- *)

let in_mem_monad_ops = State_passing.monad_ops ()

let in_mem_state_passing_ops : (fid,did,fsystem state_passing) ops = 
  (* FIXME internal_err should just mark the state as erroneous
     without having to produce an 'a; maybe state is either erroneous
     or ok *)
  let with_fs = {
    with_fs=(fun f -> 
      State_passing.of_fun
          (fun t -> 
             f t.fsys |> fun (a,fsys) ->
             a,{t with fsys}));
    internal_err=(fun s -> 
        State_passing.of_fun
          (fun _t ->
             log_now __LOC__;
             exit_1 (s^"; "^__LOC__)))
  }
  in
  let ops = mk_ops ~monad_ops:in_mem_monad_ops ~with_fs in
  ops

let _ = in_mem_state_passing_ops
