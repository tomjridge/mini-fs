open Tjr_monad
open Tjr_monad.Monad
open Tjr_either
open Tjr_map
open Base_
open R_
open Ops_type_

(* NOTE we specialize this later *)
let resolve = Tjr_path_resolution.resolve



(* in-mem impl ------------------------------------------------------ *)

let time = Unix.time (* 1s resolution *)

module Mem_base_types = Int_base_types
include Mem_base_types


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
include Fid


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


type symlink = string [@@deriving yojson]

(* type id = Fid of fid | Did of did [@@deriving yojson] *)
(* entries are either files, dirs, or symlinks; for symlinks we just
   record the string data in the entry *)
type dir_entry = Fid of fid | Did of did | Symlink of symlink[@@deriving yojson]

(* a name map - from strings to dir entries *)
type name_map_carrier = dir_entry Map_string.Map_.t
type nm_ops = (string,dir_entry,name_map_carrier) map_ops
let nm_ops = Map_string.map_ops


let mk_meta () = 
  time () |> fun t ->
  { atim=t;ctim=();mtim=t}


type dir_with_parent = {
  name_map:name_map_carrier;
  meta:meta;
  parent:did
}

let (dir_empty,dir_find,dir_add,dir_remove,dir_bindings) = 
  let dir_empty ~meta ~parent = { name_map=nm_ops.map_empty; meta; parent } in
  let dir_find k t = nm_ops.map_find k t.name_map in
  let dir_add k v t = {t with name_map=nm_ops.map_add k v t.name_map} in
  let dir_remove k t = {t with name_map=nm_ops.map_remove k t.name_map} in
  let dir_bindings t = nm_ops.map_bindings t.name_map in
  (dir_empty,dir_find,dir_add,dir_remove,dir_bindings)  


type file_meta_and_data = {
  meta: meta; 
  data: Tjr_buffer.buf
}
open Tjr_buffer
let contents_ops = Tjr_buffer.mk_buf_ops()


type files_carrier = file_meta_and_data Map_fid.Map_.t
type files_ops = (fid,file_meta_and_data,files_carrier) map_ops
let files_ops : files_ops = Map_fid.map_ops


type dirs_carrier = dir_with_parent Map_did.Map_.t
type dirs_ops = (did,dir_with_parent,dirs_carrier) map_ops
let dirs_ops : dirs_ops = Map_did.map_ops


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

(* list some of the map functions to work with fs *)
let lookup_did did fs = 
  fs.dirs |> fun map ->
  Map_did.map_ops.map_find did map


module Fs_t_to_json = struct
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
  let from_fs (fs:fs_t) = {
    files=files_ops.map_bindings fs.files |> List.map (fun (fid,c) -> 
        (fid,contents_ops.to_string c.data));
    max_fid=fs.max_fid;
    dirs=
      dirs_ops.map_bindings fs.dirs 
      |> List.map (fun (did,dir) -> 
          dir.name_map |> nm_ops.map_bindings |> fun bs ->
          (did,(bs,dir.parent)));
    max_did=fs.max_did;
  }
end


let fs_to_json fs = Fs_t_to_json.(
    from_fs fs |> fs'_to_yojson |> Yojson.Safe.pretty_to_string)


let empty_dir ~parent = dir_empty ~parent


let init_fs = {
  files=files_ops.map_empty;
  max_fid=fid0;
  dirs=(dirs_ops.map_empty |> dirs_ops.map_add root_did (empty_dir ~meta:(mk_meta()) ~parent:root_did));
  max_did=root_did;
  dir_handles=dhandles_ops.map_empty;
  max_dh=0;
}


type path = string


let is_fid = function
  | Fid _ -> true
  | _ -> false


let is_did = function
  | Did _ -> true 
  | _ -> false

let is_symlink x = function
  | Symlink _ -> true
  | _ -> false



(* extra ops on top of monad ---------------------------------------- *)

(* NOTE following can be built on top of with_fs and internal_err (?) *)
(* FIXME why make 'e a param? why not admit we are using exn_ ? *)
type 't extra_ops = {
  (* err: 'a. 'e -> (('a,'e)r_,'t) m;  (* FIXME? how can we handle all errors? *) *)
  new_did: unit -> (did,'t) m;
  new_fid: unit -> (fid,'t) m;
  with_fs: 'a. (fs_t -> 'a * fs_t) -> ('a,'t) m;  (* ASSUME-ed not to raise exception *)
  internal_err: 'a. string -> ('a,'t) m;
  dirs_add: did -> dir_with_parent -> (unit,'t) m;
  is_ancestor: parent:did -> child:dir_with_parent -> (bool,'t) m
}


(* main functionality ----------------------------------------------- *)

open Tjr_path_resolution

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
        (dirs_ops.map_find did s.dirs,s)) >>= fun r -> 
    match r with 
    | None -> extra_ops.internal_err "resolve_did, did not valid mim.l154"
    | Some dir -> return dir
  in
  let _ : did -> (dir_with_parent,'t) m = resolve_did in


  (* NOTE needs a dir_with_parent to resolve .. *)
  let resolve_name ~(dir_with_parent:dir_with_parent) ~name : dir_entry option = 
    dir_find name dir_with_parent 
  in

  (* we reuse the path_resolution library FIXME *)

  let dir_entry_option_to_resolve_result eopt : (fid,did) resolved_comp = 
    match eopt with
    | None -> RC_missing
    | Some x -> x |> function
      | Fid fid -> RC_file fid
      | Did did -> RC_dir did
      | Symlink s -> RC_sym s
  in

  (* path resolution *)
  let resolve = 
    let root = root_did in
    (* paths are always absolute when coming from fuse, and via the
       api... FIXME assert this? *)
    let resolve_comp (did:did) (comp:string) : ((fid,did)resolved_comp,'t) m = 
      (* we want to use resolve_name, but this works with dir_with_parent *)
      extra_ops.with_fs (fun fs -> 
          Map_did.map_ops.map_find did fs.dirs |> function
          | None -> failwith "impossible, invalid did"
          | Some dwp -> 
            resolve_name ~dir_with_parent:dwp ~name:comp 
            |> dir_entry_option_to_resolve_result |> fun x -> (x,fs))
    in
    let _ = resolve_comp in
    let fs_ops = { root; resolve_comp } in
    Tjr_path_resolution.resolve ~monad_ops ~fs_ops
  in




(*
  (* get parent and pass to rn_2 *)
  let rec resolve_names_1 ~parent_id ~names 
    : (did * dir_entry option,'e1)result m 
    = 
    resolve_did parent_id >>= fun parent -> 
    resolve_names_2 ~parent_id ~parent ~names 

  and resolve_names_2 ~parent_id ~parent ~names = 
    match names with
    | [] -> return @@ Ok(parent_id,None)
    | name::names -> 
      begin
        resolve_name ~dir:parent ~name |> function
        | None -> (
            match names with
            | [] -> return @@ Ok(parent_id,None)
            | _ -> err `Error_no_entry) (* FIXME give full path *)
        | Some (Fid fid) -> (
            match names with 
            | [] -> return @@ Ok(parent_id,Some (Fid fid))
            | _ -> err @@ `Error_not_directory)
        | Some (Did did) -> (
            match names with
            | [] -> return @@ Ok(parent_id,Some (Did did))
            | _ -> resolve_names_1 ~parent_id:did ~names)
        | Some (Symlink sl) -> (
            (* 
               if starts with /, then resolve from root
               if ends with /, then split is (a;b;c;"") so probably we should remove ""
               otherwise split and prepend to names *)
            failwith "FIXME"
          )
      end
  in


  let _ = resolve_names_1 in

  let resolve_path : path -> (did * dir_entry option,'e2)result m = fun p -> 
    (if not @@ String.contains p '/' 
     then extra_ops.internal_err "resolve_path, no slash, mim.l189" 
     else 
       String.split_on_char '/' p |> fun names ->
       if not (List.hd names = "")
       then extra_ops.internal_err "resolve_path, no leading slash, mim.194"
       else 
         let names=List.tl names in
         if names=[] 
         then extra_ops.internal_err "resolve_path, names empty, impossible, mim.198"
         else 
           let names = Tjr_list.(
               if last names = "" then butlast names else names) 
           in
           (* not sure about special casing root *)
           if names = [] 
           then return @@ Ok(root_did,Some (Did root_did)) 
           else resolve_names_1 ~parent_id:root_did ~names)
  in

  let _ = resolve_path in

*)

  let resolve_path ~follow_last_symlink path = 
    (* paths from Fuse should satisfy this; FIXME what if we are not using fuse? *)
    assert(String_util.starts_with_slash path); 
    let cwd = root_did in
    resolve ~follow_last_symlink ~cwd path >>= function
    | Ok r -> return (Ok r)
    | Error e -> err `Error_path_resolution  (* FIXME be more careful here? *)
  in


  (* FIXME not sure about Always in following two funs *)
  let resolve_dir_path (path:path) : ((did,'e3)result,'t) m = 
    resolve_path ~follow_last_symlink:`Always path >>= function 
    | Ok { result=(Dir did) } -> return (Ok did)
    | _ -> err `Error_not_directory
  in


  let resolve_file_path (path:path) : ((fid,'e4)result,'t) m = 
    resolve_path ~follow_last_symlink:`Always path >>= function
    | Ok { result=(File fid) } -> return (Ok fid)
    | _ -> err `Error_not_file
  in


  let root : path = "/" in


  (* FIXME or just allow unlink with no expectation of the kind? FIXME
     add a "follow" flag? optional? FIXME how does parent/name interact
     with symlinks? FIXME perhaps keep parent/name, but allow another
     layer ot deal with follow; FIXME not sure about `If_trailing_slash  *)
  let unlink path = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let Tjr_path_resolution.{ parent_id=pid; comp=name; result; trailing_slash } = rpath in
    begin
      extra_ops.with_fs (fun s ->
          s.dirs |> fun dirs ->
          dirs_ops.map_find pid dirs |> function
          | None -> `Internal "impossible, parent not found, mim.233",s
          | Some pdir -> 
            dir_find name pdir |> function
            | None -> `Error_no_entry,s
            | Some entry -> 
              (* FIXME with hardlinks, tims can change for other link *)
              dir_remove name pdir |> fun pdir ->
              dirs_ops.map_add pid pdir dirs |> fun dirs ->
              `Ok,{s with dirs}) 
      >>= (function 
          | `Ok -> return (Ok ())
          | `Internal s -> extra_ops.internal_err s
          | `Error_no_entry -> err @@ `Error_no_entry)
    end
    (* >>= (function
       | Ok x -> return (Ok x) | Error e -> return (Error e)) *)
  in

  let _ = unlink in 


  (* FIXME check if already exists etc *)
  (* FIXME meta changes for parent and child *)
  let mkdir path : ((unit,'e5)result,'t) m = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let Tjr_path_resolution.{ parent_id=pid; comp=name; result; trailing_slash } = rpath in
    match result with 
    | Missing -> (
        begin
          let meta = mk_meta () in
          extra_ops.new_did () >>= fun (did:did) -> 
          extra_ops.with_fs (fun s -> 
              s.dirs |> fun dirs ->
              (* add new empty dir to dirs *)
              dirs_ops.map_add did (empty_dir ~meta ~parent:pid) dirs |> fun dirs ->
              (* add name to parent *)
              dirs_ops.map_find pid s.dirs |> fun pdir ->
              match pdir with 
              | None -> `Internal "impossible, parent not found mim.l259",s
              | Some pdir -> 
                dir_add name (Did did) pdir |> fun pdir ->
                (* update parent meta *)
                {pdir with meta} |> fun pdir ->
                (* update parent in dirs *)
                dirs_ops.map_add pid pdir dirs |> fun dirs ->
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
          dirs_ops.map_find did dirs |> function
          | None -> `Internal "opendir, impossible, dir not found mim.278",s
          | Some dir -> 
            dir_bindings dir |> List.map fst |> fun names ->
            1+s.max_dh |> fun dh ->
            s.dir_handles |> fun handles ->
            dhandles_ops.map_add dh names handles |> fun dir_handles ->
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
        dhandles_ops.map_find dh handles |> function
        | None -> `Internal "readdir, impossible, dh not found mim.328",s
        | Some xs -> `Ok xs,s) 
    >>= function
    | `Ok xs -> return (Ok(xs,finished))
    | `Internal s -> extra_ops.internal_err s
  in


  let closedir dh = return (Ok()) in (* FIXME should we record which rd are valid? ie not closed *)


  (* default size preallocated for a file *)
  let internal_len = 1024 in 


  let create path : ((unit,'e6)result,'t) m = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let Tjr_path_resolution.{ parent_id=parent; comp=name; result; trailing_slash } = rpath in
    extra_ops.new_fid () >>= fun (fid:fid) -> 
    let meta = mk_meta() in
    extra_ops.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        dirs_ops.map_find parent dirs |> function
        | None -> `Internal "create, impossible, parent did not found mim.299",s
        | Some pdir ->
          dir_add name (Fid fid) pdir |> fun pdir ->
          (* update pdir meta *)
          {pdir with meta} |> fun pdir ->
          dirs_ops.map_add parent pdir dirs |> fun dirs ->
          {s with dirs} |> fun s ->
          s.files |> fun files ->
          let data = contents_ops.create ~internal_len 0 in
          files_ops.map_add fid {meta;data} files |> fun files ->
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
  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    (* buf_size_check length; *)
    let fid = fd2int fd in
    extra_ops.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "pread, impossible, no file, mim.325",s
        | Some(f) ->
          let clen = contents_ops.len f.data in
          (* NOTE we have some flexibility to choose length *)
          assert(length >= 0);
          assert(foff >= 0);
          assert(boff >= 0);
          assert(boff+length <= Biga.length buffer);
          (* following assures that foff+length<=clen *)
          let length = if foff > clen then 0 else min length (clen - foff) in
          match () with
          (* NOTE foff may be > clen, but then length is 0 *)
          | _ when length=0 -> (`Ok 0,s)
          | _ -> 
            contents_ops.blit_buf_to_bigarray 
              ~src:f.data ~soff:foff ~len:length 
              ~dst:buffer ~doff:boff;
            (`Ok length,s)) 
    >>= function
    | `Internal s -> extra_ops.internal_err s
    | `Ok l -> return (Ok l)
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    (* buf_size_check length; *)
    extra_ops.with_fs (fun s ->
        let fid = fd2int fd in
        let meta = mk_meta() in
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "pwrite, impossible, no file, mim.339",s
        | Some f ->
          let blen = Biga.length buffer in
          assert(length >= 0);
          assert(foff >= 0);
          assert(boff >= 0);
          assert(boff+length <= blen);
          (* we allow foff beyond EOF *)
          let contents = 
            if foff+length > contents_ops.len f.data 
            then contents_ops.resize (foff+length) f.data
            else f.data
          in
          assert(foff+length <= contents_ops.len contents);
          contents_ops.blit_bigarray_to_buf
            ~src:buffer ~soff:boff ~len:length ~dst:contents ~doff:foff 
          |> fun data ->
          (files_ops.map_add fid {f with data;meta} files)[@ ocaml.warning "-23"] |> fun files ->
          (`Ok length,{s with files}))
    >>= function
    | `Internal s -> extra_ops.internal_err s
    | `Ok l -> return (Ok l)  
  in


  let close fd = return (Ok ()) in (* FIXME record which are open? *)


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
            let meta = mk_meta() in
            let insert_and_remove id =
              match spath.parent_id = dpath.parent_id with
              | true -> 
                dir_add dcomp id ddir |> fun ddir ->
                dir_remove scomp ddir |> fun ddir ->
                {ddir with meta} |> fun ddir ->
                extra_ops.dirs_add dpath.parent_id ddir >>= fun () ->
                return (Ok ())
              | false -> 
                dir_add dcomp id ddir |> fun ddir ->
                {ddir with meta} |> fun ddir ->
                dir_remove scomp sdir |> fun sdir ->
                {sdir with meta} |> fun sdir ->
                extra_ops.dirs_add spath.parent_id sdir >>= fun () ->
                extra_ops.dirs_add dpath.parent_id ddir >>= fun () ->
                return (Ok ())
            in
            match spath.result,dpath.result with
            (* FIMXE following for symlinks *)
            | Sym _,_ | _,Sym _ -> failwith "FIXME shouldn't happen - follow=`Always"
            | File fid,Missing -> insert_and_remove (Fid fid)
            | File fid1,File fid2 -> 
              if fid1=fid2 
              then return (Ok ())
              else insert_and_remove (Fid fid1)
            | File fid,Dir did ->
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
                    {sdir with meta} |> fun sdir ->
                    (* new directory id *)
                    extra_ops.new_did() >>= fun did ->
                    (* record new directory with updated parent *)
                    extra_ops.dirs_add did { sdir with parent=dpath.parent_id } >>= fun () ->
                    insert_and_remove (Did did))
            | Dir sdid,File fid -> 
              err `Error_attempt_to_rename_dir_over_file  (* FIXME correct ? *)
            | Dir sdid,Dir ddid ->
              if sdid=ddid 
              then return (Ok ())
              else extra_ops.internal_err "FIXME rename d to d, dst should be empty?"
            | Missing ,_ -> failwith "impossible"
    end
    (*    >>= function
          | Ok x -> return (Ok x)
          | Error e -> err `EOTHER*)
  in


  (* FIXME truncate parent name; FIXME also stat *)
  let truncate ~path ~length = 
    resolve_file_path path >>=| fun fid ->
    extra_ops.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "file not found mim.362",s
        | Some f ->
          contents_ops.resize length f.data  |> fun data ->
          let meta = mk_meta() in
          files_ops.map_add fid ({f with data;meta}[@ocaml.warning "-23"]) files |> fun files ->
          `Ok (),{s with files}) 
    >>= function
    | `Ok () -> return (Ok ())
    | `Internal s -> extra_ops.internal_err s
  in

  (* FIXME here and elsewhere atim is not really dealt with *)
  let stat path = 
    resolve_path ~follow_last_symlink:`If_trailing_slash path >>=| fun rpath ->
    let Tjr_path_resolution.{ parent_id=pid; comp=name; result; trailing_slash } = rpath in
    begin
      match result with
      | Missing -> return `Error_no_entry
      | File fid ->
        extra_ops.with_fs (fun s ->
            s.files |> fun files ->
            files_ops.map_find fid files |> function
            | None -> `Internal "file fid not found mim.379",s
            | Some f ->          
              contents_ops.len f.data |> fun sz ->
              f.meta |> fun meta ->
              `Ok { sz;meta;kind=`File },s)
      | Dir did -> 
        extra_ops.with_fs (fun s ->
            s.dirs |> fun dirs ->
            dirs_ops.map_find did dirs |> function
            | None -> `Internal "dir did not found mim.651",s
            | Some f ->          
              let sz = 1 in (* for dir? FIXME size of dir *)
              f.meta |> fun meta ->
              `Ok { sz;meta;kind=`Dir },s)
      | Sym _ -> failwith "FIXME"
    end
    >>= function
    | `Ok stat -> return (Ok stat)
    | `Error_no_entry -> return (Error `Error_no_entry)
    | `Internal s -> extra_ops.internal_err s
  in


  let reset () = return () in

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; reset }

let _ = mk_ops

(* monad ------------------------------------------------------------ *)


type t = { 
(*  thread_error_state: exn_ option;  (* for current call *) *)
  internal_error_state: string option;
  fs: fs_t
}


let init_t = {
(*  thread_error_state=None; *)
  internal_error_state=None;
  fs=init_fs
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
module Y_ = struct
  open Fs_t_to_json
  type t' = {
    internal_error_state: string option;
    fs:fs' 
  } [@@deriving yojson]

  let from_t (t:t) = {
    internal_error_state=t.internal_error_state;
    fs=Fs_t_to_json.from_fs t.fs
  }
end


let t_to_string t = Y_.(
    t |> from_t |> t'_to_yojson |> Yojson.Safe.pretty_to_string)




(* extra ----------------------------------------------------------- *)

(*
  let with_fs (f:fs_t -> 'a * fs_t) : 'a m = 
    with_state 
      (fun w -> f w.fs |> fun (x,fs') -> x,{w with fs=fs'}) 
  let _ = with_fs

  let internal_err s : ('a,'t) m = 
    let open Tjr_step_monad.Step_monad_implementation in
    Step(fun w -> 
        ({ w with internal_error_state=(Some s)}, 
         `Inr(Step(fun _ -> failwith __LOC__))))

  let _ = internal_err


  let err e : 'a m = 
    let open Tjr_step_monad.Step_monad_implementation in
    Step(fun w -> 
        (w,`Inl (Error e)))

*)



module Extra_core = struct
  type 't t = {
    with_fs: 'a. (fs_t -> 'a * fs_t) -> ('a,'t) m;
    internal_err: 'a. string -> ('a,'t) m
  }
end

let mk_extra_ops ~monad_ops ~(extra_core:'t Extra_core.t) = 

  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  
  let Extra_core.{with_fs;internal_err} = extra_core in

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
        dirs_ops.map_find k dirs |> fun v ->
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
        dirs_ops.map_add k v dirs |> fun dirs ->
        (),{s with dirs})
  in

  let extra = { new_did; new_fid; with_fs; internal_err; is_ancestor; dirs_add } in
  extra


let mk_ops ~monad_ops ~extra_core = 
  let extra_ops = mk_extra_ops ~monad_ops ~extra_core in
  mk_ops ~monad_ops ~extra_ops
