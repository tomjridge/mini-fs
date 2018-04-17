open Tjr_either
open Tjr_map
open Base_
open Ops_types

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


module X_ = struct
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


let fs_to_json fs = X_.(
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


module In_mem_monad = struct
  open Monad
  type 'a m = ( 'a, t) Monad.m
  let bind,return = bind,return
  let run w a = 
    let dest_exceptional w = w.internal_error_state in
    Step_monad.run ~dest_exceptional w a
    
end
include In_mem_monad

(* easily json-able *)
module Y_ = struct
  open X_
  type t' = {
    internal_error_state: string option;
    fs:fs' 
  } [@@deriving yojson]

  let from_t (t:t) = {
    internal_error_state=t.internal_error_state;
    fs=X_.from_fs t.fs
  }
end


let t_to_string t = Y_.(
    t |> from_t |> t'_to_yojson |> Yojson.Safe.pretty_to_string)


(* generate types --------------------------------------------------- *)

module MBR = struct
  include In_mem_monad
  include Mem_base_types
  include R_as_result
end


module Ops_type = Make_ops_type(MBR)
include Ops_type


module Ops_type_plus = struct
  include MBR
  include Ops_type
end



(* main functionality ----------------------------------------------- *)

open Tjr_map
open Step_monad
open Base_
open In_mem_monad

type 'e extra_ops = {
  err: 'a 'e. 'e -> ('a,'e)result m;
  new_did: unit -> did m;
  new_fid: unit -> fid m;
  with_fs: 'a. (fs_t -> 'a * fs_t) -> 'a m;  (* ASSUME-ed not to raise exception *)
  internal_err: 'a. string -> 'a m;
  dirs_add: did -> dir_with_parent -> unit m;
  is_parent: parent:did -> child:dir_with_parent -> bool m
}



let mk_ops ~extra = 
  let err = extra.err in
  let ( >>= ) = bind in
  let ( >>=| ) a b = a >>= function
    | Ok a -> b a
    | Error e -> return (Error e)
  in

  let resolve_did did = 
    extra.with_fs (fun s ->
        (dirs_ops.map_find did s.dirs,s)) >>= fun r -> 
    match r with 
    | None -> extra.internal_err "resolve_did, did not valid mim.l154"
    | Some dir -> return dir
  in
  let _ : did -> dir_with_parent m = resolve_did in


  (* NOTE needs a dir_with_parent to resolve .. *)
  let resolve_name ~(dir_with_parent:dir_with_parent) ~name : dir_entry option = 
    dir_find name dir_with_parent 
  in

  (* we reuse the path_resolution library FIXME *)

  let dir_entry_option_to_resolve_result eopt = 
    let open Tjr_path_resolution in
    match eopt with
    | None -> Missing
    | Some x -> x |> function
      | Fid fid -> File fid
      | Did did -> Dir did
      | Symlink s -> Sym s
  in

  (* path resolution *)
  let resolve = 
    let root = root_did in
    (* paths are always absolute when coming from fuse, and via the
       api... FIXME assert this? *)
    let resolve_comp (did:did) (comp:string) 
      : ((fid,did) Tjr_path_resolution.resolve_result,'t) Tjr_fs_shared.Monad.m
      = 
      let open Tjr_path_resolution in
      (* we want to use resolve_name, but this works with dir_with_parent *)
      extra.with_fs (fun fs -> 
          Map_did.map_ops.map_find did fs.dirs |> function
          | None -> failwith "impossible, invalid did"
          | Some dwp -> 
            resolve_name ~dir_with_parent:dwp ~name:comp 
            |> dir_entry_option_to_resolve_result |> fun x -> (x,fs))
    in
    let _ = resolve_comp in
    let fs_ops = Tjr_path_resolution.{ 
        root; 
        resolve_comp;
      }        
    in
    failwith "FIXME"
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
     then extra.internal_err "resolve_path, no slash, mim.l189" 
     else 
       String.split_on_char '/' p |> fun names ->
       if not (List.hd names = "")
       then extra.internal_err "resolve_path, no leading slash, mim.194"
       else 
         let names=List.tl names in
         if names=[] 
         then extra.internal_err "resolve_path, names empty, impossible, mim.198"
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

  let resolve_path = failwith "" in

  let resolve_dir_path (path:path) : (did,'e3)result m = 
    resolve_path path >>= function 
    | Ok(_,Some (Did did)) -> return (Ok did)
    | _ -> err `Error_not_directory
  in


  let resolve_file_path (path:path) : (fid,'e4)result m = 
    resolve_path path >>= function
    | Ok(_,Some (Fid fid)) -> return (Ok fid)
    | _ -> err `Error_not_file
  in

  let root : path = "/" in

  

  (* FIXME or just allow unlink with no expectation of the kind? FIXME
     add a "follow" flag? optional? FIXME how does parent/name interact
     with symlinks? FIXME perhaps keep parent/name, but allow another
     layer ot deal with follow  *)
  let unlink path = 
    resolve_dir_path parent >>=| fun pid ->
      begin
        extra.with_fs (fun s ->
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
            | `Internal s -> extra.internal_err s
            | `Error_no_entry -> err @@ `Error_no_entry) 
      end
      (* >>= (function
      | Ok x -> return (Ok x) | Error e -> return (Error e)) *)
  in

  let _ = unlink in 


  (* FIXME check if already exists etc *)
  (* FIXME meta changes for parent and child *)
  let mkdir ~parent ~name : (unit,'e5)result m = 
    resolve_dir_path parent >>=| fun pid -> 
      begin
        let meta = mk_meta () in
        extra.new_did () >>= fun (did:did) -> 
        extra.with_fs (fun s -> 
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
    >>= (function
        | `Ok -> return (Ok ())
        | `Internal s -> extra.internal_err s)
  in

  let _ = mkdir in

  (* let mk_dh ~did es = (did,es) in *)


  (* FIXME update atim *)
  let opendir path = 
    resolve_dir_path path >>=| fun did ->
      begin
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
              `Ok(dh),s)
      end
      >>= (function
          | `Ok dh -> return (Ok dh)
          | `Internal s -> extra.internal_err s)
  in


  (* FIXME atim *)
  let readdir dh = 
    extra.with_fs (fun s ->
        s.dir_handles |> fun handles ->
        dhandles_ops.map_find dh handles |> function
        | None -> `Internal "readdir, impossible, dh not found mim.328",s
        | Some xs -> `Ok xs,s) 
    >>= function
    | `Ok xs -> return (Ok(xs,finished))
    | `Internal s -> extra.internal_err s
  in


  let closedir dh = return (Ok()) in (* FIXME should we record which rd are valid? ie not closed *)


  (* default size preallocated for a file *)
  let internal_len = 1024 in 

  
  let create ~parent ~name : (unit,'e6)result m = 
    resolve_dir_path parent >>=| fun parent ->
      extra.new_fid () >>= fun (fid:fid) -> 
      let meta = mk_meta() in
      extra.with_fs (fun s -> 
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
      | `Internal s -> extra.internal_err s
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
    extra.with_fs (fun s ->
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
    | `Internal s -> extra.internal_err s
    | `Ok l -> return (Ok l)
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    (* buf_size_check length; *)
    extra.with_fs (fun s ->
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
    | `Internal s -> extra.internal_err s
    | `Ok l -> return (Ok l)  
  in


  let close fd = return (Ok ()) in (* FIXME record which are open? *)


  (* FIXME ddir and sdir may be the same, so we need to be careful to
     always use indirection via did *)
  let rename ~spath ~sname ~dpath ~dname = 
    begin
      resolve_dir_path spath >>= function Error e -> err e | Ok sdir_did ->
        resolve_dir_path dpath >>= function Error e -> err e | Ok ddir_did ->
          resolve_did sdir_did >>= fun sdir ->
          resolve_name ~dir:sdir ~name:sname |> function
          | None -> err `Error_no_src_entry
          | Some resolved_sname -> 
            resolve_did ddir_did >>= fun ddir ->
            resolve_name ~dir:ddir ~name:dname |> fun resolved_dname ->
            let meta = mk_meta() in
            let insert_and_remove id =
              match sdir_did = ddir_did with
              | true -> 
                dir_add dname id ddir |> fun ddir ->
                dir_remove sname ddir |> fun ddir ->
                {ddir with meta} |> fun ddir ->
                extra.dirs_add ddir_did ddir >>= fun () ->
                return (Ok ())
              | false -> 
                dir_add dname id ddir |> fun ddir ->
                {ddir with meta} |> fun ddir ->
                dir_remove sname sdir |> fun sdir ->
                {sdir with meta} |> fun sdir ->
                extra.dirs_add sdir_did sdir >>= fun () ->
                extra.dirs_add ddir_did ddir >>= fun () ->
                return (Ok ())
            in
            match resolved_sname,resolved_dname with
            (* FIMXE following for symlinks *)
            | Symlink _,_ | _,Some(Symlink _) -> failwith "FIXME"
            | Fid fid,None -> insert_and_remove (Fid fid)
            | Fid fid1,Some(Fid fid2) -> 
              if fid1=fid2 
              then return (Ok ())
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
                    {sdir with meta} |> fun sdir ->
                    (* new directory id *)
                    extra.new_did() >>= fun did ->
                    (* record new directory with updated parent *)
                    extra.dirs_add did { sdir with parent=ddir_did } >>= fun () ->
                    insert_and_remove (Did did))
            | Did sdid,Some(Fid fid) -> 
              err `Error_attempt_to_rename_dir_over_file  (* FIXME correct ? *)
            | Did sdid,Some(Did ddid) ->
              if sdid=ddid 
              then return (Ok ())
              else extra.internal_err "FIXME rename d to d, dst should be empty?"
    end
(*    >>= function
    | Ok x -> return (Ok x)
    | Error e -> err `EOTHER*)
  in


  (* FIXME truncate parent name; FIXME also stat *)
  let truncate ~path ~length = 
    resolve_file_path path >>=| fun fid ->
      extra.with_fs (fun s ->
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
      | `Internal s -> extra.internal_err s
  in

  (* FIXME here and elsewhere atim is not really dealt with *)
  let stat path = 
    resolve_path path >>=| function (did,id) -> 
      begin
        match id with
        | None -> return `Error_no_entry
        | Some id ->
          match id with 
          | Fid fid -> (
              extra.with_fs (fun s ->
                  s.files |> fun files ->
                  files_ops.map_find fid files |> function
                  | None -> `Internal "file fid not found mim.379",s
                  | Some f ->          
                    contents_ops.len f.data |> fun sz ->
                    f.meta |> fun meta ->
                    `Ok { sz;meta;kind=`File },s))
          | Did did -> (
              extra.with_fs (fun s ->
                  s.dirs |> fun dirs ->
                  dirs_ops.map_find did dirs |> function
                  | None -> `Internal "dir did not found mim.651",s
                  | Some f ->          
                    let sz = 1 in (* for dir? FIXME size of dir *)
                    f.meta |> fun meta ->
                    `Ok { sz;meta;kind=`Dir },s))
          | Symlink _ -> failwith "FIXME"
      end
      >>= function
      | `Ok stat -> return (Ok stat)
      | `Error_no_entry -> return (Error `Error_no_entry)
      | `Internal s -> extra.internal_err s
  in


  let reset () = return () in

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; reset }


(* extra ----------------------------------------------------------- *)

include struct
  open Step_monad

  let ( >>= ) = Step_monad.bind

  let with_fs (f:fs_t -> 'a * fs_t) : 'a m = 
    with_state 
      (fun w -> f w.fs |> fun (x,fs') -> x,{w with fs=fs'}) 

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

  let internal_err s : 'a m = 
    Step(fun w -> 
        ({ w with internal_error_state=(Some s)}, Inr(Step(fun _ -> 
             failwith __LOC__))))

  let _ = internal_err

  let dirs_find k =
    with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_find k dirs |> fun v ->
        v,s)

  (* FIXME we could maintain a list of parents when resolving, of course *)
  let is_parent ~parent ~child = 
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
    with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_add k v dirs |> fun dirs ->
        (),{s with dirs})

  let err e : 'a m = 
    Step(fun w -> 
        (w,Inl (Error e)))

  let extra = { new_did; new_fid; with_fs; internal_err; is_parent; dirs_add; err }

end

let ops = mk_ops ~extra  

let _ : ops = ops


(* running ---------------------------------------------------------- *)

(* NOTE we run till Inl, since errors are not explicit in the monad *)
(*
let dest_exceptional w = 
  (match w.internal_error_state with
   | None -> ()
   | Some s ->  
     Printf.sprintf "fmem.458: internal error state not none: %s\n" s |> exit_1);
  w.thread_error_state
*)


(* let e2s = fun e -> e|>exn__to_string *)

module Run_pure = struct 

  (* NOTE don't attempt to step if this is Some _ *)
  let dest_exceptional x = x.internal_error_state

  let rec run (w:t) (x:'a m) = 
    w |> dest_exceptional |> function
    | None -> (
      Step_monad.run ~dest_exceptional w x
      |> function
      | Ok (w,a) -> (w,a)
      | Error (`Attempt_to_step_exceptional_state w) -> 
        failwith __LOC__ (* impossible *))
    | Some s -> 
      (* for internal errors, just fail *)
      failwith s

  let _ = run
end

(* imperative ------------------------------------------------------- *)

(*

module Run_imperative = struct
  open Run_pure  

  (* specialize to result type, and throw an exception in error case *)
  let run ~ref_ a = run (!ref_) a |> fun (w,a) -> ref_:=w; a |> function
    | Ok a -> a
    | Error e -> failwith __LOC__  (* FIXME discards too much info? *)

end

include struct
  open Imp_ops_type
  open Run_imperative
  let mk_imperative_ops ~ref_ = 
    mk_imperative_ops ~run:{run=(fun x -> run ~ref_ x)}

end

(*
  (* NOTE this is the runtime exception that results from using
     imperative operations with the in_mem impl *)
  exception In_mem_runtime_exception of exn_*t

  type raise_={
    raise_:'a. exn_*t-> 'a
  }

  let imp_run ~ref_ ~raise_ : run = {
    run=(fun x -> run (!ref_) x |> function
      | `Exn_ (e,w) ->        
        log_.log_lazy (fun () -> Printf.sprintf "fmem.495: run resulted in exn_: %s\n" (exn__to_string e));
        (* ASSUME internal_error_state is None; don't record the
           thread_error_state, since that is per call *)
        ref_:={ !ref_ with fs=w.fs };  
        raise_.raise_ (e,w)
      | `Finished(a,w) -> 
        ref_:=w;
        a)
  }

  let mk_imperative_ops ~ref_ ~raise_ = mk_imperative_ops ~run:(imp_run ~ref_ ~raise_)

*)


(* logging ---------------------------------------------------------- *)

*)
