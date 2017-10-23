open Tjr_map
open C_base

(* in-mem impl ------------------------------------------------------ *)


module Mem_base_types = struct
  type fd = int
  type dh = int
  let fd2int x = x
end
include Mem_base_types

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


type path = string


let is_fid = function
  | Fid _ -> true
  | _ -> false


let is_did x = not (is_fid x)


(* monad ------------------------------------------------------------ *)


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

module Monad = struct
  type 'a m = ('a,t) B_step_monad.m
  let bind,return = B_step_monad.(bind,return)
end
include Monad


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


(* generate types --------------------------------------------------- *)

module Ops_type = C_post_msgs.Make_ops_type(Monad)(Mem_base_types)
include Ops_type

module Imp_ops_type = C_post_msgs.Make_imp_ops_type(
    struct
      include Monad
      include Mem_base_types
      include Ops_type
    end)



