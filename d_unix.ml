open C_base

(* to make integration with fuse easier; extunix supports this in module BA *)
type fuse_buffer = Fuse.buffer

type buffer = fuse_buffer


(* unix impl -------------------------------------------------------- *)

type path = string

(*type t = state*)

type dh = Unix.dir_handle

type fd = Unix.file_descr


(* exception No_such_entry *)


(* ops -------------------------------------------------------------- *)

type w = {
  error_state: exn option;
  world_state: unit
}

let initial_world : w = { error_state=None; world_state=() }

module Unix_monad = struct
  type 'a m = ('a,w)Step_monad.m
  let bind,return = Step_monad.(bind,return)
end
include Unix_monad

(* generate types --------------------------------------------------- *)

module Ops_type = C_post_msgs.Make_ops_type(Unix_monad)(Unix_base_types)
include Ops_type

module Imp_ops_type = C_post_msgs.Make_imp_ops_type(
  struct
    include Unix_monad
    include Unix_base_types
    include Ops_type
  end)



