open C_base

module Unix_base_types = struct
  type fd=Unix.file_descr
  type dh=Unix.dir_handle
  let fd2int x = ExtUnix.All.int_of_file_descr x
end
include Unix_base_types

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



