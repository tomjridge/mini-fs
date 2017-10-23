
(* access a_* b_* via this module *)
include A_monad_type
include A_error
module Step_monad = B_step_monad

(* following for strings *)
let dirname_basename path = 
  ignore (Tjr_string.starts_with ~prefix:"/" path || failwith __LOC__);
  Tjr_string.split_on_last ~sub:"/" path |> fun (p,c) -> 
  (* the semantics is that dirname is an absolute path *)
  (if p="" then "/" else p),c


include Bigarray_buffer

(* minimal fs-like thing *)
type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }


type is_finished = bool
let finished = true

(* ensure 64 bit system *)
let _ = assert(Sys.int_size = 63)

type length = int (* FIXME in following *)
type offset = int


include struct
  open Bin_prot.Std
  type path=string [@@deriving bin_io, yojson]
  type dh=int
end


(* base types ------------------------------------------------------- *)

module type BASE_TYPES = sig
  type fd
  type dh
  val fd2int: fd -> int
end

module Mem_base_types = struct
  type fd = int
  type dh = int
  let fd2int x = x
end

module Unix_base_types = struct
  type fd=Unix.file_descr
  type dh=Unix.dir_handle
  let fd2int x = ExtUnix.All.int_of_file_descr x
end

module Lwt_base_types = struct
  type fd=Lwt_unix.file_descr
  type dh=Lwt_unix.dir_handle
  let fd2int x = x|>Lwt_unix.unix_file_descr|>ExtUnix.All.int_of_file_descr
end

module Abstract_base_types = struct
  type fd
  type dh
  let fd2int x = failwith __LOC__
end


