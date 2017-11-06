
(* access a_* b_* via this module *)
include Monad_type_
include Error_
module Step_monad = Tjr_step_monad 

let exit_1 = failwith  (* hopefully not be caught *)

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
end

(* FIXME note that this is the only mention of Lwt - so move elsewhere and drop dependency? *)
module Lwt_base_types = struct
  type fd=Lwt_unix.file_descr
  type dh=Lwt_unix.dir_handle
(*  let fd2int x = x|>Lwt_unix.unix_file_descr|>ExtUnix.All.int_of_file_descr *)
end

module Abstract_base_types = struct
  type fd
  type dh
end


(* logging ----------------------------------------------------------- *)

include Tjr_log
let log_ = mk_log_ops()

(* let log_ = {log_ with log=fun s -> ()}  (* FIXME performance *) *)

let () = at_exit log_.print_last_n


(* buffers size check  ---------------------------------------------- *)

let buf_size_check n = 
  if n < 0 || n > Sys.max_string_length 
  then (log_.log_now __LOC__; failwith __LOC__ )
  else ()



(* fix up unix ------------------------------------------------------ *)

(*
module Unix_ = struct
  include Unix
  type unix_error = Unix_error_ of error*string*string
end
*)