
(* access a_* b_* via this module *)
include Error_
include Stat_record

(* ensure 64 bit system *)
let _ = assert(Sys.int_size = 63)


let exit_1 = failwith  (* hopefully not be caught *)


(* following for strings *)
let dirname_basename path = 
  assert (String_.starts_with ~prefix:"/" path);
  String_.split_on_last ~sub:"/" path |> fun (p,c) -> 
  (* the semantics is that dirname is an absolute path *)
  (if p="" then "/" else p),c


include Bigarray_buffer


type is_finished = bool
let finished = true


type length = int (* FIXME in following *)
type offset = int


include struct
  open Bin_prot.Std
  type path=string [@@deriving bin_io, yojson]
  type dh=int  (* FIXME why specialize here? *)
end


(* logging ---------------------------------------------------------- *)

include Log_


(* buffers size check  ---------------------------------------------- *)

let buf_size_check n = 
  if n < 0 || n > Sys.max_string_length 
  then (log_.log_now __LOC__; failwith __LOC__ )
  else ()


