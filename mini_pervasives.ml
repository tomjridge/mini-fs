(* util *)


include Bigarray_buffer

let thread_id () = Thread.(self () |> id)


(* fix path --------------------------------------------------------- *)
open Bin_prot.Std
type path = string [@@deriving bin_io, yojson]

