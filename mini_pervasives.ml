(* util *)


let blit_bigarray ~src ~soff ~len ~dst ~doff = 
  Bigarray.Array1.sub src soff len |> fun src ->
  Bigarray.Array1.sub dst doff len |> fun dst ->
  Bigarray.Array1.blit src dst


let blit_string_to_bigarray ~src ~soff ~len ~dst ~doff =
  for i=0 to len-1 do
    Bigarray.Array1.set dst (doff+i) (String.get src (soff+i));
    ()
  done

let blit_bigarray_to_bytes ~src ~soff ~len ~dst ~doff = 
  for i=0 to len-1 do
    Bytes.set dst (doff+i) (Bigarray.Array1.get src (soff+i));
    ()
  done

let blit_string_to_bytes  ~src ~soff ~len ~dst ~doff = 
  Bytes.blit_string src soff dst doff len

let thread_id () = Thread.(self () |> id)


(* fix path --------------------------------------------------------- *)
open Bin_prot.Std
type path = string [@@deriving bin_io, yojson]

