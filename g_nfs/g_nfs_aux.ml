(* aux -------------------------------------------------------------- *)


(* NOTE data is Msgs.data = string; buffer is mim.buffer = bigarray *)
let data_of_buffer ~buffer ~off ~len =
  Bigarray_buffer.bigarray_to_string ~src:buffer ~off ~len

let buffer_of_data d = Bigarray_buffer.string_to_bigarray d
  
let mk_buffer = Bigarray_buffer.create

let data_length = String.length

let blit_data_to_buffer ~data ~buffer ~boff = 
  Bigarray_buffer.blit_string_to_bigarray ~src:data ~soff:0 ~len:(data_length data)
    ~dst:buffer ~doff:boff


(* example in-memory ------------------------------------------------ *)

(* see bin/nfs_server.ml *)

