(* fuse uses this form of buffer *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create len : buffer = Bigarray.(Array1.(create char c_layout len))

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


let bigarray_to_string ~src ~off ~len = 
  Bytes.create len |> fun dst ->
  blit_bigarray_to_bytes ~src ~soff:off ~len ~dst ~doff:0;
  Bytes.unsafe_to_string dst


let string_to_bigarray s = 
  String.length s |> fun len ->
  create len |> fun buf ->
  blit_string_to_bigarray ~src:s ~soff:0 ~len ~dst:buf ~doff:0;
  buf
