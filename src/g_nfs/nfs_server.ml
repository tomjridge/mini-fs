(** NFS server, backed by some filesystem *)

(** We use some backing ops to provide the functionality, and the code
   below translates this to messages on the wire *)

open Minifs_intf
(* open Ops_type_ *)

(*
type 'w extra_ops = {
  internal_err: 'a. string -> ('a,'w) m;
}
*)

open Msgs

(** NOTE the server is roughly a function of type msg_from_client ->
    msg_from_server m, which wraps a backend ops *)
open Bigarray_buffer

let mk_serve
  ~monad_ops
    ~(ops:(_,_,_)ops)  (* backend *)
    ~data_of_buffer
    ~buffer_of_data
    ~mk_buffer
    ~dh2i ~i2dh
    ~fd2i ~i2fd
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let fmap_error: 
    'a. (('a,exn_)result,'w) m -> ('a -> msg_from_server') -> (msg_from_server,'w) m 
    = 
    fun a f -> 
      a >>= function
      | Ok a -> return (Ok_ (f a))
      | Error e -> return (Error_ e)
  in
  let ( >>=| ) = fmap_error in
  let ret_unit = fun () -> Unit in
  let unlink path = ops.unlink path >>=| ret_unit in
  let mkdir path = ops.mkdir path >>=| ret_unit in
  let opendir p = ops.opendir p 
    >>=| fun dh -> Dh (dh2i dh) in
  let readdir dh = ops.readdir dh 
    >>=| fun (xs,b) -> Readdir' (xs,b.is_finished) in
  let closedir dh = ops.closedir dh 
    >>=| ret_unit in
  let create path = ops.create path >>=| ret_unit in
  let open_ p = ops.open_ p >>=| fun fd -> Open' (fd2i fd) in
  let pread ~fd ~foff ~length = 
    (* ASSUMES length should not exceed Sys.max_string_length, since
       strings/bytes are used in bigarray_buffer *)
    (* FIXME how to make these sort of constraints across all impls? *)
    buf_size_check length;
    let length = min length Sys.max_string_length in
    mk_buffer length |> fun buffer ->
    ops.pread ~fd ~foff ~len:length ~buf:buffer ~boff:0 
    >>=| fun nread -> 
    buf_size_check nread;
    data_of_buffer ~buffer ~len:nread |> fun data ->
    Pread' data
  in
  let pwrite ~fd ~foff ~data = 
    let length = String.length data in
    buf_size_check length;
    let len = min length Sys.max_string_length in
    buffer_of_data data |> fun buffer ->
    ops.pwrite ~fd ~foff ~len ~buf:buffer ~boff:0 
    >>=| fun nwritten -> Int nwritten
  in
  let close fd = ops.close fd >>=| ret_unit in
  let rename src dst = ops.rename src dst >>=| ret_unit in
  let truncate path length = ops.truncate path length >>=| ret_unit in
  let stat p = ops.stat p >>=| fun st -> Stat' st in
  let symlink contents p = ops.symlink contents p >>=| ret_unit in
  let readlink p = ops.readlink p >>=| fun s -> Readlink' s in
  let serve' = function
    | Unlink path -> unlink path
    | Mkdir path -> mkdir path
    | Opendir(p) -> opendir p
    | Readdir dh -> readdir (i2dh dh)
    | Closedir dh -> closedir (i2dh dh)
    | Create path -> create path
    | Open p -> open_ p
    | Pread(fd,foff,length) -> pread ~fd:(i2fd fd) ~foff ~length
    | Pwrite(fd,foff,data) -> pwrite ~fd:(i2fd fd) ~foff ~data
    | Close fd -> close (i2fd fd)
    | Rename (src,dst) -> rename src dst
    | Truncate(path,length) -> truncate path length 
    | Stat p -> stat p
    | Symlink(cs,p) -> symlink cs p 
    | Readlink p -> readlink p
    | Reset -> ops.reset () >>= fun () -> return (Ok_ Unit)
  in
  let _ :msg_from_client -> (msg_from_server,'w) m = serve' in
  serve'

let _ = mk_serve

(*
let mk_server
    ~serve
    ~(recv: unit -> (msg_from_client,'m) m)
    ~(send:msg_from_server' -> (unit,'m)m)
  =
  (* read incoming call, process, and return *)
  recv () >>= serve >>= send
*)

(* FIXME maybe handle_client_msg would be better than serve *)

include struct 
  open Nfs_aux

  (** Specialize mk_serve using standard auxiliary functions *)
  let mk_serve = 
    mk_serve 
      ~data_of_buffer:(fun ~buffer ~len -> data_of_buffer ~buffer ~off:0 ~len) 
      ~buffer_of_data
      ~mk_buffer
  let _ = mk_serve
end

let _ = mk_serve

