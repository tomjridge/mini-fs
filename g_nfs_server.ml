(* nfs server ------------------------------------------------------- *)

(* we use some backing ops to provide the functionality, and the code
   below translates this to messages on the wire *)

open C_base
open D_functors

module Make_server(O:OPS_TYPE) = struct
  open O

  type extra_ops = {
    internal_err: 'a. string -> 'a m;
  }

  open C_msgs
  let mk_serve
      ~ops  (* backend *)
      ~data_of_buffer
      ~buffer_of_data
      ~mk_buffer
      ~dh2i ~i2dh
      ~fd2i ~i2fd
    =
    let ( >>= ) = bind in
    let ret_unit = fun () -> return Unit in
    let unlink ~parent ~name = ops.unlink ~parent ~name >>= ret_unit in
    let mkdir ~parent ~name = ops.mkdir ~parent ~name >>= ret_unit in
    let opendir p = ops.opendir p >>= fun dh -> return @@ Dh (dh2i dh) in
    let readdir dh = ops.readdir dh >>= fun (xs,b) -> return @@ Readdir' (xs,b) in
    let closedir dh = ops.closedir dh >>= ret_unit in
    let create ~parent ~name = ops.create ~parent ~name >>= ret_unit in
    let open_ p = ops.open_ p >>= fun fd -> return @@ Open' (fd2i fd) in
    let pread ~fd ~foff ~length = 
      mk_buffer length |> fun buffer ->
      ops.pread ~fd ~foff ~length ~buffer ~boff:0 >>= fun nread -> 
      data_of_buffer ~buffer ~len:nread |> fun data ->
      return @@ Pread' data
    in
    let pwrite ~fd ~foff ~data = 
      buffer_of_data data |> fun buffer ->
      ops.pwrite ~fd ~foff ~length:(String.length data) ~buffer ~boff:0 >>= fun nwritten ->
      return @@ Int nwritten
    in
    let close fd = ops.close fd >>= ret_unit in
    let rename ~spath ~sname ~dpath ~dname = ops.rename ~spath ~sname ~dpath ~dname >>= ret_unit in
    let truncate ~path ~length = ops.truncate ~path ~length >>= ret_unit in
    let stat_file p = ops.stat_file p >>= fun st -> return @@ Stat_file' st in
    let kind p = ops.kind p >>= fun k -> return @@ Kind' k in
    let serve' = function
      | Unlink(parent,name) -> unlink ~parent ~name 
      | Mkdir(parent,name) -> mkdir ~parent ~name 
      | Opendir(p) -> opendir p
      | Readdir dh -> readdir (i2dh dh)
      | Closedir dh -> closedir (i2dh dh)
      | Create(parent,name) -> create ~parent ~name
      | Open p -> open_ p
      | Pread(fd,foff,length) -> pread ~fd:(i2fd fd) ~foff ~length
      | Pwrite(fd,foff,data) -> pwrite ~fd:(i2fd fd) ~foff ~data
      | Close fd -> close (i2fd fd)
      | Rename(spath,sname,dpath,dname) -> rename ~spath ~sname ~dpath ~dname
      | Truncate(path,length) -> truncate ~path ~length 
      | Stat_file p -> stat_file p
      | Kind p -> kind p
      | Reset -> ops.reset () >>= ret_unit
    in
    let _ :msg_from_client -> msg_from_server' m = serve' in
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

  include struct 
    open G_nfs_aux
    (* specialize mk_serve *)
    let mk_serve = 
      mk_serve 
        ~data_of_buffer:(fun ~buffer ~len -> data_of_buffer ~buffer ~off:0 ~len) 
        ~buffer_of_data
        ~mk_buffer
    let _ = mk_serve
  end

end
