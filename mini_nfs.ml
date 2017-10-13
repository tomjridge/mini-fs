(* mini network filesystem ------------------------------------------ *)

open Step_monad
open Mini_pervasives
open Minifs
open Msgs


(* client ----------------------------------------------------------- *)

module C_ = struct
type ('e,'m) extra_ops = {
  internal_err: 'a. string -> ('a,'m)m;
}
end
open C_

(* construct message, send, recv *)
let mk_client_ops (type t) 
    ~monad_ops
    ~extra_ops
    ~(call:msg_from_client -> (msg_from_server','m) m)
    ~data_length
    ~data_of_buffer
    ~blit_data_to_buffer
  = 
  let (bind,return,err) = (monad_ops.bind,monad_ops.return,monad_ops.err) in
  let ( >>= ) = bind in  
  let internal_err = extra_ops.internal_err in
  let ty_err = "incorrect return return type from server" in
  let root : path = "/" in
  let ret_unit : msg_from_server' -> (unit,'m) m = function
    | Unit -> return ()
    | _ -> internal_err @@ "ret_unit, "^ty_err^" mnfs.32"
  in
  let unlink ~parent ~name = Unlink(parent,name) |> call >>= ret_unit in
  let mkdir ~parent ~name = Mkdir(parent,name) |> call >>= ret_unit in
  let opendir p = Opendir(p) |> call >>=function
    | Dh dh -> return dh
    | _ -> internal_err @@ "opendir, "^ty_err^" mnfs.39"
  in
  let readdir dh = Readdir(dh) |> call >>= function
    | Readdir' (xs,b) -> return (xs,b)
    | _ -> internal_err @@ "readdir, "^ty_err^" mnfs.43"
  in
  let closedir dh = Closedir(dh) |> call >>= ret_unit in
  let create ~parent ~name = Create(parent,name) |> call >>= ret_unit in
  let open_ p = Open(p) |> call >>= function
    | Open' fd -> return fd
    | _ -> internal_err @@ "open_, "^ty_err^" mnfs.49"
  in
  let pread ~fd ~foff ~length ~buffer ~boff =
    Pread(fd,foff,length) |> call >>= function
    | Pread' data -> 
      blit_data_to_buffer data buffer boff;
      return (data_length data)
    | _ -> internal_err @@ "pread, "^ty_err^" mnfs.56"
  in
  let pwrite ~fd ~foff ~length ~buffer ~boff =
    data_of_buffer ~buffer ~boff ~length |> fun data ->
    Pwrite(fd,foff,data) |> call >>= function
    | Int nwritten -> return nwritten
    | _ -> internal_err @@ "pwrite, "^ty_err^" mnfs.62"
  in
  let close fd = Close fd |> call >>= ret_unit in
  let rename ~spath ~sname ~dpath ~dname = 
    Rename(spath,sname,dpath,dname) |> call >>= ret_unit in
  let truncate ~path ~length = Truncate(path,length) |> call >>= ret_unit in
  let stat_file p = Stat_file p |> call >>= function
    | Stat_file' st -> return st
    | _ -> internal_err @@ "stat_file, "^ty_err^" mnfs.68"
  in
  let kind p = Kind p |> call >>= function
    | Kind' k -> return k
    | _ -> internal_err @@ "kind, "^ty_err^" mnfs.72"
  in
  let reset () = Reset |> call >>= ret_unit in

  assert(
    Minifs.wf_ops 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
    ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate 
    ~stat_file ~kind ~reset);
  mk_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset


(* server ----------------------------------------------------------- *)

module S_ = struct
type ('e,'m) extra_ops = {
  internal_err: 'a. string -> ('a,'m)m;
}
end
open S_

(* serve requests using a backend ops *)

let mk_serve
    ~monad_ops
    ~backend
    ~data_of_buffer
    ~buffer_of_data
    ~mk_buffer
  =
  let (bind,return,err) = Step_monad.(monad_ops.bind,monad_ops.return,monad_ops.err) in
  let ( >>= ) = bind in
  dest_ops backend @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->
  let ret_unit = fun () -> return Unit in
  let unlink ~parent ~name = unlink ~parent ~name >>= ret_unit in
  let mkdir ~parent ~name = mkdir ~parent ~name >>= ret_unit in
  let opendir p = opendir p >>= fun dh -> return @@ Dh dh in
  let readdir dh = readdir dh >>= fun (xs,b) -> return @@ Readdir' (xs,b) in
  let closedir dh = closedir dh >>= ret_unit in
  let create ~parent ~name = create ~parent ~name >>= ret_unit in
  let open_ p = open_ p >>= fun fd -> return @@ Open' fd in
  let pread ~fd ~foff ~length = 
    mk_buffer length |> fun buffer ->
    pread ~fd ~foff ~length ~buffer ~boff:0 >>= fun nread -> 
    data_of_buffer ~buffer ~len:nread |> fun data ->
    return @@ Pread' data
  in
  let pwrite ~fd ~foff ~data = 
    buffer_of_data data |> fun buffer ->
    pwrite ~fd ~foff ~length:(String.length data) ~buffer ~boff:0 >>= fun nwritten ->
    return @@ Int nwritten
  in
  let close fd = close fd >>= ret_unit in
  let rename ~spath ~sname ~dpath ~dname = rename ~spath ~sname ~dpath ~dname >>= ret_unit in
  let truncate ~path ~length = truncate ~path ~length >>= ret_unit in
  let stat_file p = stat_file p >>= fun st -> return @@ Stat_file' st in
  let kind p = kind p >>= fun k -> return @@ Kind' k in
  let serve' = function
    | Unlink(parent,name) -> unlink ~parent ~name 
    | Mkdir(parent,name) -> mkdir ~parent ~name 
    | Opendir(p) -> opendir p
    | Readdir dh -> readdir dh
    | Closedir dh -> closedir dh
    | Create(parent,name) -> create ~parent ~name
    | Open p -> open_ p
    | Pread(fd,foff,length) -> pread ~fd ~foff ~length
    | Pwrite(fd,foff,data) -> pwrite ~fd ~foff ~data
    | Close fd -> close fd 
    | Rename(spath,sname,dpath,dname) -> rename ~spath ~sname ~dpath ~dname
    | Truncate(path,length) -> truncate ~path ~length 
    | Stat_file p -> stat_file p
    | Kind p -> kind p
    | Reset -> reset () >>= ret_unit
  in
  let _ = serve' in
  serve'

(*
let mk_server
    ~serve
    ~(recv: unit -> (msg_from_client,'m) m)
    ~(send:msg_from_server' -> (unit,'m)m)
  =
  (* read incoming call, process, and return *)
  recv () >>= serve >>= send
*)

(* example in-memory ------------------------------------------------ *)

(* see bin/nfs_server.ml *)

