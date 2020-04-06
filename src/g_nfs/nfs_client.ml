(** NFS client: pass requests over the network to a server *)

(** The client makes network calls, which potentially could be in
   lwt.m or unix.m; so we parameterize over ops *)

open Log_
open Minifs_intf
(* open Ops_type_ *)


(** This is used to indicate that the result of a call was not what
   was expected *)
type 'w internal_marshal_err = {
  internal_marshal_err: 'a. string -> ('a,'w) m;
}

open Msgs

(** Construct a message, send to server, and wait for response *)
let mk_client_ops (* (type t) *)
    ~monad_ops
    ~internal_marshal_err
    (* NOTE call returns errors in the monad *)
    ~(call:msg_from_client -> (msg_from_server,'w) m)
    ~data_length
    ~data_of_buffer
    ~blit_data_to_buffer
    ~i2dh ~dh2i
    ~i2fd ~fd2i
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  (* internal marshalling errors *)
  let internal_err = internal_marshal_err.internal_marshal_err in
  let ty_err = "incorrect return return type from server" in

  (* NOTE after we use call m, we are left with a msg_from_server m;
     we then need a map from msg_from_server' to 'a to get a
     ('a,exn_)result m via fmap_error *)
  (* but we also want to trap errors in the protocol, and for this
     we need the more general version *)
  let fmap_error : 
    'a. (msg_from_server,'w) m -> 
    (msg_from_server' -> ('a,'w) m) -> 
    (('a,exn_)result,'w) m 
    = fun m f -> 
      m >>= function
      | Ok_ m' -> f m' >>= fun a -> return (Ok a)
      | Error_ e -> return (Error e)
  in

  let ( >>=| ) m f = fmap_error m f in

  let ret_unit : msg_from_server -> ((unit,exn_)result,'w) m = fun m ->
    return m >>=| function 
    | Unit -> return () 
    | _ -> internal_err @@ "opendir, "^ty_err^" mnfs.52" 
  in

  let root : path = "/" in

  let unlink path = Unlink(path) |> call >>= ret_unit in
  let mkdir path = Mkdir(path) |> call >>= ret_unit in
  let opendir p = Opendir(p) |> call >>=| function
    | Dh dh -> return (i2dh dh)
    | _ -> internal_err @@ "opendir, "^ty_err^" mnfs.39"
  in
  let readdir dh = Readdir(dh2i dh) |> call >>=| function
    | Readdir' (xs,b) -> return (xs,{finished=b})
    | _ -> internal_err @@ "readdir, "^ty_err^" mnfs.43"
  in
  let closedir dh = Closedir(dh2i dh) |> call >>= ret_unit in
  let create path = Create(path) |> call >>= ret_unit in
  let open_ p = Open(p) |> call >>=| function
    | Open' fd -> return (i2fd fd)
    | _ -> internal_err @@ "open_, "^ty_err^" mnfs.49"
  in
  let pread ~fd ~foff ~len ~buf ~boff =
    Pread(fd2i fd,foff,len) |> call >>=| function
    | Pread' data -> 
      blit_data_to_buffer ~data ~buffer:buf ~boff;
      return (data_length data)
    | _ -> internal_err @@ "pread, "^ty_err^" mnfs.56"
  in
  let pwrite ~fd ~foff ~len ~buf ~boff =
    data_of_buffer ~buffer:buf ~off:boff ~len:len |> fun data ->
    Pwrite(fd2i fd,foff,data) |> call >>=| function
    | Int nwritten -> return nwritten
    | _ -> internal_err @@ "pwrite, "^ty_err^" mnfs.62"
  in
  let close fd = Close (fd2i fd) |> call >>= ret_unit in
  let rename src dst = 
    Rename(src,dst) |> call >>= ret_unit in
  let truncate path length = Truncate(path,length) |> call >>= ret_unit in
  let stat p = Stat p |> call >>=| function
    | Stat' st -> return st
    | _ -> internal_err @@ "stat_file, "^ty_err^" mnfs.68"
  in
  let symlink contents path =
    Symlink(contents,path) |> call >>= ret_unit in
  let readlink path =
    Readlink(path) |> call >>=| function
    | Readlink' s -> return s 
    | _ -> internal_err @@ "readlink, "^ty_err^" mnfs.105"
  in
  let reset () = Reset |> call >>= function 
    | Ok_ Unit -> return ()
    | _ -> internal_err @@ "reset, "^ty_err^" mnfs.92"
  in
  let ops : (_,_,_)ops = 
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat; symlink; readlink; reset }
  in
  ops


(** Specialize mk_client_ops using standard buffer auxiliary functions *)
let mk_client_ops = 
  let open Nfs_aux in
  mk_client_ops 
    ~data_length
    ~data_of_buffer
    ~blit_data_to_buffer


let _ = mk_client_ops



(** We also need to implement call: msg_from_client->msg_from_server
   m; here we specialize to state_passing monad, and Unix network
   connections *)
module State_passing_call = struct
  open State_passing
  open Msgs

  (* NOTE specialized to unix impl *)
  module Connection = Tjr_net.Unix_

  let call ~conn (m:msg_from_client) : (msg_from_server,'w state_passing) m = 
    of_fun(fun w -> 
        m |> msg_c_to_string |> fun s -> 
        log_.log_lazy (fun () -> Printf.sprintf "sending %s\n" s);
        s |> Connection.send_string conn 
        |> function Error () -> Base_extra.exit_1 __LOC__ | Ok () -> 
          Connection.recv_string conn 
          |> function Error () -> Base_extra.exit_1 __LOC__ | Ok s -> 
            log_.log_lazy (fun () -> Printf.sprintf "receiving %s\n" s);
            s |> string_to_msg_s |> fun m -> 
            (m,w))
end
