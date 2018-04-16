(* client ----------------------------------------------------------- *)

(* the client makes network calls, which potentially could be in lwt.m
   or unix.m; so we parameterize over ops *)
open Base_
open Ops_types

module Make_client(O:OPS_TYPE_WITH_RESULT) = struct
  open O

  (* this is used to indicate that the result of a call was not what
     was expected *)
  type internal_marhsal_err = {
    internal_marshal_err: 'a. string -> 'a m;
  }
  
  open Msgs

  (* construct message, send, recv *)
  let mk_client_ops (type t) 
      ~internal_marshal_err
      (* NOTE call returns errors in the monad *)
      ~(call:msg_from_client -> msg_from_server m)
      ~data_length
      ~data_of_buffer
      ~blit_data_to_buffer
      ~i2dh ~dh2i
      ~i2fd ~fd2i
    = 
    (* internal marshalling errors *)
    let internal_err = internal_marshal_err.internal_marshal_err in
    let ty_err = "incorrect return return type from server" in

    let ( >>= ) = bind in  

    (* NOTE after we use call m, we are left with a msg_from_server m;
       we then need a map from msg_from_server' to 'a to get a
       ('a,exn_)result m via fmap_error *)
    (* but we also want to trap errors in the protocol, and for this
       we need the more general version *)
    let fmap_error : 'a. msg_from_server m -> (msg_from_server' -> 'a m) -> ('a,exn_)result m = fun m f -> 
      m >>= function
      | Ok_ m' -> f m' >>= fun a -> return (Ok a)
      | Error_ e -> return (Error e)
    in

    let ( >>=| ) m f = fmap_error m f in

    let ret_unit : msg_from_server -> (unit,exn_)result m = fun m ->
      return m >>=| function 
      | Unit -> return () 
      | _ -> internal_err @@ "opendir, "^ty_err^" mnfs.52" 
    in

    let root : path = "/" in

    let unlink ~parent ~name = Unlink(parent,name) |> call >>= ret_unit in
    let mkdir ~parent ~name = Mkdir(parent,name) |> call >>= ret_unit in
    let opendir p = Opendir(p) |> call >>=| function
      | Dh dh -> return (i2dh dh)
      | _ -> internal_err @@ "opendir, "^ty_err^" mnfs.39"
    in
    let readdir dh = Readdir(dh2i dh) |> call >>=| function
      | Readdir' (xs,b) -> return (xs,b)
      | _ -> internal_err @@ "readdir, "^ty_err^" mnfs.43"
    in
    let closedir dh = Closedir(dh2i dh) |> call >>= ret_unit in
    let create ~parent ~name = Create(parent,name) |> call >>= ret_unit in
    let open_ p = Open(p) |> call >>=| function
      | Open' fd -> return (i2fd fd)
      | _ -> internal_err @@ "open_, "^ty_err^" mnfs.49"
    in
    let pread ~fd ~foff ~length ~buffer ~boff =
      Pread(fd2i fd,foff,length) |> call >>=| function
      | Pread' data -> 
        blit_data_to_buffer ~data ~buffer ~boff;
        return (data_length data)
      | _ -> internal_err @@ "pread, "^ty_err^" mnfs.56"
    in
    let pwrite ~fd ~foff ~length ~buffer ~boff =
      data_of_buffer ~buffer ~off:boff ~len:length |> fun data ->
      Pwrite(fd2i fd,foff,data) |> call >>=| function
      | Int nwritten -> return nwritten
      | _ -> internal_err @@ "pwrite, "^ty_err^" mnfs.62"
    in
    let close fd = Close (fd2i fd) |> call >>= ret_unit in
    let rename ~spath ~sname ~dpath ~dname = 
      Rename(spath,sname,dpath,dname) |> call >>= ret_unit in
    let truncate ~path ~length = Truncate(path,length) |> call >>= ret_unit in
    let stat p = Stat p |> call >>=| function
      | Stat' st -> return st
      | _ -> internal_err @@ "stat_file, "^ty_err^" mnfs.68"
    in
    let reset () = Reset |> call >>= function 
      | Ok_ Unit -> return ()
      | _ -> internal_err @@ "reset, "^ty_err^" mnfs.92"
    in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat; reset }


  include struct
    open Nfs_aux
    (* specialize mk_client_ops *)
    let mk_client_ops = mk_client_ops 
        ~data_length
        ~data_of_buffer
        ~blit_data_to_buffer
  end


  let _ = mk_client_ops

end




(* we also need to implement call: msg_from_client->msg_from_server m;
   here we specialize to step_monad *)
module Step_monad_call = struct
  open Tjr_either
  open Step_monad
  open Msgs

  (* NOTE specialized to unix impl *)
  module Connection = Tjr_connection.Unix_

  type ('a,'w)m = ('a,'w)step_monad
  let call ~conn (m:msg_from_client) : (msg_from_server,'w) m = 
    Step(fun w -> 
        m |> msg_c_to_string |> fun s -> 
        log_.log_lazy (fun () -> Printf.sprintf "sending %s\n" s);
        s |> Connection.send_string ~conn 
        |> function Error () -> exit_1 __LOC__ | Ok () -> 
          Connection.recv_string ~conn 
          |> function Error () -> exit_1 __LOC__ | Ok s -> 
            log_.log_lazy (fun () -> Printf.sprintf "receiving %s\n" s);
            s |> string_to_msg_s |> fun m -> 
            (w,Inl m))

end


