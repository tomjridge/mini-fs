(* client ----------------------------------------------------------- *)

(* the client makes network calls, which potentially could be in lwt.m
   or unix.m; so we parameterize over ops *)
open C_base
open D_functors

module Make_client(O:OPS_TYPE) = struct
  open O

  (* this is used to indicate that the result of a call was not what
     was expected *)
  type extra_ops = {
    internal_marshal_err: 'a. string -> 'a m;
  }
  
  open C_msgs

  (* construct message, send, recv *)
  let mk_client_ops (type t) 
      ~extra_ops
      (* NOTE call returns errors in the monad *)
      ~(call:msg_from_client -> msg_from_server' m)
      ~data_length
      ~data_of_buffer
      ~blit_data_to_buffer
      ~i2dh ~dh2i
      ~i2fd ~fd2i
    = 
    let ( >>= ) = bind in  
    let internal_err = extra_ops.internal_marshal_err in
    let ty_err = "incorrect return return type from server" in
    let root : path = "/" in
    let ret_unit : msg_from_server' -> unit m = function
      | Unit -> return ()
      | _ -> internal_err @@ "ret_unit, "^ty_err^" mnfs.32"
    in
    let unlink ~parent ~name = Unlink(parent,name) |> call >>= ret_unit in
    let mkdir ~parent ~name = Mkdir(parent,name) |> call >>= ret_unit in
    let opendir p = Opendir(p) |> call >>=function
      | Dh dh -> return (i2dh dh)
      | _ -> internal_err @@ "opendir, "^ty_err^" mnfs.39"
    in
    let readdir dh = Readdir(dh2i dh) |> call >>= function
      | Readdir' (xs,b) -> return (xs,b)
      | _ -> internal_err @@ "readdir, "^ty_err^" mnfs.43"
    in
    let closedir dh = Closedir(dh2i dh) |> call >>= ret_unit in
    let create ~parent ~name = Create(parent,name) |> call >>= ret_unit in
    let open_ p = Open(p) |> call >>= function
      | Open' fd -> return (i2fd fd)
      | _ -> internal_err @@ "open_, "^ty_err^" mnfs.49"
    in
    let pread ~fd ~foff ~length ~buffer ~boff =
      Pread(fd2i fd,foff,length) |> call >>= function
      | Pread' data -> 
        blit_data_to_buffer ~data ~buffer ~boff;
        return (data_length data)
      | _ -> internal_err @@ "pread, "^ty_err^" mnfs.56"
    in
    let pwrite ~fd ~foff ~length ~buffer ~boff =
      data_of_buffer ~buffer ~off:boff ~len:length |> fun data ->
      Pwrite(fd2i fd,foff,data) |> call >>= function
      | Int nwritten -> return nwritten
      | _ -> internal_err @@ "pwrite, "^ty_err^" mnfs.62"
    in
    let close fd = Close (fd2i fd) |> call >>= ret_unit in
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

    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat_file; kind; reset }


  include struct
    open G_nfs_aux
    (* specialize mk_client_ops *)
    let mk_client_ops = mk_client_ops 
        ~data_length
        ~data_of_buffer
        ~blit_data_to_buffer
  end


  let _ = mk_client_ops

end
