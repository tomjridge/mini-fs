open C_base
(* operations ------------------------------------------------------- *)

(* FIXME the following should be refined *)
type err_ = [ `EOTHER ]
type unlink_err = err_
type mkdir_err = err_
type opendir_err = err_
type readdir_err = err_
type closedir_err = err_
type create_err = err_
type open_err = err_
type pread_err = err_
type pwrite_err = err_
type close_err = err_ (* EBADF, but for valid fd, fd will be closed *)
type rename_err = err_
type truncate_err = err_
type stat_file_err = err_
type kind_err = err_

module type OPS_TYPE = sig
  include MONAD
  include BASE_TYPES
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)result m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)result m;
    opendir : path -> (dh,opendir_err)result m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)result m;
    closedir : dh -> (unit,closedir_err)result m;
    create : parent:path -> name:string -> (unit,create_err)result m;
    open_ : path -> (fd,open_err)result m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)result m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)result m;
    close : fd -> (unit,close_err)result m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)result m;
    truncate : path:path -> length:int -> (unit,truncate_err)result m;
    stat_file : path -> (file_stat,stat_file_err)result m;
    kind : path -> (st_kind,kind_err)result m;
    reset : unit -> unit m;
  }

  type 'm log_op = {
    log: 'a. C_msgs.msg_from_client -> 'a m -> 'a m
  }
end

module Make_ops_type(M:MONAD)(B:BASE_TYPES) = struct
  open M
  open B
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)result m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)result m;
    opendir : path -> (dh,opendir_err)result m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)result m;
    closedir : dh -> (unit,closedir_err)result m;
    create : parent:path -> name:string -> (unit,create_err)result m;
    open_ : path -> (fd,open_err)result m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)result m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)result m;
    close : fd -> (unit,close_err)result m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)result m;
    truncate : path:path -> length:int -> (unit,truncate_err)result m;
    stat_file : path -> (file_stat,stat_file_err)result m;
    kind : path -> (st_kind,kind_err)result m;
    reset : unit -> unit m;
  }

  type 'm log_op = {
    log: 'a. C_msgs.msg_from_client -> 'a m -> 'a m
  }

  let dh' = -99

  let mk_logged_ops (type m) ~log_op ~ops ~fd2i = 
    let open C_msgs in
    let root = ops.root in
    let unlink ~parent ~name = 
      log_op.log (Unlink(parent,name)) (ops.unlink ~parent ~name) in
    let mkdir ~parent ~name = 
      log_op.log (Mkdir(parent,name)) (ops.mkdir ~parent ~name) in
    let opendir p = log_op.log (Opendir(p)) (ops.opendir p) in
    let readdir dh = log_op.log (Readdir(dh')) (ops.readdir dh) in 
    let closedir dh = log_op.log (Closedir(dh')) (ops.closedir dh) in
    let create ~parent ~name = 
      log_op.log (Create(parent,name)) (ops.create ~parent ~name) in
    let open_ p = log_op.log (Open(p)) (ops.open_ p) in
    let pread ~fd ~foff ~length ~buffer ~boff =
      log_op.log 
        (Pread(fd|>fd2i,foff,length)) 
        (ops.pread ~fd ~foff ~length ~buffer ~boff) 
    in
    let pwrite ~fd ~foff ~length ~buffer ~boff =
      log_op.log 
        (Pwrite(fd|>fd2i,foff,"data???FIXME")) 
        (ops.pwrite ~fd ~foff ~length ~buffer ~boff) 
    in
    let close fd = log_op.log (Close(fd|>fd2i)) (ops.close fd) in
    let rename ~spath ~sname ~dpath ~dname = 
      log_op.log 
        (Rename(spath,sname,dpath,dname)) 
        (ops.rename ~spath ~sname ~dpath ~dname) 
    in
    let truncate ~path ~length = 
      log_op.log (Truncate(path,length)) (ops.truncate ~path ~length) in
    (* FIXME log_op.log the rest as well *)
    let stat_file path = log_op.log (Stat_file(path)) (ops.stat_file path) in
    let kind path = log_op.log (Kind(path)) (ops.kind path) in
    let reset = ops.reset in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat_file; kind; reset }

end


(* imperative operations -------------------------------------------- *)

module type IMP_OPS_TYPE = sig
  include MONAD
  include BASE_TYPES
  type imp_ops = {
    root: path;
    unlink : parent:path -> name:string -> unit;
    mkdir : parent:path -> name:string -> unit;
    opendir : path -> dh;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished);
    closedir : dh -> unit;
    create : parent:path -> name:string -> unit;
    open_ : path -> fd;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int;
    close : fd -> unit;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> unit;
    truncate : path:path -> length:int -> unit;
    stat_file : path -> file_stat;
    kind : path -> st_kind;
    reset : unit -> unit;
  }

  type run = {
    run:'a 'e. ('a,'e)result m -> 'a
  }
end

      

module Make_imp_ops_type(O:OPS_TYPE) = struct
  open O
  type imp_ops = {
    root: path;
    unlink : parent:path -> name:string -> unit;
    mkdir : parent:path -> name:string -> unit;
    opendir : path -> dh;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished);
    closedir : dh -> unit;
    create : parent:path -> name:string -> unit;
    open_ : path -> fd;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int;
    close : fd -> unit;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> unit;
    truncate : path:path -> length:int -> unit;
    stat_file : path -> file_stat;
    kind : path -> st_kind;
    reset : unit -> unit;
  }


  (* NOTE the imperative operations may throw exceptions; if this
     happens, it is perhaps not clear what the "state of the world"
     (maybe captured in a reference) should be *)
  type run = {
    run:'a 'e. ('a,'e)result m -> 'a
  }

  (* NOTE this at least logs any exceptions that are thrown *)
  let mk_imperative_ops ~(ops:O.ops) ~run =
    let run f = 
      try run.run f 
      with e -> (
          log_.log_lazy (fun () -> Printexc.to_string e); 
          raise e) 
    in
    let root= ops.root in
    let unlink=(fun ~parent ~name -> run @@ ops.unlink ~parent ~name) in
    let mkdir=(fun ~parent ~name -> run @@ ops.mkdir ~parent ~name) in
    let opendir=(fun p -> run @@ ops.opendir p) in
    let readdir=(fun dh -> run @@ ops.readdir dh) in
    let closedir=(fun dh -> run @@ ops.closedir dh) in
    let create=(fun ~parent ~name -> run @@ ops.create ~parent ~name) in
    let open_=(fun path -> run @@ ops.open_ path) in
    let pread=(fun ~fd ~foff ~length ~buffer ~boff -> 
        run @@ ops.pread ~fd ~foff ~length ~buffer ~boff) in
    let pwrite=(fun ~fd ~foff ~length ~buffer ~boff -> 
        run @@ ops.pwrite ~fd ~foff ~length ~buffer ~boff) in
    let close=(fun fd -> run @@ ops.close fd) in
    let rename=(fun ~spath ~sname ~dpath ~dname -> 
        run @@ ops.rename ~spath ~sname ~dpath ~dname) in
    let truncate=(fun ~path ~length -> run @@ ops.truncate ~path ~length) in
    let stat_file=(fun path -> run @@ ops.stat_file path) in
    let kind=(fun path -> run @@ ops.kind path) in
    let reset=(fun () -> run @@ bind (ops.reset ()) (fun x -> return (Ok x)) ) in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat_file; kind; reset }

end


module Make_readdir'(I:IMP_OPS_TYPE) = struct
  open I
  (* for small directories *)
  let readdir' ~ops = 
    fun path ->
      let dh = ops.opendir path in
      let es = ref [] in
      let finished = ref false in
      while(not !finished) do
        let (es',f) = ops.readdir dh in
        es:=!es@es';
        finished:=f;
      done;
      ops.closedir dh;
      !es
end
