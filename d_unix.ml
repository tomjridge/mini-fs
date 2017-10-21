



open C_base 





module Base_types = Unix_base_types 






open Base_types

module Mem_monad = C_in_mem.Monad

module Lwt_monad = struct
  type 'a m = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
end

module Unix_monad = struct
  type 'a m
  let return : 'a -> 'a m = fun x -> failwith "FIXME"
  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x -> failwith "FIXME"
end

module Abstract_monad = struct
  type 'a m
  let return : 'a -> 'a m = fun x -> failwith "FIXME"
  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x -> failwith "FIXME"
end  







module Monad = Unix_monad






  open Monad

  type ops = {
    root: path;
    unlink : parent:path -> name:string -> unit m;
    mkdir : parent:path -> name:string -> unit m;
    opendir : path -> dh m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished) m;
    closedir : dh -> unit m;
    create : parent:path -> name:string -> unit m;
    open_ : path -> fd m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m;
    close : fd -> unit m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> unit m;
    truncate : path:path -> length:int -> unit m;
    stat_file : path -> file_stat m;
    kind : path -> st_kind m;
    reset : unit -> unit m;
  }

  (* convert to logged ops *)
  type 'm log_op = {
    log: 'a. C_msgs.msg_from_client -> 'a m -> 'a m
  }

  let dh' = -99

  let mk_logged_ops (type m) ~log_op ~ops = 
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
        (Pread(fd|>fd2int,foff,length)) 
        (ops.pread ~fd ~foff ~length ~buffer ~boff) 
    in
    let pwrite ~fd ~foff ~length ~buffer ~boff =
      log_op.log 
        (Pwrite(fd|>fd2int,foff,"data???FIXME")) 
        (ops.pwrite ~fd ~foff ~length ~buffer ~boff) 
    in
    let close fd = log_op.log (Close(fd|>fd2int)) (ops.close fd) in
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
    




  module Imp = struct

    type run = {
      run:'a. 'a m -> 'a
    }

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


    let mk_imperative_ops ~(ops:ops) ~run =
      let run f = 
        try run.run f 
        with e -> (Printexc.to_string e |> print_endline; raise e) 
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
    let reset=(fun () -> run @@ ops.reset ()) in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat_file; kind; reset }


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




