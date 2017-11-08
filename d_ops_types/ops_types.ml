open Base_

(* operations ------------------------------------------------------- *)


(* FIXME the following should be refined *)
type err_ = Error_.exn_
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
type stat_err = err_

(* we often need to map our errors into standard unix errors eg when
   dealing with fuse; in the unix module, we need to construct the
   reverse mapping *)
include struct
  open Unix

  (* Our version of the Unix_error exception; we want to pattern match
     exhaustively and name the type *)
  type unix_error_ = [`Unix_error of error * string * string ]

  let unknown_error = `Unix_error(EUNKNOWNERR 999,"FIXME","FIXME")

(*
  let err_map = [
    `EOTHER, unknown_error;
    `ENOENT, `Unix_error(ENOENT,"FIXME","FIXME")
  ]

  (* FIXME refine following *)
  let err2unix e = 
    try List.assoc e err_map 
    with Not_found -> unknown_error

  let _ = err2unix
*)

  (* type unix_error = Unix_error of error * string * string *)

  (* FIXME note this won't work so well because we are quite specific
     on errors, which means we need to map precisely at each op *)
    (*
  let unix2err (`Unix_error (e,s1,s2)) = 
    try 
      List.find (function (_,`Unix_error(e',_,_)) -> e=e') err_map 
      |> fun (e,_) -> e
    with Not_found -> `EOTHER

  let _ = unix2err
*)
end


module type R = sig
  type ('a,'e)r_  (* result *)
end


(* sig -------------------------------------------------------------- *)

(* NOTE R is expected to be result, but for imperative ops we
   eliminate the 'e component and throw an exception instead *)
module type OPS_TYPE = sig
  include MONAD
  include BASE_TYPES
  include R
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)r_ m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)r_ m;
    opendir : path -> (dh,opendir_err)r_ m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)r_ m;
    closedir : dh -> (unit,closedir_err)r_ m;
    create : parent:path -> name:string -> (unit,create_err)r_ m;
    open_ : path -> (fd,open_err)r_ m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)r_ m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)r_ m;
    close : fd -> (unit,close_err)r_ m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)r_ m;
    truncate : path:path -> length:int -> (unit,truncate_err)r_ m;
    stat : path -> (stat_record,stat_err)r_ m;
    reset : unit -> unit m;
  }
end


type 'a m' = 'a
module type OPS_TYPE_WITHOUT_MONAD = OPS_TYPE with type 'a m = 'a m'

module type OPS_TYPE_WITH_RESULT = OPS_TYPE with type ('a,'e)r_ = ('a,'e)result

type ('a,'e) r' = 'a
module type IMP_OPS_TYPE = 
  OPS_TYPE_WITHOUT_MONAD with type ('a,'e)r_ = ('a,'e)r'

(*
module R_as_result = struct
  type ('a,'e)r_ = ('a,'e)result
end
*)

(* make the sig ----------------------------------------------------- *)


module type MB = sig
  include MONAD
  include BASE_TYPES
end

module type MBR = sig
  include MB
  include R
end



(* FIXME duplication *)
module Make_ops_type(MBR:MBR) = struct
  open MBR
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)r_ m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)r_ m;
    opendir : path -> (dh,opendir_err)r_ m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)r_ m;
    closedir : dh -> (unit,closedir_err)r_ m;
    create : parent:path -> name:string -> (unit,create_err)r_ m;
    open_ : path -> (fd,open_err)r_ m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)r_ m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)r_ m;
    close : fd -> (unit,close_err)r_ m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)r_ m;
    truncate : path:path -> length:int -> (unit,truncate_err)r_ m;
    stat : path -> (stat_record,stat_err)r_ m;
    reset : unit -> unit m;
  }
end



(* logging via log_op.log -------------------------------------------- *)

(* FIXME this is very similar to nfs_server because we are wrapping an
   existing ops in marshalling code *)
module Make_logged_ops(O:OPS_TYPE) = struct
  open O
  open Msgs

  type log_op = {
    log: 'a. msg_from_client -> 'a m -> 'a m
  }

  let mk_logged_ops ~log_op ~ops ~fd2i ~dh2i = 
    let root = ops.root in
    let unlink ~parent ~name = 
      log_op.log (Unlink(parent,name)) (ops.unlink ~parent ~name) in
    let mkdir ~parent ~name = 
      log_op.log (Mkdir(parent,name)) (ops.mkdir ~parent ~name) in
    let opendir p = log_op.log (Opendir(p)) (ops.opendir p) in
    let readdir dh = log_op.log (Readdir(dh2i dh)) (ops.readdir dh) in 
    let closedir dh = log_op.log (Closedir(dh2i dh)) (ops.closedir dh) in
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
    let stat path = log_op.log (Stat(path)) (ops.stat path) in
    let reset = ops.reset in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat; reset }   
end
