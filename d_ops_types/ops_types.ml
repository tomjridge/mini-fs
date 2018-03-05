(* FIXME rename this file *)

open Base_

open R_


(* sig -------------------------------------------------------------- *)

open Error_types
open Ops_type_


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
