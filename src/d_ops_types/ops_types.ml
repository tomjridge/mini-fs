(* FIXME rename this file *)

open Tjr_monad.Monad
open Base_

open R_


(* sig -------------------------------------------------------------- *)

open Error_types
open Ops_type_


module type OPS_TYPE_WITH_RESULT = OPS_TYPE with type ('a,'e)r_ = ('a,'e)result

type ('a,'e) r' = 'a


(* make the sig ----------------------------------------------------- *)

(* FIXME duplication *)
module Make_ops_type(R:R) = struct
  include R
  type ('fd,'dh,'w) ops = {
    root: path;
    unlink : path -> ((unit,unlink_err)r_,  'w) m;
    mkdir : path -> ((unit,mkdir_err)r_,  'w) m;
    opendir : path -> (('dh,opendir_err)r_,  'w) m;
    (* . and .. are returned *)
    readdir : 'dh -> ((string list * is_finished,readdir_err)r_,  'w) m;
    closedir : 'dh -> ((unit,closedir_err)r_,  'w) m;
    create : path -> ((unit,create_err)r_,  'w) m;
    open_ : path -> (('fd,open_err)r_,  'w) m;
    pread: 
      fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      ((int,pread_err)r_,  'w) m; 
    pwrite: 
      fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      ((int,pwrite_err)r_,  'w) m;
    close : 'fd -> ((unit,close_err)r_,  'w) m;
    rename: path -> path -> ((unit,rename_err)r_,  'w) m;
    truncate : path:path -> length:int -> ((unit,truncate_err)r_,  'w) m;
    stat : path -> ((stat_record,stat_err)r_,  'w) m;
    reset : unit -> (unit,  'w) m;
  }
end



module Ops_type_with_result = Make_ops_type(R_is_result)


(* logging via log_op.log -------------------------------------------- *)

(* FIXME resurrect this

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
    let unlink path = 
      log_op.log (Unlink(path)) (ops.unlink path) in
    let mkdir path = 
      log_op.log (Mkdir(path)) (ops.mkdir path) in
    let opendir p = log_op.log (Opendir(p)) (ops.opendir p) in
    let readdir dh = log_op.log (Readdir(dh2i dh)) (ops.readdir dh) in 
    let closedir dh = log_op.log (Closedir(dh2i dh)) (ops.closedir dh) in
    let create path = 
      log_op.log (Create(path)) (ops.create path) in
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
    let rename path path'  = 
      log_op.log 
        (Rename(path,path')) 
        (ops.rename path path') 
    in
    let truncate ~path ~length = 
      log_op.log (Truncate(path,length)) (ops.truncate ~path ~length) in
    let stat path = log_op.log (Stat(path)) (ops.stat path) in
    let reset = ops.reset in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat; reset }   
end

*)
