(* log calls and returns -------------------------------------------- *)

open Mini_pervasives
open Minifs
open Msgs

module Make(M:MONAD) = struct

  module M = M

  module Minifs' = Minifs.Make(M)(Minifs.Standard_base_types)
  open Minifs'
  open M

  type 'm log_op = {
    log: 'a. msg_from_client -> 'a m -> 'a m
  }

  let dh' = -99

  let mk_logged_ops (type m) ~log_op ~ops = 
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
        (Pread(fd,foff,length)) 
        (ops.pread ~fd ~foff ~length ~buffer ~boff) 
    in
    let pwrite ~fd ~foff ~length ~buffer ~boff =
      log_op.log 
        (Pwrite(fd,foff,"data???FIXME")) 
        (ops.pwrite ~fd ~foff ~length ~buffer ~boff) 
    in
    let close fd = log_op.log (Close(fd)) (ops.close fd) in
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
