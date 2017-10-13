(* log calls and returns -------------------------------------------- *)

open Mini_pervasives
open Minifs
open Msgs

type 'm log_op = {
  log: 'a. msg_from_client -> ('a,'m) m_ -> ('a,'m) m_
}

let dh' = -99

let mk_logged_ops (type m) ~log_op ~ops = 
  dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->
  assert(wf_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset);
  
  let unlink ~parent ~name = 
    log_op.log (Unlink(parent,name)) (unlink ~parent ~name) in
  let mkdir ~parent ~name = log_op.log (Mkdir(parent,name)) (mkdir ~parent ~name) in
  let opendir p = log_op.log (Opendir(p)) (opendir p) in
  let readdir dh = log_op.log (Readdir(dh')) (readdir dh) in 
  let closedir dh = log_op.log (Closedir(dh')) (closedir dh) in
  let create ~parent ~name = 
    log_op.log (Create(parent,name)) (create ~parent ~name) in
  let open_ p = log_op.log (Open(p)) (open_ p) in
  let pread ~fd ~foff ~length ~buffer ~boff =
    log_op.log (Pread(fd,foff,length)) (pread ~fd ~foff ~length ~buffer ~boff) in
  let pwrite ~fd ~foff ~length ~buffer ~boff =
    log_op.log 
      (Pwrite(fd,foff,"data???FIXME")) 
      (pwrite ~fd ~foff ~length ~buffer ~boff) 
  in
  let close fd = log_op.log (Close(fd)) (close fd) in
  let rename ~spath ~sname ~dpath ~dname = 
    log_op.log 
      (Rename(spath,sname,dpath,dname)) 
      (rename ~spath ~sname ~dpath ~dname) 
  in
  let truncate ~path ~length = 
    log_op.log (Truncate(path,length)) (truncate ~path ~length) in
  (* FIXME log_op.log the rest as well *)
  let stat_file path = log_op.log (Stat_file(path)) (stat_file path) in
  let kind path = log_op.log (Kind(path)) (kind path) in
  Minifs.mk_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset
