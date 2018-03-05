
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
