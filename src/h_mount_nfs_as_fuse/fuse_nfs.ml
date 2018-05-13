(* mount nfs via fuse ----------------------------------------------- *)

(* This combines fuse with nfs client functionality (server does not
   need fuse) *)

open Base_
open Ops_type_with_result

(* NOTE nfs client ops could be in unix.m or lwt.m, although probably
   ocamlfuse does not work properly with lwt *)

(* Steps:
   - construct client
   - wrap with fuse

   Essentially we compose the mk... functions, given some background
   types.
*)


(* construct types for the nfs client *)
module Client' = Nfs_client.Make_client(O)  
include Client'

let _ = mk_client_ops

(* construct fuse from imperative operations *)
module Fuse' = Fuse_.Make_fuse(O)


(* call Fuse_common.mk_fuse_ops with nfs client ops *)
let mk_fuse_nfs_ops (* was mk_fuse_ops *)
    ~internal_marshal_err
    ~call
    ~i2dh ~dh2i
    ~i2fd ~fd2i
    ~co_eta
  = 
  let nfs_ops = mk_client_ops ~internal_marshal_err ~call ~i2dh ~dh2i ~i2fd ~fd2i in
  let ops = nfs_ops in
  Fuse'.mk_fuse_ops
    ~ops
    ~co_eta

