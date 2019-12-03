(* mount nfs via fuse ----------------------------------------------- *)

(* This combines fuse with nfs client functionality (server does not
   need fuse) *)

(* NOTE nfs client ops could be in unix.m or lwt.m, although probably
   ocamlfuse does not work properly with lwt *)

(* Steps:
   - construct client
   - wrap with fuse

   Essentially we compose the mk... functions, given some background
   types.
*)

open Minifs_intf

(* open Nfs_client *)
let mk_client_ops = Nfs_client.mk_client_ops

(* open Fuse_ *)
let mk_fuse_ops = Fuse_.mk_fuse_ops

let mk_fuse_nfs_ops 
  ~monad_ops
    ~internal_marshal_err
    ~call
    ~i2dh ~dh2i
    ~i2fd ~fd2i
    ~co_eta
  = 
  let nfs_ops = 
    mk_client_ops ~monad_ops ~internal_marshal_err ~call ~i2dh ~dh2i ~i2fd ~fd2i 
  in
  let ops = nfs_ops in
  mk_fuse_ops
    ~monad_ops
    ~ops
    ~co_eta

let _ : 
  monad_ops:'a monad_ops ->
  internal_marshal_err:'a Nfs_client.internal_marshal_err ->
  call:(Msgs.msg_from_client -> (Msgs.msg_from_server, 'a) m) ->
  i2dh:(Msgs.dh -> 'b) ->
  dh2i:('b -> Msgs.dh) ->
  i2fd:(Msgs.fd -> 'c) ->
  fd2i:('c -> Msgs.fd) -> co_eta:'a Fuse_.co_eta -> Fuse.operations
  = mk_fuse_nfs_ops
