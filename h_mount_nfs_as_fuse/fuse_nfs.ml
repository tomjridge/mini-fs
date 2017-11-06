(* mount nfs via fuse ----------------------------------------------- *)

(* This combines fuse with nfs client functionality (server does not
   need fuse) *)

open Base_
open Ops_types

(* NOTE nfs client ops could be in unix.m or lwt.m, although probably
   ocamlfuse does not work properly with lwt *)

module Make_fuse_nfs_client(O:OPS_TYPE_WITH_RESULT) = struct

  (* Steps:
     - construct client
     - wrap with fuse

     Essentially we compose the mk... functions, given some background
     types.
  *)


  (* construct types for the nfs client *)
  include struct
    module Client' = Nfs_client.Make_client(O)  
    include Client'

    let _ = mk_client_ops
  end


  (* construct fuse from imperative operations *)
  include struct
    module Fuse' = Fuse_.Make_fuse(O)
  end


  module Readdir' = Readdir'.Make_readdir'(O)
  let readdir' = Readdir'.readdir'


  (* call Fuse_common.mk_fuse_ops with nfs client ops *)
  let mk_fuse_ops 
    ~extra_ops
    ~call
    ~i2dh ~dh2i
    ~i2fd ~fd2i
    ~co_eta
    = 
    let nfs_ops = mk_client_ops ~extra_ops ~call ~i2dh ~dh2i ~i2fd ~fd2i in
    let ops = nfs_ops in
    Fuse'.mk_fuse_ops
      ~readdir':(readdir' ~ops)
      ~ops
      ~co_eta

end



    (* let _ : imp_ops -> Fuse.operations = Fuse'.mk_fuse_ops

       let mk_imperative_ops
       ~extra_ops
       ~call
       ~i2dh ~dh2i
       ~i2fd ~fd2i
       ~run
       = Imp_ops_type.mk_imperative_ops
        ~ops:(mk_client_ops ~extra_ops ~call ~i2dh ~dh2i ~i2fd ~fd2i)
        ~run

       let _ = mk_imperative_ops
    *)



  (* construct (type of) an imperative version of the nfs client operations *)
(*
  module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus)
  type imp_ops = Imp_ops_type.imp_ops

  module Imp_ops_type_plus = struct
    include M
    include B
    include Imp_ops_type
  end
*)

