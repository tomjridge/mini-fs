(* mount nfs via fuse ----------------------------------------------- *)

open C_base
open D_functors

(* nfs client ops could be in unix.m or lwt.m; fuse takes an
   imperative ops which is expected to raise unix exceptions *)


module Make_fuse_nfs_client(M:MONAD)(B:BASE_TYPES) = struct

  (* construct the type of operations *)
  module Ops_type = D_functors.Make_ops_type(M)(B)

  open Ops_type
      
  module Ops_type_plus = struct
    include M
    include B
    include Ops_type
  end

  (* construct the nfs client *)
  module Client' = G_nfs_client.Make_client(Ops_type_plus)  
  include Client'

  let _ = mk_client_ops


  (* construct (type of) an imperative version of the nfs client operations *)
  module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus)
  type imp_ops = Imp_ops_type.imp_ops

  module Imp_ops_type_plus = struct
    include M
    include B
    include Imp_ops_type
  end

  (* construct fuse from imperative operations *)
  module Fuse' = G_fuse_common.Make_fuse(Imp_ops_type_plus)

  let _ : imp_ops -> Fuse.operations = Fuse'.mk_fuse_ops

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

  let mk_fuse_ops 
    ~extra_ops
    ~call
    ~i2dh ~dh2i
    ~i2fd ~fd2i
    ~run    
    = 
    mk_imperative_ops
    ~extra_ops
    ~call
    ~i2dh ~dh2i
    ~i2fd ~fd2i
    ~run    
    |> Fuse'.mk_fuse_ops

end
