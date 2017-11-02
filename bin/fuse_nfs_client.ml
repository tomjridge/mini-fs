(* a fuse filesystem that mounts a remote filesystem *)

(* we follow nfs_client, but wrap in a fuse binding *)

open Tjr_connection
open Tjr_minifs
open C_base
open H_nfs_client_common

let quad = M_runtime_config.get_config ~filename:"config.json" @@ 
  fun ~client ~server -> client


(* put all state in this module *)
let conn = 
  Connection.connect ~quad
  |> function 
  | Ok fd -> fd
  | Error e -> 
    exit_1 __LOC__

let call = call ~conn

let client_ops = E_in_mem.(
    Client'.mk_client_ops
      ~extra_ops
      ~call
      ~i2dh ~dh2i
      ~i2fd ~fd2i)

let w : w ref = ref (init_w conn)

let run = Imp_ops_type.{run=(fun a -> H_nfs_client_common.run ~w_ref:w a)}

let imp_ops = 
  Imp_ops_type.mk_imperative_ops
    ~ops:client_ops
    ~run

let _ = imp_ops

open Imp_ops_type

module Fuse' = G_fuse_common.Make_fuse(Imp_ops_type_plus)
open Fuse'
let fuse_ops = mk_fuse_ops imp_ops
let _ = fuse_ops

let main () = 
  print_endline "Fuse_nfs_client starts";
  Fuse.main Sys.argv fuse_ops

let _ = main()






