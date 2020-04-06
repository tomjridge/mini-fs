(* a fuse filesystem that mounts a remote filesystem via nfs_client *)

(* we follow nfs_client, but wrap in a fuse binding *)
open Tjr_monad
open Tjr_minifs

(* monad ------------------------------------------------------------ *)

let monad_ops = State_passing.monad_ops ()


(* set up nfs client ------------------------------------------------ *)

(* establish the connection to the server *)
module Connection = Tjr_net.Unix_

let endpt_pair = Runtime_config.server_endpt_pair


(* FIXME put all state in this module? FIXME the following prevents
   ./xxx --help from displaying FUSE help *)
let conn = 
  Connection.connect endpt_pair
  |> function 
  | Ok fd -> fd
  | Error _e -> 
    failwith __LOC__

let call = Nfs_client.State_passing_call.call ~conn

let internal_marshal_err = Nfs_client.{ internal_marshal_err=fun s -> failwith s }


open Int_base_types

let nfs_ops = Nfs_client.mk_client_ops 
    ~monad_ops
    ~internal_marshal_err
    ~call
    ~fd2i ~i2fd ~dh2i ~i2dh
    
let _ = nfs_ops


(* set up fuse ------------------------------------------------------ *)

let run ~init_state m = State_passing.to_fun m init_state

let co_eta = fun a -> 
  run ~init_state: () a |> function
  | a,_ -> a  (* NOTE can ignore the state - it doesn't change *)
let co_eta = {co_eta}


let fuse_ops = 
  Fuse_.mk_fuse_ops
    ~monad_ops
    ~ops:nfs_ops
    ~co_eta

let _ : Fuse.operations = fuse_ops

let main () = 
  print_endline "Fuse_nfs_client starts";
  Fuse.main Sys.argv fuse_ops

let _ = main()

