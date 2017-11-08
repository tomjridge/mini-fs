(* a fuse filesystem that mounts a remote filesystem via nfs_client *)

(* we follow nfs_client, but wrap in a fuse binding *)

open Tjr_connection
open Tjr_minifs
open Base_
open Ops_types

(* set up nfs client ------------------------------------------------ *)

(* we want to use the step_monad with the world type just unit, with
   simple (int) base types, and result *)

module World : sig type w val init_w:w end = struct
  type w = unit
  let init_w = ()
end
open World

module Monad = struct
  open Step_monad
  type 'a m = ('a,w)step_monad
  let bind,return = bind,return
end
open Monad

module MBR = struct
  include Monad
  include In_mem.Mem_base_types
  include R_as_result
end
  
module Ops_type = Make_ops_type(MBR)
module Ops_type_plus = struct include MBR include Ops_type end

module Fuse_nfs_client = Fuse_nfs.Make_fuse_nfs_client(Ops_type_plus)


(* establish the connection to the server *)
include struct 
  module Connection = Tjr_connection.Unix_

  let quad = Runtime_config.get_config ~filename:"config.json" @@ 
    fun ~client ~server ~log_everything -> client


  (* FIXME put all state in this module? *)
  let conn = 
    Connection.connect ~quad
    |> function 
    | Ok fd -> fd
    | Error e -> 
      failwith __LOC__


  let call = Nfs_client.Step_monad_call.call ~conn
end



(* need co_eta for Step_monad *)
include struct 
  open Fuse_nfs_client.Fuse'
  let co_eta = fun a -> 
    Step_monad.run ~dest_exceptional:(fun x -> None) init_w a |> function
    | Ok(w,a) -> a  (* NOTE can ignore the state - it doesn't change *)
    | Error (`Attempt_to_step_exceptional_state _) -> failwith __LOC__
  let co_eta = {co_eta}
end


open Fuse_nfs_client

let fuse_ops = 
  let open Int_base_types in
  let internal_marshal_err = { internal_marshal_err=fun s -> failwith s } in
  mk_fuse_ops 
    ~internal_marshal_err ~call 
    ~co_eta
    ~fd2i ~i2fd ~dh2i ~i2dh

let _ : Fuse.operations = fuse_ops

let main () = 
  print_endline "Fuse_nfs_client starts";
  Fuse.main Sys.argv fuse_ops

let _ = main()

