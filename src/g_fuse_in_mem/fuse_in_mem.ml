(* mount an in-mem fs via fuse -------------------------------------- *)

open Base_
open In_mem
open Fuse_
open Tjr_monad

let mk_unix_exn = Error_.mk_unix_exn

(* NOTE could have multiple filesystems in memory, but here we just
   fix one reference which points to the state of one in-mem fs *)
let w_ = ref In_mem.init_t

let run ~init_state m = State_passing.to_fun m init_state

let co_eta = fun a -> run ~init_state:!w_ a |> function
  | a,w -> 
    match w.internal_error_state with
    | None -> (
        w_:=w;
        a)
    | Some s -> (
        log_.log_now s;
        exit_1 __LOC__)

let co_eta = { co_eta }

let monad_ops = State_passing.monad_ops ()

let fuse_ops = mk_fuse_ops ~monad_ops ~ops:In_mem.in_mem_state_passing_ops ~co_eta

let _ : Fuse.operations = fuse_ops

