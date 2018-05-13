(* mount an in-mem fs via fuse -------------------------------------- *)

open Base_
open In_mem
open Fuse_

let mk_unix_exn = Error_.mk_unix_exn

(* NOTE could have multiple filesystems in memory, but here we just
   fix one reference which points to the state of one in-mem fs *)
let w_ = ref In_mem.init_t

let co_eta = fun a -> In_mem_monad.run (!w_) a |> function
  | w,Error `Attempt_to_step_halted_state -> failwith __LOC__
  | w,Ok a -> 
    w_:=w;
    a

let co_eta = { co_eta }

let fuse_ops = mk_fuse_ops ~ops ~co_eta

let _ : Fuse.operations = fuse_ops


(* old -------------------------------------------------------------- *)

(*
include struct
  open Unix
  open Imp_ops_type
  (* similar to E_in_mem.imp_run, but translate errors *)

  let run ref_ : run = {
    run=(fun x -> E_in_mem.Run_pure.run (!ref_) x |> function
      | (w',a) -> 
        ref_:=w';
        a |> function
        | Ok a -> a
        | Error e -> 
          ("Run resulted in error" |> log_.log);
          match w.internal_error_state with 
          | None -> (
              match w.thread_error_state with
              | None -> 
                "impossible, gfuse.161" |> fun s ->
                log_.log s;
                raise @@ Unix_error(EUNKNOWNERR 99, s, s)
              | Some e ->
                "gfuse.165, thread error: "^(A_error.exn__to_string e) |> fun s ->
                log_.log s;
                raise @@ mk_unix_exn e)
          | Some s ->
            "thread error gfuse.170: "^s) |> fun s ->
          log_.log s;
          raise @@ Unix_error(EUNKNOWNERR 99, s, s)
      | `Finished(a,w) -> 
        ref_:=w;
        a)
  }
end
*)

