(*
(* version of unix that keeps track of an int<->fd/dh map *)

(* when we allocate an fd or dh, we tag it with a genint, then insert
   (with key the genint) into a map; subsequently, we only pass the
   genint to the client *)

open C_base
open Tjr_map

type w = {
  ew: E_unix.w;
  int2fd: Unix.file_descr Map_int.t;
  int2dh: Unix.dir_handle Map_int.t;
}

let initial_world : w = 
  { ew=E_unix.initial_world; int2fd=Map_int.empty; int2dh=Map_int.empty }

let genint = Tjr_gensym.gensym

module Monad = struct
  type 'a m = ('a,w)Step_monad.m
  let bind,return = Step_monad.(bind,return)
end
include Monad

(* generate types --------------------------------------------------- *)

module Ops_type = D_functors.Make_ops_type(Monad)(E_unix.Unix_base_types)
include Ops_type

module Ops_type_plus = struct
  include Monad
  include E_unix.Unix_base_types
  include Ops_type
end
      

module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus)


let ops = E_unix.unix_ops

let run = Step_monad.run ~dest_exceptional:E_unix.dest_exceptional  

let _ = run

let lift (a:'a E_unix.m) : 'a m = Step_monad.Step(fun w ->
  run w.ew a |> function
  | `Exceptional (e,ew) -> 
    ({w with ew}, fun () -> Step_monad.failwith_step_error __LOC__)
  | `Finished (ew,a) -> 
    ({w with ew},fun () -> Step_monad.Finished a))

let mk_ops () = 
  let open E_unix in
  let root = ops.root in

  let unlink ~parent ~name = lift(ops.unlink ~parent ~name) in

  let mkdir ~parent ~name = lift(ops.mkdir ~parent ~name) in
  
  let opendir path = 
    

  
  ()
*)
