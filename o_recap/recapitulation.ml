(* Pick out the main types and record in a single file *)

(* FIXME abandoned - use code cutting to extract the relevant pieces

The problem is we want to say:

module type MONAD = Monad_type_.MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: 'a m -> ('a -> 'b m) -> 'b m
end

But this isn't accepted by OCaml grammar

 *)



include Base_.Step_monad  

(* = Tjr_step_monad 

type ('a,'w) step_monad = Step of ('w -> 'w * ('a,('a,'w)step_monad) either)

*)

include Monad_type_

include Base_types_
module Int_base_types = Int_base_types


include R_

include Error_types

include Ops_type_


(* NOTE following refinements of OPS_TYPE *)
open Ops_types

module type OPS_TYPE_WITHOUT_MONAD = OPS_TYPE_WITHOUT_MONAD

module type OPS_TYPE_WITH_RESULT = OPS_TYPE_WITH_RESULT

module type IMP_OPS_TYPE = IMP_OPS_TYPE


open In_mem
module Mem_base_types = Mem_base_types (* = Int_base_types *)

module In_mem_monad = In_mem_monad

type nonrec 'e extra_ops = 'e extra_ops



open Unix_ops
module Unix_base_types = Unix_base_types
module Unix_monad = Unix_monad
module Unix_ops_type = Unix_ops.Ops_type


open Fuse_
module Make_fuse = Make_fuse

(* module Make_fuse(I:Ops_types.OPS_TYPE_WITH_RESULT) =  struct ... *)


open Nfs_client
module Make_client = Make_client

(* module Make_client(O:OPS_TYPE_WITH_RESULT) = struct *)

open Nfs_server
module Make_server = Make_server

(* module Make_server(O:OPS_TYPE_WITH_RESULT) = struct *)


open Fuse_nfs
module Make_fuse_nfs_client = Make_fuse_nfs_client

(* module Make_fuse_nfs_client(O:OPS_TYPE_WITH_RESULT) = struct ... *)




