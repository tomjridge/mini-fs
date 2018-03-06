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


(* NOTE struct include x end and sig version are so that ocamlc -i
   includes the full detail *)
module Int_base_types = struct include Int_base_types end


include R_

module Error_types = struct include Error_types end

include Ops_type_


(* NOTE following refinements of OPS_TYPE *)
open Ops_types

module type OPS_TYPE_WITHOUT_MONAD = sig include OPS_TYPE_WITHOUT_MONAD end

module type OPS_TYPE_WITH_RESULT = sig include OPS_TYPE_WITH_RESULT end

module type IMP_OPS_TYPE = sig include IMP_OPS_TYPE end


open In_mem
module Mem_base_types = struct include Mem_base_types end (* = Int_base_types *)

module In_mem_monad = struct include In_mem_monad end

type nonrec 'e extra_ops = 'e extra_ops



open Unix_ops
type nonrec w = Unix_ops.w
module Unix_base_types = struct include Unix_base_types end
module Unix_monad = struct include Unix_monad end
module Unix_MBR = struct include MBR end
module Unix_ops_type = struct include Unix_ops.Ops_type end


open Fuse_
module Make_fuse(I:Ops_types.OPS_TYPE_WITH_RESULT) = 
  struct include Make_fuse(I) end

(* module Make_fuse(I:Ops_types.OPS_TYPE_WITH_RESULT) =  struct ... *)

let fuse_in_mem_ops = Fuse_in_mem.fuse_ops


open Nfs_client
module Make_client(O:OPS_TYPE_WITH_RESULT) = struct include Make_client(O) end

(* module Make_client(O:OPS_TYPE_WITH_RESULT) = struct *)

open Nfs_server
module Make_server(O:OPS_TYPE_WITH_RESULT) = struct include Make_server(O) end

(* module Make_server(O:OPS_TYPE_WITH_RESULT) = struct *)


open Fuse_nfs
module Make_fuse_nfs_client(O:OPS_TYPE_WITH_RESULT) = 
struct include Make_fuse_nfs_client(O) end

(* module Make_fuse_nfs_client(O:OPS_TYPE_WITH_RESULT) = struct ... *)




