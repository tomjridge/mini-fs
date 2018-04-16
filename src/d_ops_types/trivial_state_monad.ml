(* common instance of Monad, Base_types and R ----------------------- *)

open Base_

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
  
module Ops_type = Ops_types.Make_ops_type(MBR)
module Ops_type_plus = struct include MBR include Ops_type end

let run x = Step_monad.run ~dest_exceptional:(fun x -> None) init_w x

let readdir' = 
  let module M = Readdir'.Make_readdir'(Ops_type_plus) in 
  M.readdir' 
