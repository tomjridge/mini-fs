(* common instance of Monad, Base_types and R ----------------------- *)

open Base_

module World : sig type w val init_w:w end = struct
  type w = unit
  let init_w = ()
end
open World

module Monad = struct
  type 'a m = ('a,w)Tjr_step_monad.m
  let bind (ab:'a -> 'b m) (a:'a m) : 'b m = Tjr_step_monad.bind ab a
  let _ = bind
  let return = Tjr_step_monad.return
end

open Monad

module MBR = struct
  include Monad
  include Int_base_types
  include R_as_result
end
  
module Ops_type = Ops_types.Make_ops_type(MBR)
module Ops_type_plus = struct include MBR include Ops_type end

let run x = Tjr_step_monad.Extra.run init_w x

let readdir' = 
  let module M = Readdir'.Make_readdir'(Ops_type_plus) in 
  M.readdir' 
