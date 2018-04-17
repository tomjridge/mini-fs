(* common instance of Monad, Base_types and R ----------------------- *)

open Base_

module World : sig type w val init_w:w end = struct
  type w = unit
  let init_w = ()
end
open World

module Monad = struct
  type 'a m = ('a,w)Monad.m
  let bind a ab = Monad.bind ab a
  let return = Monad.return
end
open Monad

module MBR = struct
  include Monad
  include Int_base_types
  include R_as_result
end
  
module Ops_type = Ops_types.Make_ops_type(MBR)
module Ops_type_plus = struct include MBR include Ops_type end

let run x = Tjr_fs_shared.Monad.run init_w x

let readdir' = 
  let module M = Readdir'.Make_readdir'(Ops_type_plus) in 
  M.readdir' 
