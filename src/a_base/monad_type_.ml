(* Although we expect to use Tjr_fs_shared.Monad, we try to make the
   interfaces parametric over the monad *)

(* FIXME unify with tjr_step_monad; reverse order of bind *)

(* :bb: *)
module type MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: ('a -> 'b m) -> 'a m -> 'b m
end
(* :bc: *)
