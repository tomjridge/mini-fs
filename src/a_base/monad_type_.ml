(* Although we expect to use Tjr_fs_shared.Monad, we try to make the
   interfaces parametric over the monad *)
(* :bb: *)
module type MONAD = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: 'a m -> ('a -> 'b m) -> 'b m
end
(* :bc: *)
