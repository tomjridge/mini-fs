open Tjr_monad.Monad

(* Read all directory entries at once; obviously not a good idea if
   there are a large number of entries. *)
open Ops_type_

(* for small directories *)
let readdir' ~monad_ops ~ops = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  fun path ->
    ops.opendir path >>= function Error e -> return (Error e) | Ok dh ->
      let es = ref [] in
      let rec f () = 
        ops.readdir dh 
        >>= function Error e -> return (Error e) | Ok (es',finished) ->
          es:=!es@es';
          if finished then return (Ok !es) else f ()
      in
      f() >>= fun x -> 
      ops.closedir dh >>= fun _ -> 
      return x
