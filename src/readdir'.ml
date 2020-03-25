(** A utility function to return all possible entries in a directory (for small directories/testing only! *)

open Minifs_intf
(* open Ops_type_ *)

(** Read all directory entries at once; obviously not a good idea if
   there are a large number of entries. *)
let readdir' ~monad_ops ~(ops:(_,_,_)ops) = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  fun path ->
    ops.opendir path >>= function Error e -> return (Error e) | Ok dh ->
      let es = ref [] in
      let rec f () = 
        ops.readdir dh 
        >>= function Error e -> return (Error e) | Ok (es',finished) ->
          es:=!es@es';
          if finished.is_finished then return (Ok !es) else f ()
      in
      f() >>= fun x -> 
      ops.closedir dh >>= fun _ -> 
      return x
