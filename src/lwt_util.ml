(** Implement "co-eta" to extract a value from the lwt monad.

We assume that the Lwt main loop is running in a separate
   Stdlib.Thread. We can then use Lwt_preemptive.run_in_main to run
   the lwt code and return the result.  *)

open Minifs_intf
open Tjr_monad.With_lwt

let co_eta = 
  let co_eta (type a) (t0:(a,lwt)m) : a = 
    Lwt_preemptive.run_in_main (fun () -> to_lwt t0)
  in
  ({ co_eta } : lwt co_eta)
  


(* OLD

So the strategy is to create a mutex, cvar and a ref
   (to hold the result). Then we lock the mutex, construct an lwt
   wrapper thread that wraps some underlying lwt call whilst locking
   and then signalling and releasing the lock.

    let m,c = Mutex.create(),Condition.create() in
    
    (* placeholder for return value *)
    let v : 'a option ref = ref None in
    let return_ : 'a -> (unit,lwt)m = fun x ->
      v:=Some x;
      Condition.signal c;
      Mutex.unlock m;
      return ()
    in
    let spawn (f: unit -> ('a,lwt)m) =
      Mutex.lock m;
*)
