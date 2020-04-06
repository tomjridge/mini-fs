(* separate file so can use before we get to base_ *)

include Tjr_lib.Log
let log_ = mk_log_ops()


let log_ = 
  if Config.log_immediately
  then 
    (* NOTE this ensures all logs appear immediately *)
    {
      log_ with 
      log=(fun s -> log_.log_now s);
      log_lazy=(fun f -> log_.log_now (f()));
    }
  else log_

(* FIXME add config flag *)
let () = at_exit log_.print_last_n

