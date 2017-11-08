(* nfs client test  ------------------------------------------------- *)

open Tjr_connection
open Tjr_minifs
open Base_

module Connection = Tjr_connection.Unix_


(* config ----------------------------------------------------------- *)

let _ = Runtime_config.test()

let quad = Runtime_config.get_config ~filename:"config.json" @@ 
  fun ~client ~server ~log_everything -> client


(* connection, call ------------------------------------------------- *)

include struct
  let conn = 
    Connection.connect ~quad
    |> function 
    | Ok fd -> fd
    | Error _ -> failwith __LOC__

  let call = Nfs_client.Step_monad_call.call ~conn
end


(* set up nfs client ------------------------------------------------ *)

open Trivial_state_monad
open Ops_type_plus 


module Client' = Nfs_client.Make_client(Trivial_state_monad.Ops_type_plus)
open Client'


let internal_marshal_err = fun s -> failwith s
let internal_marshal_err = Client'.{internal_marshal_err}


let client_ops = 
  let open Int_base_types in
  Client'.mk_client_ops
        ~internal_marshal_err
        ~call
        ~i2dh ~dh2i
        ~i2fd ~fd2i


let ops : Ops_type_plus.ops = client_ops


(* run tests -------------------------------------------------------- *)

let run x = Trivial_state_monad.run x |> function
  | Error (`Attempt_to_step_exceptional_state _) -> failwith __LOC__
  | Ok (w,a) -> a |> function
    | Error e -> (e |> exn__to_string |> log_.log_now; failwith __LOC__)
    | Ok a -> a

let readdir' = readdir' ~ops

let main () = 
  Printf.printf "Calling mkdir tmp; mkdir tmp2\n";
  ops.mkdir ~parent:"/" ~name:"tmp" |> run;
  ops.mkdir ~parent:"/" ~name:"tmp2" |> run;
  Printf.printf "Calling readdir'\n";    
  readdir' "/" |> run |> List.iter print_endline;
  ()

let _ = main()






