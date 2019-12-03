(* nfs client test  ------------------------------------------------- *)

(* open Tjr_connection *)
open Tjr_minifs
open Base_
open Ops_type_

module Connection = Tjr_connection.Unix_


(* config ----------------------------------------------------------- *)

let _ = Runtime_config.test()

let quad = Runtime_config.get_config () @@ 
  fun ~client ~server:_ ~log_everything:_ -> client


(* monad ------------------------------------------------------------ *)

let monad_ops = Tjr_monad.State_passing.monad_ops ()


(* connection, call ------------------------------------------------- *)

let conn = 
  Connection.connect ~quad
  |> function 
  | Ok fd -> fd
  | Error _ -> failwith __LOC__

let call = Nfs_client.State_passing_call.call ~conn


(* set up nfs client ------------------------------------------------ *)

let internal_marshal_err = fun s -> failwith s
let internal_marshal_err = Nfs_client.{internal_marshal_err}

let client_ops = 
  let open Int_base_types in
  Nfs_client.mk_client_ops
    ~monad_ops
    ~internal_marshal_err
    ~call
    ~i2dh ~dh2i
    ~i2fd ~fd2i

let _ = client_ops

let ops = client_ops

(* run tests -------------------------------------------------------- *)

let run ~init_state m = Tjr_monad.State_passing.to_fun m init_state

let run x = run ~init_state:() x |> function
  | a,_ -> a |> function
    | Error e -> (e |> exn__to_string |> log_.log_now; failwith __LOC__)
    | Ok a -> a

let readdir' = Readdir'.readdir' ~monad_ops ~ops

let main () = 
  Printf.printf "Calling mkdir tmp; mkdir tmp2\n";
  ops.mkdir "/tmp" |> run;
  ops.mkdir "/tmp2" |> run;
  Printf.printf "Calling readdir'\n";    
  readdir' "/" |> run |> List.iter print_endline;
  ()
(* FIXME note that this causes the server to fail with an exception
   because we don't close the connection in a nice way *)

let _ = main()





