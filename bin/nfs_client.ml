(* nfs client ------------------------------------------------------- *)

(* we use unix again, since we will probably interface with fuse *)

open Tjr_connection
open Tjr_minifs
open C_base
open H_nfs_client_common

let _ = M_runtime_config.test()

let quad = M_runtime_config.get_config ~filename:"config.json" @@ 
  fun ~client ~server -> client

module Main : sig val main: unit -> unit end = struct

  (* put all state in this module *)
  let conn = 
    Connection.connect ~quad
    |> function 
    | Ok fd -> fd
    | Error _ -> exit_1 __LOC__

  let call = call ~conn

  let client_ops = E_in_mem.(
      Client'.mk_client_ops
        ~extra_ops
        ~call
        ~i2dh ~dh2i
        ~i2fd ~fd2i)

  let w : w ref = ref (init_w conn)

  let run = Imp_ops_type.{run=(fun a -> run ~w_ref:w a)}

  let imp_ops = 
    Imp_ops_type.mk_imperative_ops 
      ~ops:client_ops
      ~run

  let _ = imp_ops

  open Imp_ops_type

  let main () = 
    let readdir' = Readdir'.readdir' ~ops:imp_ops in
    Printf.printf "calling mkdir tmp; mkdir tmp2\n";
    imp_ops.mkdir ~parent:"/" ~name:"tmp";
    imp_ops.mkdir ~parent:"/" ~name:"tmp2";
    Printf.printf "calling readdir'\n";    
    readdir' "/" |> List.iter print_endline;
    ()

end


let _ = Main.main()






