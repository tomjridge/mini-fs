(* remote fs server using in-mem (could be lwt) *)

open Tjr_monad
open Tjr_monad.Monad

open Tjr_connection
open Tjr_minifs
open Base_
open Msgs

(* backend ---------------------------------------------------------- *)

open In_mem

let monad_ops = in_mem_monad_ops

let ops = in_mem_state_passing_ops


(* server ----------------------------------------------------------- *)

open Nfs_server

let serve = 
  let open Int_base_types in
  mk_serve
    ~monad_ops
    ~ops
    ~dh2i
    ~i2dh
    ~fd2i
    ~i2fd

let _ : msg_from_client -> (msg_from_server,'t) m = serve


let send ~conn (m:msg_from_server) =
  let open Tjr_connection.Unix_ in
  m |> msg_s_to_string |> send_string ~conn


(* main ------------------------------------------------------------- *)

let quad = Runtime_config.get_config ~filename:"config.json" @@ 
  fun ~client ~server ~log_everything -> server

let run = Tjr_monad.State_passing_instance.run

let main () = 
  let open Tjr_connection.Unix_ in
  let ( >>= ) = bind in
  let w_ref = ref init_t in
  print_endline "nfs_server accepting connections";
  listen_accept ~quad >>= function
  | Error e -> failwith __LOC__
  | Ok conn ->
    let rec loop () = 
      recv_string ~conn >>= function
      | Error () -> failwith __LOC__
      | Ok s ->
        log_.log s;
        string_to_msg_c s |> fun msg -> 
        serve msg |> fun x ->
        run ~init_state:!w_ref x |> function
        | a,w -> 
          w_ref:=w;
          send ~conn a >>= function
          | Error () -> failwith __LOC__
          | Ok () -> loop ()
    in
    loop ()


let _ = main()

