(* remote fs server using unix passthrough  *)

open Tjr_connection
open Tjr_minifs
open Base_
open Msgs

(* backend ---------------------------------------------------------- *)

module Backend = Unix_with_int_handles
open Backend

(* server ----------------------------------------------------------- *)

module Server' = Nfs_server.Make_server(Ops_type_plus)
include Server' 

let serve = 
  let open Int_base_types in
  mk_serve
    ~ops:Backend.ops
    ~dh2i
    ~i2dh
    ~fd2i
    ~i2fd

let _ : msg_from_client -> msg_from_server m = serve

include struct
  open Tjr_connection.Unix_
  let send ~conn (m:msg_from_server) =
    m |> msg_s_to_string |> send_string ~conn
end


(* main ------------------------------------------------------------- *)

let quad = Runtime_config.get_config ~filename:"config.json" @@ 
  fun ~client ~server -> server

let run = Step_monad.run ~dest_exceptional:(fun x -> None)

let main ~init_world = 
  let open Tjr_connection.Unix_ in
  let ( >>= ) = bind in
  let w_ref = ref init_world in
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
        run (!w_ref) x |> function
        | Error (`Attempt_to_step_exceptional_state w) ->
            (* NOTE an internal error - so just exit *)
            failwith __LOC__
        | Ok (w,a) -> 
          w_ref:=w;
          send ~conn a >>= function
          | Error () -> failwith __LOC__
          | Ok () -> loop ()
    in
    loop ()


let _ = main ~init_world:Unix_with_int_handles.initial_world

