(* remote fs server using plain unix (could be lwt) *)

open Tjr_connection
open Tjr_minifs
open Base_
open Msgs

(* backend ---------------------------------------------------------- *)
open In_mem

(* server ----------------------------------------------------------- *)
module Server' = Nfs_server.Make_server(Ops_type_plus)
include Server'


let serve = 
  let open Int_base_types in
  mk_serve
    ~ops
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
  fun ~client ~server ~log_everything -> server

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
        In_mem.run (!w_ref) x |> function
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


let _ = main()

