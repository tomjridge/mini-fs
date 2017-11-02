(* remote fs server using plain unix (could be lwt) *)

open Tjr_connection
open Tjr_minifs
open C_base
open C_msgs
open E_in_mem


let quad = M_runtime_config.get_config ~filename:"config.json" @@ 
  fun ~client ~server -> server

module Server' = G_nfs_server.Make_server(Ops_type_plus)
include Server'

let serve = 
  let id = fun x -> x in
  mk_serve
    ~ops:logged_ops
    ~dh2i:id
    ~i2dh:id
    ~fd2i:id
    ~i2fd:id


(* NOTE the errors are still in the monad *)
let _ : msg_from_client -> msg_from_server' m = serve

include struct
  (* NOTE Tjr_connection.Unix_ has: 'a m = ('a,exn)result, which is not
     the same as in_mem.m *)
  open Tjr_connection.Unix_
  let send ~conn (m:msg_from_server) =
    m |> msg_from_server_to_yojson |> Yojson.Safe.pretty_to_string
    |> fun s -> send_string ~conn s

  let string_to_msg s = s |> Yojson.Safe.from_string |> msg_from_client_of_yojson
end


let main () = 
  let open Tjr_connection.Unix_ in
  let ( >>= ) = bind in
  let w_ref = ref init_t in
  print_endline "nfs_server accepting connections";
  listen_accept ~quad >>= function
  | Error e -> exit_1 __LOC__
  | Ok conn ->
    let rec loop () = 
      recv_string ~conn >>= function
      | Error () -> exit_1 __LOC__
      | Ok s ->
        log_.log s;
        string_to_msg s |> function
        | Error e -> 
          "nfs_server.63, error unmarshalling string: "^e |> exit_1
        | Ok msg -> 
          serve msg |> fun x ->
          E_in_mem.run (!w_ref) x |> function
          | `Exn_ (e,w) -> (
              (* FIXME in exceptional case, fs unchanged?*)
              w_ref:={!w_ref with fs=w.fs};  
              (* error is bound to e; FIXME ASSUMES
                 w.thread_error_state/internal_error_state is None? *)
              (* in the error case, we send the exception back *)
              send ~conn (Error e) >>= function
              | Error () -> exit_1 __LOC__
              | Ok () -> loop())
          | `Finished (a,w) -> 
            w_ref:=w;
            send ~conn (Msg a) >>= function
            | Error () -> exit_1 __LOC__
            | Ok () -> loop ()
    in
    loop ()


let _ = main()

