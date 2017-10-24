(* remote fs server using plain unix (could be lwt) *)

open Tjr_connection
open Tjr_minifs
open C_base
open C_msgs
open E_in_mem


module Shared = struct
  open Unix
  let ip = Unix.inet_addr_of_string "127.0.0.1"
  let rport=4001
  let sport=4007

  let s = ADDR_INET(ip,sport)
  let r = ADDR_INET(ip,rport)

  let sender = { local=s; remote=r }
  let recvr = {local=r; remote=s }
end

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
  listen_accept ~quad:Shared.recvr >>= function
  | `Connection conn ->
    let rec loop () = 
      recv_string ~conn >>= fun s ->
      string_to_msg s |> function
      | Error e -> 
        print_endline @@ "nfs_server.63, error unmarshalling string: "^e;
        exit 1
      | Ok msg -> 
        serve msg |> fun x ->
        E_in_mem.run (!w_ref) x |> function
        | `Exn_ (e,w) -> (
            w_ref:={!w_ref with fs=w.fs};  (* FIXME in exceptional case, fs unchanged?*)
            match w.thread_error_state with
            | None -> (
                match w.internal_error_state with
                | None -> 
                  print_endline @@ "nfs_server.74, impossible"; (* exceptional state *)
                  exit 1
                | Some e ->
                  Printf.printf "nfs_server.65, internal error: %s\n" e;
                  exit 1)
            | Some e -> 
              send ~conn (Error e) >>= fun () -> loop())
        | `Finished (a,w) -> 
          w_ref:=w;
          send ~conn (Msg a) >>= fun () -> loop ()
    in
    loop ()
  | `Error_incorrect_peername | `Net_err _ -> failwith __LOC__


let _ = main()
