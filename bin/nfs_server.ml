(* remote fs server using lwt *)

open Lwt
open Lwt_unix
open Tjr_connection
open Tjr_minifs


module Shared = struct
  let ip = Unix.inet_addr_of_string "127.0.0.1"
  let rport=4001
  let sport=4007

  let s = ADDR_INET(ip,sport)
  let r = ADDR_INET(ip,rport)

  let sender = { local=s; remote=r }
  let recvr = {local=r; remote=s }
end


module In_mem_with_unix_errors = Mini_fuse.In_mem_with_unix_errors


let ops = In_mem_with_unix_errors.ops


let monad_ops = Mini_in_mem.monad_ops


(* NOTE data is Msgs.data = string; buffer is mim.buffer = bigarray *)
let data_of_buffer ~buffer ~len =
  Mini_pervasives.bigarray_to_string ~src:buffer ~off:0 ~len

let buffer_of_data d = Mini_pervasives.string_to_bigarray d
  

let serve = 
  Mini_nfs.mk_serve
    ~monad_ops
    ~backend:ops
    ~data_of_buffer
    ~buffer_of_data
    ~mk_buffer:Bigarray_buffer.create

let _ = serve


include struct
  open Msgs
  let send ~conn (m:msg_from_server) =
    m |> msg_from_server_to_yojson |> Yojson.Safe.pretty_to_string
    |> fun string_ -> send_string ~conn ~string_ 
      
  let string_to_msg s = s |> Yojson.Safe.from_string |> msg_from_client_of_yojson
end


let main () = 
  let w_ref = ref Mini_in_mem.init_t in
  print_endline "nfs_server accepting connections";
  listen_accept ~quad:Shared.recvr >>= fun conn ->
  let rec loop () = 
    recv_string ~conn >>= fun s ->
    let open Msgs in
    let open Mini_in_mem in
    string_to_msg s |> function
    | Error e -> 
      print_endline @@ "nfs_server.63, error unmarshalling string: "^e;
      exit 1
    | Ok msg -> 
      serve msg |> fun x ->
      Mini_in_mem.run (!w_ref) x |> function
      | `Exceptional w -> (
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


let _ = Lwt_main.run @@ main()
