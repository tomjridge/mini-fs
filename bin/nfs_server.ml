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

let serve = 
  Mini_nfs.mk_serve 
    ~monad_ops
    ~backend:ops
    

let main () = 
  listen_accept ~quad:Shared.recvr >>= fun conn ->
  
  let rec loop i () =
    recv_string ~conn >>= fun msg ->
    (if i mod 100 = 0 then print_endline msg else ());
    send_string ~conn ~string_:msg >>= fun () ->
    loop (i+1) ()
  in
  loop 0 ()

let _ = Lwt_main.run @@ main()
