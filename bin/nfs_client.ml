(* nfs client *)

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

let extra_ops = Mini_nfs.C_.{
    internal_err=Mini_in_mem.internal_err
  }

include struct
  open Msgs
  let send ~conn (m:msg_from_client) =
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string
    |> fun string_ -> send_string ~conn ~string_ 
      
  let string_to_msg s = s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
end


include struct
  open Step_monad
  open Msgs
  let call ~conn (m:msg_from_client) : (msg_from_server','m) m = 
    m |> send ~conn |> recv_string ~conn
      
  (* have the problem of two monads; in the server, we run in the fs
     monad, then patch around with lwt; here the outside monad is the
     fs monad, and we need to call the lwt monad and wait for a
     return; alternatively we can hack the step monad to include lwt,
     but this is a bit horrible; or maybe make a combined monad? *)

end

let client_ops = 
  Mini_nfs.mk_client_ops
    ~monad_ops
    ~extra_ops
    ~call
