(* nfs client ------------------------------------------------------- *)

(* we use unix again, since we will probably interface with fuse *)

open Tjr_connection
open Tjr_minifs
open C_base
open H_nfs_client_common

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


module Main : sig val main: unit -> unit end = struct

  (* put all state in this module *)
  let conn = 
    Connection.connect ~quad:Shared.sender
    |> function 
    | Ok fd -> fd
    | Error (e,s1,s2) -> raise (Unix.Unix_error(e,s1,s2))

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
    imp_ops.mkdir ~parent:"/" ~name:"tmp";
    imp_ops.mkdir ~parent:"/" ~name:"tmp2";
    readdir' "/" |> List.iter print_endline;
    ()

end


let _ = Main.main()






