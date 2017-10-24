(* nfs client ------------------------------------------------------- *)

(* we use unix again, since we will probably interface with fuse *)

open Tjr_connection
open Tjr_minifs
open C_base

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


(* instantiate nfs_client ------------------------------------------- *)

(*

*)


type w = {
  thread_error: exn_ option;
  internal_error: string option;
  conn: Unix.file_descr  (* ASSUMED valid *)
}


module Monad = struct
  type 'a m = ('a,w) Step_monad.m
  let return,bind = Step_monad.(return,bind)
end
include Monad

module Base_types = E_in_mem.Mem_base_types


module Ops_type = D_functors.Make_ops_type(Monad)(Base_types)
include Ops_type

module Ops_type_plus = struct
  include Monad
  include Base_types
  include Ops_type
end

module Client' = G_nfs_client.Make_client(Ops_type_plus)
include Client'

(* in order to call mk_client_ops, we need extra_ops *)
let extra_ops = {
  internal_err=(fun s -> Step_monad.Step(fun w -> 
      {w with internal_error=Some s},fun () -> failwith __LOC__))
}

(* we also need to implement call: msg_from_client->msg_from_server' m *)
include struct
  open C_msgs
  open Tjr_connection.Unix_
  let send ~conn (m:msg_from_client) =
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string
    |> fun s -> send_string ~conn s

  let _ = send
  
  let string_to_msg s = 
    s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
    |> function
    | Ok x -> x
    | Error e -> failwith __LOC__
end


(* NOTE from the server, we send a msg_from_server, but we need to
   convert this into a monadic error *)

include struct
  open C_msgs
  let call ~conn (m:msg_from_client) : msg_from_server' m = 
    Step_monad.Step(fun w -> 
        m |> send ~conn |> function
        | Error e -> failwith __LOC__ 
        | Ok () -> 
          Tjr_connection.Unix_.recv_string ~conn |> function
          | Error e -> failwith __LOC__
          | Ok s -> 
            s |> string_to_msg |> fun m -> 
            match m with
            | Msg m -> (w,fun () -> Step_monad.Finished m)
            | Error (e:exn_) -> 
              {w with thread_error=Some e},fun () -> failwith __LOC__)
end


let conn = 
  Lwt_main.run (Tjr_connection.connect ~quad:Shared.sender)
  |> function
  | `Connection c -> c
  | _ -> failwith __LOC__


let call = call ~conn


module Main : sig val main: unit -> unit end = struct

  let client_ops = 
    Mini_nfs.mk_client_ops
      ~monad_ops
      ~extra_ops
      ~call

  let _ = client_ops

  let w = ref None 
  let dest_exceptional w = w 

  let run : 'a m -> 'a = Step_monad.(fun x -> run ~dest_exceptional w x)

  let _ = run

  let run = { Minifs.run=run }

  let _ = run

  let imp_ops = Minifs.mk_imperative_ops ~run ~ops:client_ops

  let main () = 

    Minifs.dest_imperative_ops imp_ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->

    let readdir' = Minifs.readdir' ~ops:imp_ops in

    mkdir ~parent:"/" ~name:"tmp";
    readdir' "/" |> List.iter print_endline;
    ()

end


let _ = Main.main()






