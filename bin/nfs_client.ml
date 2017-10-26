(* nfs client ------------------------------------------------------- *)

(* we use unix again, since we will probably interface with fuse *)

open Tjr_connection
open Tjr_minifs
open C_base

module Connection = Tjr_connection.Unix_

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

let init_w conn = { thread_error=None; internal_error=None; conn }

let dest_exceptional w = 
  assert(w.internal_error=None);
  w.thread_error

let _ = dest_exceptional 


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

module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus)

module Imp_ops_type_plus = struct
  include Monad
  include Base_types
  include Imp_ops_type
end

module Readdir' = D_functors.Make_readdir'(Imp_ops_type_plus)
      

module Client' = G_nfs_client.Make_client(Ops_type_plus)
include Client'

(* in order to call mk_client_ops, we need extra_ops *)
let extra_ops = {
  internal_marshal_err=(fun s -> Step_monad.Step(fun w -> 
      {w with internal_error=Some s},fun () -> failwith __LOC__))
}

(* we also need to implement call: msg_from_client->msg_from_server' m *)
include struct
  open C_msgs
  open Connection
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
          Connection.recv_string ~conn |> function
          | Error e -> failwith __LOC__
          | Ok s -> 
            s |> string_to_msg |> fun m -> 
            match m with
            | Msg m -> (w,fun () -> Step_monad.Finished m)
            | Error (e:exn_) -> 
              {w with thread_error=Some e},fun () -> failwith __LOC__)
end


let conn = 
  Connection.connect ~quad:Shared.sender
  |> function 
  | Ok x -> (x |> function
    | `Connection c -> c
    | `Net_err e -> raise e)
  | Error e -> raise e  (* shouldn't happen, given that msg_lib uses catch  *)


let call = call ~conn

let client_ops = E_in_mem.(
    Client'.mk_client_ops
      ~extra_ops
      ~call
      ~i2dh ~dh2i
      ~i2fd ~fd2i)


let run (type a) ~w_ref (a:a m) = 
  let w = w_ref in
  !w |> fun w' ->
  Step_monad.run ~dest_exceptional w' a |> function
  | `Exceptional(e,w'') -> w:=w''; raise (A_error.mk_unix_exn e)  (* FIXME? *)
  | `Finished(w'',a) -> w:=w''; a

module Main : sig val main: unit -> unit end = struct

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
    readdir' "/" |> List.iter print_endline;
    ()

end


let _ = Main.main()






