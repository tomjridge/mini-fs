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

type e = [ `Internal of string ]
type w = e option
type 'a m = ('a,w) Step_monad.m

let err = Step_monad.(fun e -> Step(fun w -> Some e,fun () -> failwith __LOC__))
let monad_ops = Step_monad.{
  return;
  bind;
  err;
}

let extra_ops = Mini_nfs.C_.{
    internal_err=(fun s -> err (`Internal s))
  }


include struct
  open Msgs
  let send ~conn (m:msg_from_client) =
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string
    |> fun string_ -> send_string ~conn ~string_ 

  let _ = send
      
  let string_to_msg s = 
    s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
    |> function
    | Ok x -> x
    | Error e -> failwith __LOC__
end


include struct
  open Step_monad
  open Msgs
  let call ~conn (m:msg_from_client) : (msg_from_server,'m) m = 
    Finished (
      let open Lwt in
      let lwt = m |> send ~conn >>= fun () -> recv_string ~conn >>= fun s -> 
        return @@ string_to_msg s 
      in
      let lwt = Lwt.catch (fun () -> lwt) (fun e -> Printexc.to_string e|> print_endline; failwith __LOC__) in
      let r = Lwt_main.run lwt in
      r)
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






