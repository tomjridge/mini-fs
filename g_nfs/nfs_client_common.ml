(* common defns for nfs_client and fuse_nfs_client *)

(* we use unix again, since we will probably interface with fuse *)

open Tjr_connection
open Base_

module Connection = Tjr_connection.Unix_


(* instantiate nfs_client ------------------------------------------- *)

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
  type 'a m = ('a,w) Step_monad.step_monad
  let return,bind = Step_monad.(return,bind)
end
include Monad


module Base_types = In_mem.Mem_base_types


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
  internal_marshal_err=Step_monad.(fun s -> Step(fun w -> 
      {w with internal_error=Some s},fun () -> failwith_step_error __LOC__))
}


(* we also need to implement call: msg_from_client->msg_from_server' m *)
include struct
  open C_msgs
  let msg_to_string m = 
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string

  let string_to_msg s = 
    s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
    |> function
    | Ok x -> x
    | Error e -> 
      exit_1 __LOC__
        
end


(* NOTE from the server, we send a msg_from_server, but we need to
   convert this into a monadic error *)
include struct
  open C_msgs
  let call ~conn (m:msg_from_client) : msg_from_server' m = 
    Step_monad.Step(fun w -> 
        m |> msg_to_string 
        |> fun s -> 
        log_.log_lazy (fun () -> Printf.sprintf "sending %s\n" s);
        s |> Connection.send_string ~conn |> function Error () -> exit_1 __LOC__ | Ok () -> 
          Connection.recv_string ~conn |> function Error () -> exit_1 __LOC__ | Ok s -> 
            log_.log_lazy (fun () -> Printf.sprintf "receiving %s\n" s);
            s |> string_to_msg |> fun m -> 
            match m with
            | Msg m -> (w,fun () -> Step_monad.Finished m)
            | Error (e:exn_) -> 
              {w with thread_error=Some e},fun () -> 
                exit_1 "hncc: attempt to step exceptional state")
end


let run (type a) ~w_ref (a:a m) = 
  let w = w_ref in
  !w |> fun w' ->
  Step_monad.run ~dest_exceptional w' a |> function
  | `Exceptional(e,w'') -> 
    (* NOTE we don't update w since conn is mutable, internal error is
       None, and this error is just for the particular call *)
    raise (A_error.mk_unix_exn e)  (* FIXME? *)
  | `Finished(w'',a) -> w:=w''; a


