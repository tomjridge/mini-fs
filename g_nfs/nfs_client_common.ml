(* common defns for nfs_client.exe and fuse_nfs_client.exe *)

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


(* NOTE if the client is Lwt-concurrent we would instantiate with Lwt
   monad *)
module Monad = struct
  type 'a m = ('a,w) Step_monad.step_monad
  let return,bind = Step_monad.(return,bind)
end
include Monad


module Base_types = In_mem.Mem_base_types

module MBR = struct
  include Monad
  include Base_types
  type ('a,'e) r_ = ('a,'e)result
end

module Ops_type = Ops_types.Make_ops_type(MBR)
include Ops_type

module Ops_type_plus = struct
  include MBR
  include Ops_type
end


(*
module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus)


module Imp_ops_type_plus = struct
  include Monad
  include Base_types
  include Imp_ops_type
end
*)

module Readdir' = Readdir'.Make_readdir'(Ops_type_plus)
      

module Client' = Nfs_client.Make_client(Ops_type_plus)
include Client'


(* in order to call mk_client_ops, we need extra_ops *)
include struct
  open Tjr_either
  open Step_monad
  let extra_ops = 
    let f s = 
      Step(fun w -> 
          {w with internal_error=Some s},Inr (Step(fun _ -> failwith __LOC__)))
    in
    { internal_marshal_err=f }
end

(* we also need to implement call: msg_from_client->msg_from_server m *)
include struct
  open Tjr_either
  open Step_monad
  open Msgs
  let call ~conn (m:msg_from_client) : msg_from_server m = 
    Step(fun w -> 
        m |> msg_to_string |> fun s -> 
        log_.log_lazy (fun () -> Printf.sprintf "sending %s\n" s);
        s |> Connection.send_string ~conn 
        |> function Error () -> exit_1 __LOC__ | Ok () -> 
          Connection.recv_string ~conn 
          |> function Error () -> exit_1 __LOC__ | Ok s -> 
            log_.log_lazy (fun () -> Printf.sprintf "receiving %s\n" s);
            s |> string_to_msg |> fun m -> 
            (w,Inl m))
end


(*
let run (type a) ~w_ref (a:a m) = 
  let w = w_ref in
  !w |> fun w' ->
  Step_monad.run ~dest_exceptional w' a |> function
  | `Exceptional(e,w'') -> 
    (* NOTE we don't update w since conn is mutable, internal error is
       None, and this error is just for the particular call *)
    raise (A_error.mk_unix_exn e)  (* FIXME? *)
  | `Finished(w'',a) -> w:=w''; a
*)

