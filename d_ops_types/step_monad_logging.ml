(* log calls and returns -------------------------------------------- *)

open Tjr_either
open Base_
open Msgs
open Step_monad

(* FIXME should also log exceptional returns *)
let log ~log_string msg = 
  let call = msg |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string in
  let rec log_return a = 
    a |> dest_Step |> fun a ->
    Step(fun w ->
        a w |> fun (w',rest) ->
        (w', 
         match rest with
         | Inl a -> (
             log_string (fun () -> Printf.sprintf "call %s returns\n" call);
             Inl a)
         | Inr a -> Inr(log_return a)))
  in
  let log_call_and_return a = Step(
      fun w -> 
        log_string (fun () -> Printf.sprintf "call %s starts\n" call);
        w,Inr (log_return a))
  in
  log_call_and_return
