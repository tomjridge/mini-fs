(* double negation monad; state passing *)
(*
#thread;;
#require "tjr_lib";;
*)

(* 'a m = ('a -> ww) -> ww, where ww = (lazy)list of (w -> w) *)

(* coinductive *)
type 'w trans = Finished | Step of ('w -> 'w * (unit -> 'w trans))

type ('a,'w) m' = ('a -> 'w trans) -> 'w trans

module X_ = struct

  type w

  type 'a m = ('a -> w trans) -> w trans

  let return (x:'a) : 'a m = fun (k:('a -> w trans)) -> k x

  let bind (x:'a m) (g: 'a -> 'b m) : 'b m = 
    fun (h : 'b -> 'w trans) ->  
      x @@ (fun a -> g a h)

end

let return x : ('a,'w) m' = fun k -> k x

(* FIXME *)
let bind (x:('a,'w)m') (g:'a -> ('b,'w)m') : ('b,'w) m' = 
  fun (h : 'b -> 'w trans) ->  
    x @@ (fun a -> g a h)

let ( >>= ) = bind

(*
let with_state f : (unit,'w) m' = fun (k:unit -> 'w trans) -> 
  Step(fun w -> f w,k)
*)

let with_state (f:'w -> 'a * 'w) (g: 'a -> ('b,'w)m') : ('b,'w)m' = 
  fun (h: 'b -> 'w trans) -> 
  Step(fun w -> 
      f w |> fun (w',a) ->
      w',fun () -> g a h)


(* example ---------------------------------------------------------- *)

module Example = functor (X_ : sig end) -> struct

  type w = int
  type 'a m = ('a -> w trans) -> w trans

  let inc : unit m = fun (k:unit -> w trans) -> 
    let ww = fun w -> 
      Printf.printf "incrementing %d" w;
      w+1
    in
    Step (fun w -> ww w,k)


  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run ~(state:w) ~(code:'a m) =
    (code @@ fun a -> Finished) |> fun trans ->
    let rec run ~(state:w) ~(trans:w trans) = 
      match trans with
      | Finished -> state
      | Step(f) ->
        f state |> fun (state,rest) -> run ~state ~trans:(rest())
    in
    run ~state ~trans
  
  let _ : state:w -> code:'a m -> w = run

  let code = inc >>= fun () -> inc >>= fun () -> inc

  let _ : unit m = code

  let _ = run ~code ~state:0

end



(* example with error ----------------------------------------------- *)


module Example2 = functor (X_ : sig end) -> struct

  module Example = Example(X_)
  open Example

  (* the exceptional state *)
  let is_exceptional = fun w -> w = 1


  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run ~(state:w) ~(code:'a m) =
    (code @@ fun a -> Finished) |> fun trans ->
    let rec run ~(state:w) ~(trans:w trans) = 
      match is_exceptional state with
      | true -> state 
      | false -> 
        match trans with
        | Finished -> state
        | Step(f) ->
          f state |> fun (state,rest) -> run ~state ~trans:(rest())
    in
    run ~state ~trans

  let _ = run ~code ~state:0

end
