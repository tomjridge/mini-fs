(* double negation monad; state passing *)
(*
#thread;;
#require "tjr_lib";;
*)

(* 'a m = ('a -> ww) -> ww, where ww = (lazy)list of (w -> w) *)

(* coinductive *)
type ('a,'w) trans = Finished of 'a | Step of ('w -> 'w * (unit -> ('a,'w) trans))

type ('a,'w) m' = ('a -> ('a,'w) trans) -> ('a,'w) trans

module X_ = struct

  type w

  type 'a m = ('a -> ('a,w) trans) -> ('a,w) trans

  let return (x:'a) : 'a m = fun (k:('a -> ('a,w) trans)) -> k x

  let bind (x:'a m) (g: 'a -> 'b m) : 'b m = 
    fun (h : 'b -> ('a,'w) trans) ->  
      x @@ (fun a -> g a h)

end

let return x : ('a,'w) m' = fun k -> k x

(* FIXME *)
let bind (x:('a,'w)m') (g:'a -> ('b,'w)m') : ('b,'w) m' = 
  fun (h : 'b -> ('b,'w) trans) ->  
    x @@ (fun a -> g a h)

let ( >>= ) = bind

(*
let with_state f : (unit,'w) m' = fun (k:unit -> 'w trans) -> 
  Step(fun w -> f w,k)
*)

let with_state (type a w b) (f:w -> a * w) (g: a -> (b,w)m') : (b,w)m' = 
  fun (h: b -> (b,w) trans) -> 
  Step(fun w -> 
      f w |> fun (a,w') ->
      w',fun () -> g a h)


let _ = with_state

(* example ---------------------------------------------------------- *)

module Example = functor (X_ : sig end) -> struct

  type w = int
  type 'a m = ('a -> ('a,w) trans) -> ('a,w) trans

  let inc : unit m = fun (k:unit -> (unit,w) trans) -> 
    let ww = fun w -> 
      Printf.printf "incrementing %d" w;
      w+1
    in
    Step (fun w -> ww w,k)


  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run ~(state:w) ~(code:'a m) =
    (code @@ fun a -> Finished a) |> fun trans ->
    let rec run ~(state:w) ~(trans:('a,w) trans) = 
      match trans with
      | Finished a -> (a,state)
      | Step(f) ->
        f state |> fun (state,rest) -> run ~state ~trans:(rest())
    in
    run ~state ~trans
  
  let _ : state:w -> code:'a m -> ('a * w) = run

  let code = inc >>= fun () -> inc >>= fun () -> inc

  let _ : unit m = code

  let _ = run ~code ~state:0

end



(* example with error ----------------------------------------------- *)


module Example2 = functor (X_ : sig end) -> struct

  module Example = Example(X_)
  open Example

  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run ~(is_exceptional:w -> bool) ~(state:w) ~(code:'a m) =
    (code @@ fun a -> Finished a) |> fun trans ->
    let rec run ~(state:w) ~(trans:('a,w) trans) = 
      match is_exceptional state with
      | true -> (None,state)
      | false -> 
        match trans with
        | Finished a -> (Some a,state)
        | Step(f) ->
          f state |> fun (state,rest) -> run ~state ~trans:(rest())
    in
    run ~state ~trans

  (* the exceptional state *)
  let is_exceptional = fun w -> w = 1

  (* following stops when w is 1 *)
  let _ = run ~code ~state:0

end
