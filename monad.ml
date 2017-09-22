(** Simple state-passing monad *)

(** The monad type, a function from an initial state to a final state,
   and a possible result of type ['a] (or an error). *)
type ('a,'s) m = 's -> ('a,string) result * 's 

let bind : ('a -> ('b,'s) m) -> ('a,'s)m -> ('b,'s)m = fun f x s ->
  match x s with
  | (Ok y,s') -> f y s'
  | (Error e,s') -> (Error e,s')

let _ = bind

let return x = fun s -> (Ok x,s)

let err e = fun s -> (Error e,s)

let run s = fun m -> m s

