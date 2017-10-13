(* similar to dn monad, but simpler *)

type ('a,'w) m = Finished of 'a | Step of ('w -> 'w * (unit -> ('a,'w) m))

let return (x:'a) : ('a,'w) m = Finished x

let rec bind (x:('a,'w)m) (f:'a -> ('b,'w)m) : ('b,'w)m = 
  match x with 
  | Finished a -> f a
  | Step g -> Step(fun w -> g w |> fun (w',rest) -> (w',fun () -> bind (rest()) f))

let ( >>= ) = bind

let with_state (type a w b) (f:w -> a*w) (g:a->(b,w)m) : (b,w)m = 
  Step(fun w ->
      f w |> fun (a,w') ->
      w',fun () -> g a)



(* state passing with error *)

type ('e,'w,'m) monad_ops = {
  return: 'a. 'a -> ('a,'m)m;
  bind: 'a 'b. ('a,'m)m -> ('a -> ('b,'m)m) -> ('b,'m)m;
  err: 'a. 'e -> ('a,'m)m;
}

