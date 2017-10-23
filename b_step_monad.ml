type ('a,'w) m = Finished of 'a | Step of ('w -> 'w * (unit -> ('a,'w) m))

let return (x:'a) : ('a,'w) m = Finished x

let rec bind (x:('a,'w)m) (f:'a -> ('b,'w)m) : ('b,'w)m = 
  match x with 
  | Finished a -> f a
  | Step g -> Step(
      fun w -> 
        g w |> fun (w',rest) -> 
        (w',fun () -> bind (rest()) f))

let ( >>= ) = bind

let with_state (type a w b) (f:w -> a*w) (g:a->(b,w)m) : (b,w)m = 
  Step(
    fun w ->
      f w |> fun (a,w') ->
      w',fun () -> g a)

(* NOTE this is pure *)
let rec run ~dest_exceptional w x =
  let rec run w x = match x with
    | Finished y -> `Finished (w,y)
    | Step f -> 
      dest_exceptional w |> function
      | None ->
        f w |> fun (w',rest) ->        
        run w' (rest())
      | Some e -> 
        `Exceptional (e,w)
  in
  run w x


(*
(* if dest_exceptional<>None, raise an OCaml exception; otherwise
   update world ref and keep going *)
let run ~dest_exceptional ~e2s w x =
  let rec run x = match x with
    | Finished y -> y
    | Step f -> 
      dest_exceptional !w |> function
      | None ->
        f !w |> fun (w',rest) ->
        w:=w';
        run (rest())
      | Some e -> 
        Printf.printf "smonad.34: run resulted in exceptional state: %s\n" (e2s e);
        failwith __LOC__
  in
  run x

let _ : dest_exceptional:('a -> 'b option) -> e2s:('b -> string) -> 
  'a ref -> ('c, 'a) m -> 'c = run
*)
