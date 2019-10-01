(*
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

(* NOTE this is pure; NOTE exceptional states typically have
   rest=fun() -> fail, so we need to take care to call rest only when we
   are sure the state is not finished *)
let rec run ~dest_exceptional w x =
  let rec run w x = match x with
    | Finished y -> `Finished (w,y)
    | Step f -> 
      dest_exceptional w |> function
      | None -> f w |> run' 
      | Some e -> `Exceptional (e,w)
  and run' (w,rest) =
    dest_exceptional w |> function
    | None -> run w (rest())
    | Some e -> `Exceptional(e,w)
  in
  run w x

let _ : 
  dest_exceptional:('a -> 'b option) -> 'a -> ('c, 'a) m -> 
  [> `Exceptional of 'b * 'a | `Finished of 'a * 'c] 
  = run


let exit_1 s = print_endline s; exit (-1)

let failwith_step_error loc = 
  Printf.sprintf "Attempt to step exceptional state: %s\n" loc |> exit_1 
*)
