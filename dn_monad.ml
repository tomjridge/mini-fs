(* double negation monad; state passing *)
(*
#thread;;
#require "tjr_lib";;
*)

(* 'a m = ('a -> ww) -> ww, where ww = list of (w -> w) *)

type 'w trans = ('w -> 'w) list

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

let bind (x:('a,'w)m') (g:'a -> ('b,'w)m') : ('b,'w) m' = 
  fun (h : 'b -> 'w trans) ->  
    x @@ (fun a -> g a h)

let ( >>= ) = bind


(* example ---------------------------------------------------------- *)

module Example = functor (X_ : sig end) -> struct

  type w = int
  type 'a m = ('a -> w trans) -> w trans

  let inc : unit m = fun (k:unit -> w trans) -> 
    let ww = fun w -> 
      Printf.printf "incrementing %d" w;
      w+1
    in
    [ww]@k()


  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run (comp:'a m) (w:w) = 

    (* ignore the final value; comp should take a unit m? *)
    let xs : w trans = comp (fun (x:'a) -> []) in  

    Tjr_list.with_each_elt ~step:(fun ~state f -> f state) ~init_state:w xs


  let _ = run (inc >>= fun () -> inc >>= fun () -> inc) 0

end



(* example with error ----------------------------------------------- *)


module Example2 = functor (X_ : sig end) -> struct

  type w = int
  type 'a m = ('a -> w trans) -> w trans

  (* the exceptional state *)
  let is_exceptional = fun w -> w = 1

  let inc : unit m = fun (k:unit -> w trans) -> 
    let ww = fun w -> 
      Printf.printf "incrementing %d" w;
      w+1
    in
    [ww]@k()


  (* FIXME note that for "with error" we stop when we hit an exceptional state *)
  let run (comp:'a m) (w:w) = 

    (* ignore the final value; comp should take a unit m? *)
    let xs : w trans = comp (fun (x:'a) -> []) in  

    Tjr_list.with_each_elt 
      ~step:(fun ~state f -> if is_exceptional state then state else f state)
      ~init_state:w 
      xs


  let _ = run (inc >>= fun () -> inc >>= fun () -> inc) 0

end
