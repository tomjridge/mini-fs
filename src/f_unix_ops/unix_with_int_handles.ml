(* version of unix that keeps track of an int<->fd/dh map *)

(* when we allocate an fd or dh, we tag it with a genint, then insert
   (with key the genint) into a map; subsequently, we only pass the
   genint to the client *)

open Tjr_map
open Base_

module Base_types = Int_base_types

type w = {
  (* ew: Unix_ops.w; this is currently unit, so we omit it here *)
  int2fd: Unix.file_descr Map_int.t;
  int2dh: Unix.dir_handle Map_int.t;
}

let initial_world : w = 
  { int2fd=Map_int.empty; int2dh=Map_int.empty }

let genint = Tjr_gensym.gensym

module Monad = struct
  open Step_monad
  type 'a m = ('a,w)step_monad
  let bind,return = bind,return
end
include Monad

module MBR = struct
  include Monad
  include Base_types
  include R_as_result
end

(* generate types --------------------------------------------------- *)


module This = struct
  module Ops_type = Ops_types.Make_ops_type(MBR)
  include Ops_type

  module Ops_type_plus = struct
    include MBR
    include Ops_type
  end
end
include This



(* lift 'a Unix_ops.m to 'a m *)

include struct
  open Tjr_either
  open Step_monad
  let run w a = 
    Unix_ops.run Unix_ops.the_world a |> function
    | Ok (w,a) -> a
    | Error (`Attempt_to_step_exceptional_state _) -> failwith __LOC__

  let _ = run

  let lift: 'a Unix_ops.m -> 'a m = fun a ->
    Step(fun w -> w,Inl (run w a))

end

let ops = Unix_ops.unix_ops

let ops ~dh2i ~i2dh ~fd2i ~i2fd = 
  let open Unix_ops in
  let ( >>=| ) a b = a >>= function Error e -> return (Error e) | Ok a -> b a in
  let root = ops.root in

  let unlink ~parent ~name = ops.unlink ~parent ~name |> lift in

  let _ = unlink in

  let mkdir ~parent ~name = ops.mkdir ~parent ~name |> lift in
  
  let opendir path = lift(ops.opendir path) >>=| fun dh ->
    dh2i dh >>= fun i -> return (Ok i)
  in

  let readdir dh = i2dh dh >>= fun dh -> lift (ops.readdir dh) in
  
  let closedir dh = i2dh dh >>= fun dh -> lift (ops.closedir dh) in

  let create ~parent ~name = ops.create ~parent ~name |> lift in
  
  let open_ path = lift (ops.open_ path) >>=| fun fd -> 
    fd2i fd >>= fun i ->
    return (Ok i)
  in

  let pread ~fd ~foff ~length ~buffer ~boff = 
    i2fd fd >>= fun fd ->
    ops.pread ~fd ~foff ~length ~buffer ~boff |> lift
  in

  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    i2fd fd >>= fun fd ->
    ops.pwrite ~fd ~foff ~length ~buffer ~boff |> lift
  in  

  let close fd = i2fd fd >>= fun fd -> ops.close fd |> lift in

  let rename ~spath ~sname ~dpath ~dname = 
    ops.rename ~spath ~sname ~dpath ~dname |> lift 
  in

  let truncate ~path ~length = ops.truncate ~path ~length |> lift in

  let stat path = ops.stat path |> lift in

  let reset () = ops.reset () |> lift in

  let open This.Ops_type in
  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; reset }
  
(* aux funs dh2i etc ------------------------------------------------ *)

include struct
  open Tjr_either
  open Step_monad
  let dh2i dh = Step(fun w ->
      (* allocate int, add i,dh to map, and return i *)
      genint() |> fun i ->
      {w with int2dh=Map_int.add i dh w.int2dh} |> fun w ->
      w,Inl i)
  let i2dh i = Step(fun w->
      w,Inl (Map_int.find i w.int2dh))  (* FIXME if missing? *)
  let fd2i fd = Step(fun w ->
      (* allocate int, add i,dh to map, and return i *)
      genint() |> fun i ->
      {w with int2fd=Map_int.add i fd w.int2fd} |> fun w ->
      w,Inl i)
  let i2fd i = Step(fun w->
      w,Inl (Map_int.find i w.int2fd))  (* FIXME if missing? *)
end

let ops = ops ~dh2i ~i2dh ~fd2i ~i2fd
