(* version of unix that keeps track of an int<->fd/dh map *)

(* when we allocate an fd or dh, we tag it with a genint, then insert
   (with key the genint) into a map; subsequently, we only pass the
   genint to the client *)

open Tjr_map
open Base_

module Base_types = Int_base_types

type fd_dh_map = {
  int2fd: Unix.file_descr Map_int.t;
  int2dh: Unix.dir_handle Map_int.t;
}

(* we assume we can get the fd_dh_map from the world 'w *)

let genint = Tjr_gensym.gensym


(* target ops_type ----------------------------------------------- *)

(* this is what we aim to provide *)

type 'w ops_type = (int,int,'w) Ops_types.Ops_type_with_result.ops

open Unix_ops

let ops ~dh2i ~i2dh ~fd2i ~i2fd = 
  let open Unix_ops in
  let ( >>=| ) a b = a >>= function Error e -> return (Error e) | Ok a -> b a in
  let ops = unix_ops () in
  let root = ops.root in

  let unlink path = ops.unlink path in

  let _ = unlink in

  let mkdir path = ops.mkdir path in
  
  let opendir path = ops.opendir path >>=| fun dh ->
    dh2i dh >>= fun i -> return (Ok i)
  in

  let readdir dh = i2dh dh >>= fun dh -> ops.readdir dh in
  
  let closedir dh = i2dh dh >>= fun dh -> ops.closedir dh in

  let create path = ops.create path in
  
  let open_ path = ops.open_ path >>=| fun fd -> 
    fd2i fd >>= fun i ->
    return (Ok i)
  in

  let pread ~fd ~foff ~length ~buffer ~boff = 
    i2fd fd >>= fun fd ->
    ops.pread ~fd ~foff ~length ~buffer ~boff 
  in

  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    i2fd fd >>= fun fd ->
    ops.pwrite ~fd ~foff ~length ~buffer ~boff 
  in  

  let close fd = i2fd fd >>= fun fd -> ops.close fd in

  let rename src dst = ops.rename src dst in

  let truncate ~path ~length = ops.truncate ~path ~length in

  let stat path = ops.stat path in

  let reset () = ops.reset () in

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; reset }
  


(* aux funs dh2i etc ------------------------------------------------ *)

include struct
  open Tjr_step_monad
  open Tjr_step_monad.Step_monad_implementation
  let dh2i dh = Step(fun w ->
      (* allocate int, add i,dh to map, and return i *)
      genint() |> fun i ->
      {w with int2dh=Map_int.add i dh w.int2dh} |> fun w ->
      w,`Inl i)
  let i2dh i = Step(fun w->
      w,`Inl (Map_int.find i w.int2dh))  (* FIXME if missing? *)
  let fd2i fd = Step(fun w ->
      (* allocate int, add i,dh to map, and return i *)
      genint() |> fun i ->
      {w with int2fd=Map_int.add i fd w.int2fd} |> fun w ->
      w,`Inl i)
  let i2fd i = Step(fun w->
      w,`Inl (Map_int.find i w.int2fd))  (* FIXME if missing? *)
end

let ops = ops ~dh2i ~i2dh ~fd2i ~i2fd

(* NOTE this assumes the world is just the map - FIXME probably want
   to generalize this a bit by providing functions to get/set the map
   in the world state *)
let _ : (int,int,fd_dh_map) ops = ops
