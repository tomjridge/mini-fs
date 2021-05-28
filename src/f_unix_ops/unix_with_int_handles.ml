(** Version of unix ops that keeps track of an int<->fd/dh map *)

(** When we allocate an fd or dh, we tag it with a genint, then insert
   (with key the genint) into a map; subsequently, we only pass the
   genint to the client *)

(* FIXME parameterization overkill? FIXME hacky implementation *)


open Minifs_intf
(* open Ops_type_ *)

module Base_types = Int_base_types

type fd_dh_map = {
  int2fd: Unix.file_descr Map_int.t;
  int2dh: Unix.dir_handle Map_int.t;
}

let init_fd_dh_map = {
  int2fd=Map_int.empty;
  int2dh=Map_int.empty
}

(* we assume we can get the fd_dh_map from the world 'w *)

let genint () = Gensym.gensym ()


(* target ops_type ----------------------------------------------- *)

(* this is what we aim to provide *)

type 'w ops_type = (int,int,'w) ops

(* open Unix_ops *)

let ops ~monad_ops ~dh2i ~i2dh ~fd2i ~i2fd = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let open Unix_ops in
  let ( >>=| ) a b = a >>= function Error e -> return (Error e) | Ok a -> b a in
  let ops = unix_ops ~monad_ops () in 
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

  let pread ~fd ~foff ~len ~buf ~boff = 
    i2fd fd >>= fun fd ->
    ops.pread ~fd ~foff ~len ~buf ~boff 
  in

  let pwrite ~fd ~foff ~len ~(buf:ba_buf) ~boff = 
    i2fd fd >>= fun fd ->
    ops.pwrite ~fd ~foff ~len ~buf ~boff 
  in  

  let close fd = i2fd fd >>= fun fd -> ops.close fd in

  let rename src dst = ops.rename src dst in

  let truncate path length = ops.truncate path length in

  let stat path = ops.stat path in

  let symlink contents path = ops.symlink contents path in

  let readlink path = ops.readlink path in

  let reset () = ops.reset () in

  let ops : (_,_,_)ops = 
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat; symlink; readlink; reset }
  in

  ops
  


(* aux funs dh2i etc ------------------------------------------------ *)

include struct
  open State_passing
  let dh2i dh = of_fun(fun w ->
      (* allocate int, add i,dh to map, and return i *)
      genint() |> fun i ->
      {w with int2dh=Map_int.add i dh w.int2dh} |> fun w ->
      i,w)
  let i2dh i = of_fun(fun w->
      (Map_int.find i w.int2dh),w)  (* FIXME if missing? *)
  let fd2i fd = of_fun(fun w ->
      (* allocate int, add i,dh to map, and return i *)
      genint() |> fun i ->
      {w with int2fd=Map_int.add i fd w.int2fd} |> fun w ->
      i,w)
  let i2fd i = of_fun(fun w->
      (Map_int.find i w.int2fd),w)  (* FIXME if missing? *)
  let monad_ops = monad_ops ()
end

let ops = ops ~monad_ops ~dh2i ~i2dh ~fd2i ~i2fd

(* NOTE this assumes the world is just the map - FIXME probably want
   to generalize this a bit by providing functions to get/set the map
   in the world state *)
let _ : (int,int,fd_dh_map state_passing) ops = ops
