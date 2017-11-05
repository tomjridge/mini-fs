open Tjr_either
open Base_
open Ops_types

module Unix_base_types = struct
  type fd=Unix.file_descr
  type dh=Unix.dir_handle
(*  let fd2int x = ExtUnix.All.int_of_file_descr x *)
end
include Unix_base_types


module Unix_conversions = struct
  open ExtUnix.All
  let fd2i = int_of_file_descr
  let i2fd = file_descr_of_int
  (* NOTE the following uses dirfd which is fragile; better to
     expose a version of the api that tracks a dh<->int bijection *)
  (* let dh2i x = x |> dirfd |> fd2i *)
end


(* ops -------------------------------------------------------------- *)

type w = {
  world_state: unit
}

let initial_world : w = { world_state=() }

module Unix_monad = struct
  open Step_monad
  type 'a m = ('a,w)step_monad
  let bind,return = bind,return
end
include Unix_monad


(* generate types --------------------------------------------------- *)

module MBR = struct
  include Unix_monad
  include Unix_base_types
  type ('a,'e) r_ = ('a,'e)result
end

module Ops_type = Make_ops_type(MBR)
include Ops_type

module Ops_type_plus = struct
  include MBR
  include Ops_type
end
      

(* module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus) *)





(* construct ops ---------------------------------------------------- *)

(* pass-through to Unix.xxx *)

let return = Step_monad.return

(*
type 'e extra_ops = {
  safely: 'a. (w -> 'a m) -> ('a,'e)result m;  
  (* this delays until receives a world *)
}
*)

type 'e extra_ops = {
  delay: 'a. (w -> 'a m) -> 'a m;  
  (* this delays until receives a world *)
}


let mk_ops ~extra = 

  let delay = extra.delay in

  let root : path = "/" in

  (* FIXME refine - at the moment we wrap all exns as EOTHER *)
  (* let safely a = extra.safely (fun e -> Ops_types.unix2err e) a in *)
  
  let unlink ~parent ~name = 
    delay @@ fun _ ->
    try
      Unix.unlink @@ parent^"/"^name ;
      return (Ok ())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)

  in


  let default_perm = 0o640 in


  let mkdir ~parent ~name = 
    delay @@ fun _ ->
    try 
      Unix.mkdir (parent^"/"^name) default_perm;
      return (Ok ())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let mk_dh ~path = Unix.opendir path in

  let opendir path = 
    delay @@ fun _ ->
    return (Ok (mk_dh path))
  in


  let readdir dh = 
    delay @@ fun _ ->
    try Unix.readdir dh |> fun e -> return (Ok([e],not finished))
    with
    | End_of_file -> return (Ok([],finished))
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let closedir dh = 
    delay @@ fun _ ->
    try 
      Unix.closedir dh; return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name = 
    delay @@ fun _ ->
    try 
      let open Unix in
      openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
      close fd;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let mk_fd path = 
    delay @@ fun _ ->
    try 
      Unix.(openfile path [O_RDWR] default_perm) |> fun x -> return (Ok x)
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in

  let open_ path = mk_fd path in


  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    delay @@ fun _ ->
    try
      (* bigarray pread has no boff, and length is taken from array, so
       resort to slicing *)
      Bigarray.Array1.sub buffer boff length |> fun buffer -> 
      ExtUnix.All.BA.pread fd foff buffer |> fun nread ->
      return (Ok nread)
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    delay @@ fun _ ->
    try 
      Bigarray.Array1.sub buffer boff length |> fun buffer -> 
      ExtUnix.All.BA.pwrite fd foff buffer |> fun n ->
      return (Ok n)
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let close fd = 
    delay @@ fun _ ->
    try 
      Unix.close fd; return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in 
  (* FIXME record which are open? *)


  let rename ~spath ~sname ~dpath ~dname = 
    delay @@ fun _ ->
    try
      Unix.rename (spath^"/"^sname) (dpath^"/"^dname); 
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in

  let truncate ~path ~length = 
    delay @@ fun _ ->
    try
      Unix.truncate path length; 
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let stat_file path = 
    delay @@ fun _ ->
    try
      let open Unix in
      stat path |> fun st ->
      st.st_size |> fun sz ->        
      return (Ok{sz})
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in


  let kind path = 
    delay @@ fun _ ->
    try
      let open Unix in
      stat path |> fun st ->
      st.st_kind 
      |> (function
          | S_DIR -> (`Dir:st_kind)
          | S_REG -> (`File:st_kind)
          | _ -> `Other) 
      |> fun x -> return (Ok x)
    with
    | Unix.Unix_error(e,_,_) -> return (Error `EOTHER)
  in

    
  let reset () = return () in
  

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat_file; kind; reset }

let (>>=) = bind

let delay : 'a. (w -> 'a m) -> 'a m =
  let open Step_monad in
  fun f -> 
    Step(fun w -> w,Inl w) >>= f

let extra = { delay }

let unix_ops = mk_ops ~extra 

let _ = unix_ops


(* imperative ------------------------------------------------------- *)

(*

let rec run (w:w) (x:'a m) = 
  (Step_monad.run ~dest_exceptional:(fun x -> None) w x) |> function
  | Ok (w,a) -> (w,a)
  | Error e -> failwith "impossible since no state is exceptional"

let dest_exceptional w = w.error_state 

include struct
  open Imp_ops_type

  let run ~ref_ a = run (!ref_) a |> fun (w,a) -> ref_:=w; a |> function
    | Ok a -> a
    | Error e -> failwith __LOC__


  let ref_ = ref initial_world
      
  let run x = run ~ref_ x

  let run : run = { run }

  let unix_imperative_ops = mk_imperative_ops ~run ~ops:unix_ops

  let _ : imp_ops = unix_imperative_ops

end

*)
