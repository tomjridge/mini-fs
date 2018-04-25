(* FIXME this should be called "wrap_local_filesystem" or similar *)

open Tjr_step_monad
open Tjr_either
open Base_
open Ops_types

module Unix_base_types = struct
  type fd=Unix.file_descr
  type dh=Unix.dir_handle
(*  let fd2int x = ExtUnix.All.int_of_file_descr x *)
end
include Unix_base_types


(*
module Unix_conversions = struct
  open ExtUnix.All
  let fd2i = int_of_file_descr
  let i2fd = file_descr_of_int
  (* NOTE the following uses dirfd which is fragile; better to
     expose a version of the api that tracks a dh<->int bijection *)
  (* let dh2i x = x |> dirfd |> fd2i *)
end
*)


(* generate types --------------------------------------------------- *)

module Ops_type = Ops_types.Ops_type_with_result
include Ops_type

(* construct ops ---------------------------------------------------- *)

(* pass-through to Unix.xxx *)

let return = Tjr_step_monad.return

(*
type 'e extra_ops = {
  safely: 'a. (w -> 'a m) -> ('a,'e)result m;  
  (* this delays until receives a world *)
}
*)

type 'w extra_ops = {
  delay: 'a. ('w -> ('a,'w) m) -> ('a,'w) m;  
  (* this delays until receives a world *)
}

let _EOTHER = Error `Error_other

(* return errors that we recognize, otherwise pass to an aux f *)
let map_error (f : [ `EINVAL ] -> 'a) e =
  Error_.map_error e |> function
  | Inl e -> return (Error e)
  | Inr e -> match e with
    | `EINVAL -> f `EINVAL
    | `SOME_OTHER_ERROR -> return _EOTHER

let mk_ops ~extra = 

  let delay = extra.delay in

  let root : path = "/" in

  (* FIXME refine - at the moment we wrap all exns as EOTHER *)
  (* let safely a = extra.safely (fun e -> Ops_types.unix2err e) a in *)

  (* FIXME unlink usually operates only on files; do we want to have
     rmdir as well? *)
  let unlink path = 
    let open Unix in
    delay @@ fun _ ->
    try 
      stat path |> fun st ->
      begin
        st.st_kind |> unix2kind |> function
        | `File -> Unix.unlink path
        | `Dir -> Unix.rmdir path
        | `Symlink -> failwith __LOC__ (* FIXME *)
        | _ -> failwith __LOC__
      end;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function 
      | `EINVAL ->
        failwith __LOC__ (* posix: can't be thrown? check with SibylFS *)
  in


  let default_file_perm = 0o640 in  (* u:rw g:r o: *)
  let default_dir_perm = 0o775 in  (* u:rwx g:rwx o:rx *)


  let mkdir path = 
    delay @@ fun _ ->
    try 
      Unix.mkdir path default_dir_perm;
      return (Ok ())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in


  let mk_dh ~path = Unix.opendir path in

  let opendir path = 
    delay @@ fun _ ->
    try 
      return (Ok (mk_dh path))
    with 
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in


  let readdir dh = 
    delay @@ fun _ ->
    try Unix.readdir dh |> fun e -> return (Ok([e],not finished))
    with
    | End_of_file -> return (Ok([],finished))
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in


  let closedir dh = 
    delay @@ fun _ ->
    try 
      Unix.closedir dh; return (Ok())
    with
    | Unix.Unix_error(e,_,_) ->
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create path = 
    delay @@ fun _ ->
    try 
      let open Unix in
      openfile path [O_CREAT] default_file_perm |> fun fd ->
      close fd;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in


  let mk_fd path = 
    delay @@ fun _ ->
    try 
      Unix.(openfile path [O_RDWR] default_file_perm) |> fun x -> return (Ok x)
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
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
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    delay @@ fun _ ->
    try 
      Bigarray.Array1.sub buffer boff length |> fun buffer -> 
      ExtUnix.All.BA.pwrite fd foff buffer |> fun n ->
      return (Ok n)
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in


  let close fd = 
    delay @@ fun _ ->
    try 
      Unix.close fd; return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return _EOTHER
  in 
  (* FIXME record which are open? *)


  let rename src dst = 
    delay @@ fun _ ->
    try
      Unix.rename src dst;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in

  let truncate ~path ~length = 
    delay @@ fun _ ->
    try
      Unix.truncate path length; 
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> return _EOTHER  (* length < 0 *)
  in


  let stat path = 
    delay @@ fun _ ->
    try
      let open Unix.LargeFile in
      stat path |> unix2stat |> fun stat -> 
      return (Ok stat)
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> failwith __LOC__
  in



  let reset () = return () in


  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; reset }

let (>>=) = fun a ab -> bind ab a

let delay : 'a. ('w -> ('a,'w) m) -> ('a,'w) m =
  let open Tjr_step_monad.Step_monad_implementation in
  fun f -> 
    Step(fun w -> w,`Inl w) >>= f

let extra = { delay }

let unix_ops () = mk_ops ~extra 

let _ : unit -> (fd,dh,'w) ops = unix_ops

let run w x = Tjr_step_monad.Extra.run w x

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
