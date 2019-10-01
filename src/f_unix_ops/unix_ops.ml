(* FIXME this should be called "wrap_local_filesystem" or similar *)

(* open Tjr_monad *)
(* open Tjr_monad.Monad *)
(* open Tjr_either *)
open Base_
open Ops_type_

module Unix_base_types = struct
  type fd=Unix.file_descr
  type dh=Unix.dir_handle
(*  let fd2int x = ExtUnix.All.int_of_file_descr x *)
end
include Unix_base_types


(* generate types --------------------------------------------------- *)



(* construct ops ---------------------------------------------------- *)

(* pass-through to Unix.xxx *)

type ('w,'t) extra_ops = {
  delay: 'a. ('w -> ('a,'t) m) -> ('a,'t) m;  
  (* this delays until receives a world *)
}

let _EOTHER = Error `Error_other

(* return errors that we recognize, otherwise pass to an aux f *)
(* for the time being, we typically just map EINVAL to return _EOTHER *)
let map_error' ~monad_ops (f : [ `EINVAL ] -> 'a) e =
  let return = monad_ops.return in
  Error_.map_error e |> function
  | Inl e -> return (Error e)
  | Inr e -> match e with
    | `EINVAL -> f `EINVAL
    | `SOME_OTHER_ERROR -> return _EOTHER

let _ = map_error'


let mk_ops ~monad_ops ~extra = 

  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in

  (* FIXME refine behaviour in following *)
  let handle_EINVAL () = monad_ops.return _EOTHER in

  let _ = handle_EINVAL in

  let map_error f e = map_error' ~monad_ops f e in

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
        | `Symlink -> (
          log_.log_now __LOC__;
          exit_1 __LOC__ 
          (* FIXME should be impossible since stat resolves symlinks; but is this what we want? *))
        | _ -> (
          log_.log_now __LOC__;
          exit_1 __LOC__)
      end;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function 
      | `EINVAL -> handle_EINVAL ()
        (* posix: can't be thrown? check with SibylFS *)
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
      | `EINVAL -> handle_EINVAL ()
  in


  let mk_dh ~path = Unix.opendir path in

  let opendir path = 
    delay @@ fun _ ->
    try 
      return (Ok (mk_dh path))
    with 
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
  in


  let readdir dh = 
    delay @@ fun _ ->
    try Unix.readdir dh |> fun e -> return (Ok([e],not finished))
    with
    | End_of_file -> return (Ok([],finished))
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
  in


  let closedir dh = 
    delay @@ fun _ ->
    try 
      Unix.closedir dh; return (Ok())
    with
    | Unix.Unix_error(e,_,_) ->
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
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
      | `EINVAL -> handle_EINVAL ()
  in


  let mk_fd path = 
    delay @@ fun _ ->
    try 
      Unix.(openfile path [O_RDWR] default_file_perm) |> fun x -> return (Ok x)
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
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
      | `EINVAL -> handle_EINVAL ()
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
      | `EINVAL -> handle_EINVAL ()
  in


  let close fd = 
    delay @@ fun _ ->
    try 
      Unix.close fd; return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> return _EOTHER
  in 
  (* FIXME record which fd are open? *)


  let rename src dst = 
    delay @@ fun _ ->
    try
      Unix.rename src dst;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
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
      | `EINVAL -> handle_EINVAL ()
  in

  let symlink contents path =
    delay @@ fun _ ->
    try
      Unix.symlink contents path;
      return (Ok())
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
  in

  let readlink path = 
    delay @@ fun _ ->
    try
      Unix.readlink path |> fun contents ->
      return (Ok contents)
    with
    | Unix.Unix_error(e,_,_) -> 
      e |> map_error @@ function
      | `EINVAL -> handle_EINVAL ()
  in

  let reset () = return () in


  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat; symlink; readlink; reset }


let unix_ops ~monad_ops () =
  (* define within unix_ops, otherwise an error about type vars that
     cannot be generalized *)
  let open State_passing_instance in
  let monad_ops = monad_ops () in
      
  let ( >>= ) = monad_ops.bind in

  let delay : 'a. ('w -> ('a,'w state_passing) m) -> ('a,'w state_passing) m =
    fun f -> 
      with_world(fun w -> w,w) >>= f 
  in

  let extra = { delay } in

  let unix_ops = mk_ops ~monad_ops ~extra in

  let _ : (fd,dh,'w state_passing) ops = unix_ops in
  unix_ops
  

