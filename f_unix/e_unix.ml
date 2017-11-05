open Tjr_either
open C_base

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
  let dh2i x = x |> dirfd |> fd2i
end


(* ops -------------------------------------------------------------- *)

type w = {
  error_state: exn option;
  world_state: unit
}

let initial_world : w = { error_state=None; world_state=() }

module Unix_monad = struct
  open Step_monad
  type 'a m = ('a,w)step_monad
  let bind,return = bind,return
end
include Unix_monad

(* generate types --------------------------------------------------- *)

module Ops_type = D_functors.Make_ops_type(Unix_monad)(Unix_base_types)
include Ops_type

module Ops_type_plus = struct
  include Unix_monad
  include Unix_base_types
  include Ops_type
end
      

module Imp_ops_type = D_functors.Make_imp_ops_type(Ops_type_plus)





(* construct ops ---------------------------------------------------- *)

(* pass-through to Unix.xxx *)


open C_base

let return = Step_monad.return

include struct
  open Unix
  type unix_error = Unix_error_ of error*string*string
end

type extra_ops = {
  safely: 'a 'e. (unix_error -> 'e) -> (w -> 'a m) -> ('a,'e)result m;  (* this delays until receives a world *)
}

let mk_ops ~extra = 

  let root : path = "/" in

  (* FIXME refine - at the moment we wrap all exns as EOTHER *)
  let safely a = extra.safely (fun e -> `EOTHER) a in

  let unlink ~parent ~name = 
    safely @@ fun _ ->
    Unix.unlink @@ parent^"/"^name ;
    return ()
  in


  let default_perm = 0o640 in


  let mkdir ~parent ~name = 
    safely @@ fun _ -> 
    Unix.mkdir (parent^"/"^name) default_perm;
    return ()
  in


  let mk_dh ~path = Unix.opendir path in

  let opendir path = 
    safely @@ fun _ -> return (mk_dh path)
  in


  let readdir dh = 
    safely @@ fun _ -> (* safely is just to delay till world available *)
    try Unix.readdir dh |> fun e -> return ([e],not finished)
    with _ -> return ([],finished)
  in


  let closedir dh = 
    safely @@ fun _ -> Unix.closedir dh; return ()
  in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name = 
    safely @@ fun _ -> 
    let open Unix in
    openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
    close fd;
    return ()
  in


  let mk_fd path = 
    safely @@ fun _ ->
    Unix.(openfile path [O_RDWR] default_perm) |> return
  in

  let open_ path = mk_fd path in


  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    safely @@ fun _ ->
    (* bigarray pread has no boff, and length is taken from array, so
       resort to slicing *)
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pread fd foff buffer |> fun nread ->
    return nread
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    safely @@ fun _ ->
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pwrite fd foff buffer |> fun n ->
    return n
  in


  let close fd = safely @@ fun _ ->
    (Unix.close fd; return ()) 
  in 
  (* FIXME record which are open? *)


  let rename ~spath ~sname ~dpath ~dname = safely @@ fun _ ->
    (Unix.rename (spath^"/"^sname) (dpath^"/"^dname); return ())
  in

  let truncate ~path ~length = 
    safely @@ fun _ -> 
    Unix.truncate path length; 
    return ()
  in


  let stat_file path = 
    safely @@ fun _ -> 
    let open Unix in
    stat path |> fun st ->
    st.st_size |> fun sz ->        
    return {sz}
  in


  let kind path = 
    safely @@ fun _ ->
    let open Unix in
    stat path |> fun st ->
    st.st_kind 
    |> (function
        | S_DIR -> (`Dir:st_kind)
        | S_REG -> (`File:st_kind)
        | _ -> `Other) 
    |> return
  in

    
  let reset () = return () in
  

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat_file; kind; reset }

let (>>=) = bind

let safely : 'a 'e. (unix_error -> 'e) -> (w -> 'a m) -> ('a,'e)result m = 
  let open Step_monad in
  fun g f -> 
    Step(fun w -> 
        try 
          f w |> fun a ->
          w,Inr (fmap (fun a -> Ok a) a)
        with 
        | Unix.Unix_error(e,s1,s2) -> (w,Inl (Error(g (Unix_error_(e,s1,s2))))))

let _ = safely

let extra = { safely }

let unix_ops = mk_ops ~extra 

let rec run (w:w) (x:'a m) = 
  (Step_monad.run ~dest_exceptional:(fun x -> None) w x) |> function
  | Ok (w,a) -> (w,a)
  | Error e -> failwith "impossible since no state is exceptional"

(* imperative ------------------------------------------------------- *)

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

