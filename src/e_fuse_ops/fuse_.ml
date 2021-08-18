(** Construct [Fuse.operations] given a backend FS ops *)

open Minifs_intf
(* open Ops_type_ *)

open Fuse

(* FIXME wrap operations so they return unix_error *)

(** NOTE hidden defns of mk_fuse_ops *)

(**/**)
let mk_fuse_ops ~monad_ops ~readdir' ~(ops:('fd,'dh,'w)ops) ~co_eta = 

  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in

  (* propagate errors *)
  let ( >>=| ) a b = a >>= function Error e -> return (Error e) | Ok a -> b a in

  let co_eta = co_eta.co_eta in

  let unlink path = ops.unlink path in

  let mkdir path _perms = ops.mkdir path in

  (* opendir and closedir omitted *)
  (* create combined with fopen *)


  (* let readdir' = Readdir'.readdir' ~ops in *)
  let readdir path _ = readdir' path in

  (* FIXME tricky combining create with fopen *)
  (* NOTE that fuse keeps track of fds, so this just returns None;
     read and write are via path *)
  let fopen (path:string) flags = 
    (* log_.log @@ "# fopen "^ path ^ " mfuse.fopen.l61"; *)
    Unix.(List.mem O_CREAT flags) |> function
    | true -> (
        (* log_.log @@ "# l64"; *)
        (* may be creating a file *)
        ops.create path >>=| fun () ->
        (* log_.log @@ "# l68"; *)
        return (Ok None))
    | false -> 
      (* log_.log @@ "# l71"; *)
      (path |> ops.stat) >>=| function
      | _ -> return (Ok None)
      (* NOTE if no file, stat return error, which is propagated *)
  in


  (* really worth making sure that buffer types match, or abstracting over *)
  let read path buf ofs _ = 
    ofs |> Int64.to_int |> fun ofs -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    ops.open_ path >>=| fun fd ->  
    (* FIXME cache fds in LRU? *)
    (* FIXME fd leak if pread errors *)
    ops.pread ~fd ~foff:ofs ~len:buf_size ~buf:buf ~boff:0 >>=| fun n ->
    ops.close fd >>=| fun () ->
    return (Ok n)
  in


  let write path buf foff _ = 
    foff |> Int64.to_int |> fun foff -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    ops.open_ path >>=| fun fd ->
    (* FIXME fd leak *)
    ops.pwrite ~fd ~foff ~len:buf_size ~buf:buf ~boff:0 >>=| fun n ->
    ops.close fd >>=| fun () ->
    return (Ok n)
  in

  let rename src dst = ops.rename src dst in

  let truncate path length = 
    length |> Int64.to_int |> fun length ->  (* FIXME *)
    ops.truncate path length
  in


  (* stat_file and kind combined in following *)
  let getattr path0 = 
(*    log_.log_lazy (fun () -> 
        Printf.sprintf "# getattr (%s) mfuse.getattr.l110\n" path0);*)
    path0 |> fun path ->
    (* log_.log @@ "# mfuse.getattr.l112"; *)
    (* FIXME kind needs to be wrapped so it throws a unix_error *)
    path |> ops.stat >>=| fun st -> return (Ok(St_convs.stat2unix st))
  in


  let symlink src dst =
    ops.symlink src dst >>=| fun () ->
    return (Ok ())
  in

  let readlink p = 
    ops.readlink p >>=| fun s ->
    return (Ok s)
  in

  (* hack to avoid errors for apps that expect chmod *)
  let chmod _path _i = () in
  let utime _path _atim _mtim = () in


  let maybe_raise a = a |> co_eta |> function
    | Ok a -> a
    | Error e -> 
      Error_.mk_unix_exn e |> fun e ->
      raise e
  in
  let _ = maybe_raise in

  (* notify when exception is thrown *)
  let wrap f = 
    try f ()
    with e -> Log.log_lazy (fun () -> Printexc.to_string e); raise e
  in
  let wrap1 f = fun a -> wrap @@ fun () -> f a |> maybe_raise in
  let wrap2 f = fun a b -> wrap @@ fun () -> f a b |> maybe_raise in
  let wrap3 f = fun a b c -> wrap @@ fun () -> f a b c |> maybe_raise in
  let wrap4 f = fun a b c d -> wrap @@ fun () -> f a b c d |> maybe_raise in

  let unlink = wrap1 unlink in
  let rmdir = unlink in
  let mkdir = wrap2 mkdir in
  let readdir = wrap2 readdir in
  let fopen = wrap2 fopen in
  let read = wrap4 read in
  let write = wrap4 write in
  let rename = wrap2 rename in
  let truncate = wrap2 truncate in
  let getattr = wrap1 getattr in
  let symlink = wrap2 symlink in
  let readlink = wrap1 readlink in

  { default_operations with 
    init = (fun () -> Printf.printf "filesystem started\n%!");
    unlink=unlink;
    rmdir=unlink;
    mkdir;    
    readdir;
    fopen;
    mknod = (fun path _mode -> 
      let _ = fopen path [Unix.O_CREAT] in
      ()); (* NOTE gets called instead of fopen to create a file *)
    read;
    write;
    rename;
    truncate;
    getattr;
    chmod;
    utime;
    symlink;
    readlink;
  } [@@ocaml.warning "-26"]


let mk_fuse_ops ~monad_ops ~ops = 
  let readdir' ~ops = Readdir_util.readdir' ~ops in
  mk_fuse_ops ~monad_ops ~readdir':(readdir' ~monad_ops ~ops) ~ops

(**/**)

let mk_fuse_ops : 
monad_ops:'t monad_ops ->
ops:('f, 'd, 't) ops -> 
co_eta:'t co_eta -> 
operations 
= mk_fuse_ops
(** {[
monad_ops:'t monad_ops ->
ops:('f, 'd, 't) ops -> 
co_eta:'t co_eta -> 
operations 
]} *)
