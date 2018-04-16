(* bind ops to fuse ----------------------------------- *)

open Base_
open Ops_types


module Make_fuse(I:Ops_types.OPS_TYPE_WITH_RESULT) =  struct

  open Unix
  open LargeFile
  open Bigarray
  open Fuse
  open I
  (* FIXME wrap operations so they return unix_error *)

  (* module Readdir' = Readdir'.Make_readdir'(I)  *)

  let ( >>=) = I.bind

  (* propagate errors *)
  let ( >>=| ) a b = a >>= function Error e -> return (Error e) | Ok a -> b a

  type co_eta = {
    co_eta: 'a. 'a m -> 'a
  }

  let mk_fuse_ops ~readdir' ~ops ~co_eta = 
    let co_eta = co_eta.co_eta in

    let unlink path = 
      path |> dirname_basename |> fun (parent,name) -> 
      ops.unlink ~parent ~name 
    in

    let mkdir path _perms = 
      path |> dirname_basename |> fun (parent,name) -> 
      ops.mkdir ~parent ~name
    in

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
          dirname_basename path |> fun (parent,name) ->
          ops.create ~parent ~name >>=| fun () ->
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
      ops.pread ~fd ~foff:ofs ~length:buf_size ~buffer:buf ~boff:0 >>=| fun n ->
      ops.close fd >>=| fun () ->
      return (Ok n)
    in


    let write path buf foff _ = 
      foff |> Int64.to_int |> fun foff -> (* FIXME ofs should be int64 *)
      let buf_size = Bigarray.Array1.dim buf in
      ops.open_ path >>=| fun fd ->
      (* FIXME fd leak *)
      ops.pwrite ~fd ~foff ~length:buf_size ~buffer:buf ~boff:0 >>=| fun n ->
      ops.close fd >>=| fun () ->
      return (Ok n)
    in

    let rename src dst = 
      src |> dirname_basename |> fun (spath,sname) ->
      dst |> dirname_basename |> fun (dpath,dname) ->
      ops.rename ~spath ~sname ~dpath ~dname
    in

    let truncate path length = 
      path |> fun path ->
      length |> Int64.to_int |> fun length ->  (* FIXME *)
      ops.truncate ~path ~length
    in


    (* stat_file and kind combined in following *)
    let getattr path0 = 
      log_.log_lazy (fun () -> 
          Printf.sprintf "# getattr (%s) mfuse.getattr.l110\n" path0);
      path0 |> fun path ->
      (* log_.log @@ "# mfuse.getattr.l112"; *)
      (* FIXME kind needs to be wrapped so it throws a unix_error *)
      path |> ops.stat >>=| fun st -> return (Ok(stat2unix st))
    in


    (* hack to avoid errors for apps that expect chmod *)
    let chmod path i = () in
    let utime path atim mtim = () in


    let maybe_raise a = a |> co_eta |> function
      | Ok a -> a
      | Error e -> 
        mk_unix_exn e |> fun e ->
        raise e
    in
    let _ = maybe_raise in

    (* notify when exception is thrown *)
    let wrap f = 
      try f ()
      with e -> log_.log_lazy (fun () -> Printexc.to_string e); raise e
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


    { default_operations with 
      init = (fun () -> Printf.printf "filesystem started\n%!");
      unlink=unlink;
      rmdir=unlink;
      mkdir;    
      readdir;
      fopen;
      mknod = (fun path mode -> ignore(fopen path [Unix.O_CREAT]); ()); (* FIXME gets called instead of fopen to create a file *)
      read;
      write;
      rename;
      truncate;
      getattr;
      chmod;
      utime;
    } [@@ocaml.warning "-26"]

  let readdir' = 
    let module M = Readdir'.Make_readdir'(I) in 
    M.readdir' 

  let mk_fuse_ops ~ops = mk_fuse_ops ~readdir':(readdir' ~ops) ~ops

  let _ = mk_fuse_ops

end
