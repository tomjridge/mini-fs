(* bind an imperative ops to fuse ----------------------------------- *)
open C_base

module Make_fuse(I:D_functors.IMP_OPS_TYPE) = struct

  open Unix
  open LargeFile
  open Bigarray
  open Fuse
  open I
  (* FIXME wrap operations so they return unix_error *)

  let default_file_stats = 
    (* ASSUMES this file is present *)
    LargeFile.stat "tmp.txt"  

  let default_file_stats st_size = 
    { default_file_stats with 
      st_nlink = 1;
      st_kind=Unix.S_REG;
      st_perm = 0o640;
      st_size
    }

  let default_dir_stats = LargeFile.stat "."

  module Readdir' = D_functors.Make_readdir'(I)
      
  let mk_fuse_ops ops = 

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


    let readdir' = Readdir'.readdir' ~ops in
    let readdir path _ = readdir' path in

    let _ = ops.kind in

    (* FIXME tricky combining create with fopen *)
    (* NOTE that fuse keeps track of fds, so this just returns None;
       read and write are via path *)
    let fopen (path:string) flags = 
      print_endline @@ "# fopen "^ path ^ " mfuse.fopen.l61";
      Unix.(List.mem O_CREAT flags) |> function
      | true -> 
        print_endline @@ "# l64";
        (* may be creating a file *)
        dirname_basename path |> fun (parent,name) ->
        ops.create ~parent ~name;
        print_endline @@ "# l68";
        None
      | false -> 
        print_endline @@ "# l71";
        path |> ops.kind |> function
        | `File -> None
        | _ -> raise @@ Unix_error (ENOENT,"open",path)
    in


    (* really worth making sure that buffer types match, or abstracting over *)
    let read path buf ofs _ : int = 
      path |> fun path ->
      ofs |> Int64.to_int |> fun ofs -> (* FIXME ofs should be int64 *)
      let buf_size = Bigarray.Array1.dim buf in
      ops.open_ path |> fun fd ->  (* FIXME cache fds in LRU? *)
      (* FIXME fd leak if pread errors *)
      ops.pread ~fd ~foff:ofs ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
      ops.close fd;
      n
    in


    let write path buf foff _ : int = 
      path |> fun path ->
      foff |> Int64.to_int |> fun foff -> (* FIXME ofs should be int64 *)
      let buf_size = Bigarray.Array1.dim buf in
      ops.open_ path |> fun fd ->
      (* FIXME fd leak *)
      ops.pwrite ~fd ~foff ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
      ops.close fd;
      n
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
      Printf.printf "# getattr (%s) mfuse.getattr.l110\n" path0;
      path0 |> fun path ->
      print_endline @@ "# mfuse.getattr.l112";
      (* FIXME kind needs to be wrapped so it throws a unix_error *)
      path |> ops.kind |> function
      | `File -> (
          print_endline @@ "# mfuse.getattr.l116";
          ops.stat_file path |> fun x -> 
          x.sz |> Int64.of_int |> default_file_stats)
      | `Dir -> (
          print_endline @@ "# mfuse.getattr.l120";
          default_dir_stats)
      | _ -> (
          print_endline @@ "# getattr exception(ENOENT) mfuse.getattr.l123";
          raise @@ Unix_error (ENOENT,"getattr l124",path0))
    in


    (* hack to avoid errors for apps that expect chmod *)
    let chmod path i = () in
    let utime path atim mtim = () in


    (* notify when exception is thrown *)
    let wrap f = 
      try f ()
      with e -> print_endline (Printexc.to_string e); raise e
    in
    let wrap1 f = fun a -> wrap @@ fun () -> f a in
    let wrap2 f = fun a b -> wrap @@ fun () -> f a b in
    let wrap3 f = fun a b c -> wrap @@ fun () -> f a b c in
    let wrap4 f = fun a b c d -> wrap @@ fun () -> f a b c d in

    let unlink = wrap1 unlink in
    let rmdir = wrap1 rmdir in
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


  let _ = mk_fuse_ops

end
