(* fuse filesystem backed by minifs *)

open Unix
open LargeFile
open Bigarray
open Fuse

(* fuse ------------------------------------------------------------- *)

open Minifs

(* FIXME wrap operations so they return unix_error *)

let default_file_stats = LargeFile.stat "tmp.txt"  (* ASSUMES this file is present *)

let default_file_stats st_size = 
  { default_file_stats with 
    st_nlink = 1;
    st_kind=Unix.S_REG;
    st_perm = 0o640;
    st_size
  }

let default_dir_stats = LargeFile.stat "."




let mk_fuse_ops (type path) 
    ~run ~ops 
  = 

  let ops = mk_imperative_ops run ops in
  dest_imperative_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->


  let unlink path = 
    path |> dirname_basename |> fun (parent,name) -> 
    unlink ~parent ~name
  in

  let mkdir path _perms = 
    path |> dirname_basename |> fun (parent,name) -> 
    mkdir ~parent ~name
  in

  (* opendir and closedir omitted *)
  (* create combined with fopen *)


  let readdir' = readdir' ~ops in
  let readdir path _ = 
    path |> fun path ->
    readdir' path 
  in

  let _ = kind in

  (* FIXME tricky combining create with fopen *)
  let fopen (path:string) flags = 
    print_endline @@ "# fopen "^ path ^ " mfuse.fopen.l61";
    Unix.(List.mem O_CREAT flags) |> function
    | true -> 
      print_endline @@ "# l64";
      (* may be creating a file *)
      dirname_basename path |> fun (parent,name) ->
      create ~parent ~name;
      print_endline @@ "# l68";
      None
    | false -> 
      print_endline @@ "# l71";
      path |> kind |> function
      | `File -> None
      | _ -> raise @@ Unix_error (ENOENT,"open",path)
  in


  (* really worth making sure that buffer types match, or abstracting over *)
  let read path buf ofs _ : int = 
    path |> fun path ->
    ofs |> Int64.to_int |> fun ofs -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    open_ path |> fun fd ->  (* FIXME cache fds in LRU? *)
    pread ~fd ~foff:ofs ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
    close fd;
    n
  in


  let write path buf foff _ : int = 
    path |> fun path ->
    foff |> Int64.to_int |> fun foff -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    open_ path |> fun fd ->
    pwrite ~fd ~foff ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
    close fd;
    n
  in


  let truncate path length = 
    path |> fun path ->
    length |> Int64.to_int |> fun length ->  (* FIXME *)
    truncate ~path ~length
  in


  (* stat_file and kind combined in following *)
  let getattr path0 = 
    Printf.printf "# getattr (%s) mfuse.getattr.l110\n" path0;
    path0 |> fun path ->
    print_endline @@ "# mfuse.getattr.l112";
    (* FIXME kind needs to be wrapped so it throws a unix_error *)
    path |> kind |> function
    | `File -> (
      print_endline @@ "# mfuse.getattr.l116";
      stat_file path |> fun x -> 
      x.sz |> Int64.of_int |> default_file_stats)
    | `Dir -> (
      print_endline @@ "# mfuse.getattr.l120";
      default_dir_stats)
    | _ -> (
      print_endline @@ "# getattr exception(ENOENT) mfuse.getattr.l123";
      raise @@ Unix_error (ENOENT,"getattr l124",path0))
  in


  { default_operations with 
	  init = (fun () -> Printf.printf "filesystem started\n%!");
   unlink;
   mkdir;    
   readdir;
   fopen;
	 mknod = (fun path mode -> ignore(fopen path [Unix.O_CREAT]); ()); (* FIXME gets called instead of fopen to create a file *)
   read;
   write;
   truncate;
   getattr;
  } [@@ocaml.warning "-26"]


let _ = mk_fuse_ops


module Logged_in_mem_with_unix_errors = struct

  open Mini_in_mem

  let log_op : 'm Mini_log.log_op = Mini_log.{
      log=Mini_in_mem.log
    }

  let ops = Mini_log.mk_logged_ops ~log_op ~ops

  let _ = ops

  (* need to convert errors to exceptions, and use imperative state *)

  type ww = X_.ww

  let ref_ = ref init_fs

  (* target type: 'm run = 'a. (('a -> 'm) -> 'm) -> 'a, where 'm is ww *)

  let mk_exn = function
    | `Error_no_entry _ -> Unix_error(ENOENT, "154","")
    | `Error_not_directory -> Unix_error(ENOTDIR, "155","")
    | `Error_not_file -> Unix_error(ENOENT, "156","") 


  let run (type a) (aa:((a -> ww) -> ww)) : a = 
    let module M = struct exception E of a end in
    (* a -> ww is the type of the "rest of the computation" *)
    let a_ww : a -> ww = 
      fun a w -> 
        let (e,w') = w in
        ref_:=w'; 
        print_endline "# mfuse.l178";
        match e with
        | None -> (print_endline "# mfuse.l180"; raise (M.E a))
        | Some e' -> failwith "impossible mfuse.l178"
    in
    try 
      match aa a_ww (None,!ref_) with
      | (None,w') -> failwith "impossible mfuse.run.l188"
      | (Some e,w') -> ref_:=w'; raise (mk_exn e)
    with (M.E a) -> a

  let run : ww run = { run }

  let imp_ops = Minifs.mk_imperative_ops ~run ~ops

end

    

let in_mem_fuse_ops = Logged_in_mem_with_unix_errors.(mk_fuse_ops ~run ~ops)
