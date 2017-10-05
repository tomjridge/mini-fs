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

  let ops = ops_to_imperative run ops in
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
    print_endline @@ "fopen "^ path ^ " " ^__LOC__;
    Unix.(List.mem O_CREAT flags) |> function
    | true -> 
      print_endline __LOC__;
      (* may be creating a file *)
      dirname_basename path |> fun (parent,name) ->
      create ~parent ~name;
      print_endline __LOC__;
      None
    | false -> 
      print_endline __LOC__;
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
    print_endline @@ path0 ^ __LOC__;
    path0 |> fun path ->
    print_endline __LOC__;
    (* FIXME kind needs to be wrapped so it throws a unix_error *)
    path |> kind |> function
    | `File -> (
      print_endline __LOC__;
      stat_file path |> fun x -> 
      x.sz |> Int64.of_int |> default_file_stats)
    | `Dir -> (
      print_endline __LOC__;
      default_dir_stats)
    | _ -> (
      print_endline __LOC__;
      raise @@ Unix_error (ENOENT,"getattr" ^ __LOC__,path0))
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

  let err e = fun (g: 'a -> ww) ->
    fun w ->
      (e |> exn__to_yojson |> Yojson.Safe.pretty_to_string |> print_endline);
      print_endline __LOC__;
      match e with
      | `Error_no_entry _ -> raise @@ Unix_error(ENOENT, __LOC__,"")
      | `Error_not_directory -> raise @@ Unix_error(ENOTDIR, __LOC__,"")
      | `Error_not_file -> raise @@ Unix_error(ENOENT, __LOC__,"") 
  (* FIXME Error_not_file could be a dir, in which case raise EISDIR *)

  let extra = { err; new_did; new_fid }

  let ops = mk_ops ~extra

  let ops = Mini_log.mk_logged_ops ~ops
end

    


let in_mem_fuse_ops = 
    mk_fuse_ops
      ~run:Mini_in_mem.run 
      ~ops:Logged_in_mem_with_unix_errors.ops

(* TODO 
let unix_fuse_ops = 
  mk_fuse_ops 
    ~path_to_string:(fun s -> s) 
    ~string_to_path:(fun s -> s)
    ~dirname_basename
    ~run:Mini_unix.run
    ~ops:Mini_unix.unix_ops

let _ : Fuse.operations = unix_fuse_ops
*)
