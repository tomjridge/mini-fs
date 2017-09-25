(* fuse filesystem backed by minifs *)

open Unix
open LargeFile
open Bigarray
open Fuse

(* fuse ------------------------------------------------------------- *)

open Minifs


let default_stats = LargeFile.stat "."


let default_file_stats st_size = 
  { default_stats with 
    st_nlink = 1;
    st_kind=Unix.S_REG;
    st_perm = 0o640;
    st_size
  }

let default_dir_stats = default_stats



let mk_fuse_ops (type path) 
    ~(path_to_string:path->string) ~(string_to_path:string->path) 
    ~run ~ops 
  = 

  let ops = ops_to_imperative run ops in
  dest_imperative_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->


  let dirname_basename (path:path) : path * string = failwith __LOC__ in (* FIXME *)



  let unlink path = 
    path |> string_to_path |> dirname_basename |> fun (parent,name) -> 
    unlink ~parent ~name
  in

  let mkdir path _perms = 
    path |> string_to_path |> dirname_basename |> fun (parent,name) -> 
    mkdir ~parent ~name
  in

  (* opendir and closedir omitted *)
  (* create combined with fopen *)


  let readdir' = readdir' ~ops in
  let readdir path _ = 
    path |> string_to_path |> fun path ->
    readdir' path 
  in

  let _ = kind in

  (* FIXME tricky combining create with fopen *)
  let fopen (path:string) flags = 
    path |> string_to_path |> fun path ->
    Unix.(List.mem O_CREAT flags) |> function
    | true -> 
      (* may be creating a file *)
      dirname_basename path |> fun (parent,name) ->
      create ~parent ~name;
      None
    | false -> 
      path |> kind |> function
      | `File -> None
      | _ -> raise @@ Unix_error (ENOENT,"open",path|>path_to_string)
  in


  (* really worth making sure that buffer types match, or abstracting over *)
  let read path buf ofs _ : int = 
    path |> string_to_path |> fun path ->
    ofs |> Int64.to_int |> fun ofs -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    open_ path |> fun fd ->  (* FIXME cache fds in LRU? *)
    pread ~fd ~foff:ofs ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
    close fd;
    n
  in
    

  let write path buf foff _ : int = 
    path |> string_to_path |> fun path ->
    foff |> Int64.to_int |> fun foff -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    open_ path |> fun fd ->
    pwrite ~fd ~foff ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
    close fd;
    n
  in


  let truncate path length = 
    path |> string_to_path |> fun path ->
    length |> Int64.to_int |> fun length ->  (* FIXME *)
    truncate ~path ~length
  in

  

  (* stat_file and kind combined in following *)
  let getattr path0 = 
    path0 |> string_to_path |> fun path ->
    path |> kind |> function
    | `File -> 
      stat_file path |> fun x -> 
      x.sz |> Int64.of_int |> default_file_stats
    | `Dir -> default_dir_stats
    | _ -> raise @@ Unix_error (ENOENT,"getattr" ^ __LOC__,path0)
  in


  { default_operations with 
    unlink;
    mkdir;    
    readdir;
    fopen;
    read;
    write;
    truncate;
    getattr;
  } [@@ocaml.warning "-26"]


let _ = mk_fuse_ops

let unix_fuse_ops = 
  mk_fuse_ops ~path_to_string:(fun s -> s) ~ops:Mini_unix.unix_ops
