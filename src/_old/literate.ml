(* generate the source code in a literate style *)

let dest_dir = try Sys.argv.(1) with _ -> "."


let mk fn = 
  let contents = ref "" in
  object (self)
    method fn=fn;
    method app=(fun s -> contents:=!contents^s);
    method append=(fun s -> contents:=!contents^"\n\n"^s^"\n\n");
    method write_to_file=(Tjr_file.write_string_to_file ~fn !contents)
    method contents=(!contents)
    method clear=contents:=""
    method copy_to=(fun x -> x#append (self#contents))
    method copy_to_all=(fun xs -> List.iter (fun x -> x#append (self#contents)) xs)
  end

let mem = mk @@ dest_dir^"/d_mem.ml"
let unix = mk @@ dest_dir^"/d_unix.ml"
let lwt = mk @@ dest_dir^"/d_lwt.ml"
let tmp = mk "/tmp/tmp.txt"  (* for temporary fiddling *)
let abstract = mk @@ dest_dir^"/d_abstract.ml"

let write fn s = Tjr_file.write_string_to_file ~fn s

(** 

We start with some basic types.

*)
;;

tmp#append {|

(* following for strings *)
let dirname_basename path = 
  ignore (Tjr_string.starts_with ~prefix:"/" path || failwith __LOC__);
  Tjr_string.split_on_last ~sub:"/" path |> fun (p,c) -> 
  (* the semantics is that dirname is an absolute path *)
  (if p="" then "/" else p),c


include Bigarray_buffer

(* minimal fs-like thing *)
type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }


type is_finished = bool
let finished = true

(* ensure 64 bit system *)
let _ = assert(Sys.int_size = 63)

type length = int (* FIXME in following *)
type offset = int

|}
;

(** There are some basic types which we could choose to make abstract. *)

ignore{|
module type FS_BASE_TYPES = sig
  type path 
  type dh 
  type fd 
end
|}
;

(** However, most of these have fairly fixed types, with the exception
    of fd, which is a Unix.file_descr or an Lwt_unix.file_descr (not the
    same!) *)

tmp#append {|
include struct
  open Bin_prot.Std
  type path=string [@@deriving bin_io, yojson]
  type dh=int
end

module Mem_base_types = struct
  type fd = int
  let fd2int x = x
end

module Unix_base_types = struct
  type fd=Unix.file_descr
  let fd2int x = ExtUnix.All.int_of_file_descr x
end

module Lwt_base_types = struct
  type fd=Lwt_unix.file_descr
  let fd2int x = x|>Lwt_unix.unix_file_descr|>ExtUnix.All.int_of_file_descr
end

module Abstract_base_types = struct
  type fd
  let fd2int x = failwith __LOC__
end
|}
;

write "c_base.ml" (tmp#contents);
tmp#clear;

let copy_tmp () = tmp#copy_to_all [mem;unix;lwt;abstract] in

tmp#append {|open C_base |};
copy_tmp();
tmp#clear;

mem#append {|module Base_types = Mem_base_types |};
unix#append {|module Base_types = Unix_base_types |};
lwt#append {|module Base_types = Lwt_base_types |};
abstract#append {|module Base_types = Abstract_base_types |};

(** One these are in place, we can consider the monad. These are
    different for Unix and Lwt. *)

tmp#append {|
open Base_types

module Mem_monad = C_in_mem.Monad

module Lwt_monad = struct
  type 'a m = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
end

module Unix_monad = struct
  type 'a m
  let return : 'a -> 'a m = fun x -> failwith "FIXME"
  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x -> failwith "FIXME"
end

module Abstract_monad = struct
  type 'a m
  let return : 'a -> 'a m = fun x -> failwith "FIXME"
  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x -> failwith "FIXME"
end  

|};

copy_tmp();
tmp#clear;

mem#append {|module Monad = Mem_monad |};
lwt#append {|module Monad = Lwt_monad|};
unix#append {|module Monad = Unix_monad|};
abstract#append {|module Monad = Abstract_monad |};


(** Finally we are in a position to define the operations. We keep
    imperative operations in a different structure to avoid names
    clashing. *)

tmp#append {|
open Monad

type ops = {
  root: path;
  unlink : parent:path -> name:string -> unit m;
  mkdir : parent:path -> name:string -> unit m;
  opendir : path -> dh m;
  (* . and .. are returned *)
  readdir : dh -> (string list * is_finished) m;
  closedir : dh -> unit m;
  create : parent:path -> name:string -> unit m;
  open_ : path -> fd m;
  pread: 
    fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m; 
  pwrite: 
    fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m;
  close : fd -> unit m;
  rename: 
    spath:path -> sname:string -> dpath:path -> dname:string -> unit m;
  truncate : path:path -> length:int -> unit m;
  stat_file : path -> file_stat m;
  kind : path -> st_kind m;
  reset : unit -> unit m;
}

(* convert to logged ops *)
type 'm log_op = {
  log: 'a. C_msgs.msg_from_client -> 'a m -> 'a m
}

let dh' = -99

let mk_logged_ops (type m) ~log_op ~ops = 
  let open C_msgs in
  let root = ops.root in
  let unlink ~parent ~name = 
    log_op.log (Unlink(parent,name)) (ops.unlink ~parent ~name) in
  let mkdir ~parent ~name = 
    log_op.log (Mkdir(parent,name)) (ops.mkdir ~parent ~name) in
  let opendir p = log_op.log (Opendir(p)) (ops.opendir p) in
  let readdir dh = log_op.log (Readdir(dh')) (ops.readdir dh) in 
  let closedir dh = log_op.log (Closedir(dh')) (ops.closedir dh) in
  let create ~parent ~name = 
    log_op.log (Create(parent,name)) (ops.create ~parent ~name) in
  let open_ p = log_op.log (Open(p)) (ops.open_ p) in
  let pread ~fd ~foff ~length ~buffer ~boff =
    log_op.log 
      (Pread(fd|>fd2int,foff,length)) 
      (ops.pread ~fd ~foff ~length ~buffer ~boff) 
  in
  let pwrite ~fd ~foff ~length ~buffer ~boff =
    log_op.log 
      (Pwrite(fd|>fd2int,foff,"data???FIXME")) 
      (ops.pwrite ~fd ~foff ~length ~buffer ~boff) 
  in
  let close fd = log_op.log (Close(fd|>fd2int)) (ops.close fd) in
  let rename ~spath ~sname ~dpath ~dname = 
    log_op.log 
      (Rename(spath,sname,dpath,dname)) 
      (ops.rename ~spath ~sname ~dpath ~dname) 
  in
  let truncate ~path ~length = 
    log_op.log (Truncate(path,length)) (ops.truncate ~path ~length) in
  (* FIXME log_op.log the rest as well *)
  let stat_file path = log_op.log (Stat_file(path)) (ops.stat_file path) in
  let kind path = log_op.log (Kind(path)) (ops.kind path) in
  let reset = ops.reset in
  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat_file; kind; reset }





module Imp = struct

  type run = {
    run:'a. 'a m -> 'a
  }

  type imp_ops = {
    root: path;
    unlink : parent:path -> name:string -> unit;
    mkdir : parent:path -> name:string -> unit;
    opendir : path -> dh;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished);
    closedir : dh -> unit;
    create : parent:path -> name:string -> unit;
    open_ : path -> fd;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int;
    close : fd -> unit;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> unit;
    truncate : path:path -> length:int -> unit;
    stat_file : path -> file_stat;
    kind : path -> st_kind;
    reset : unit -> unit;
  }


  let mk_imperative_ops ~(ops:ops) ~run =
    let run f = 
      try run.run f 
      with e -> (Printexc.to_string e |> print_endline; raise e) 
    in
    let root= ops.root in
    let unlink=(fun ~parent ~name -> run @@ ops.unlink ~parent ~name) in
    let mkdir=(fun ~parent ~name -> run @@ ops.mkdir ~parent ~name) in
    let opendir=(fun p -> run @@ ops.opendir p) in
    let readdir=(fun dh -> run @@ ops.readdir dh) in
    let closedir=(fun dh -> run @@ ops.closedir dh) in
    let create=(fun ~parent ~name -> run @@ ops.create ~parent ~name) in
    let open_=(fun path -> run @@ ops.open_ path) in
    let pread=(fun ~fd ~foff ~length ~buffer ~boff -> 
        run @@ ops.pread ~fd ~foff ~length ~buffer ~boff) in
    let pwrite=(fun ~fd ~foff ~length ~buffer ~boff -> 
        run @@ ops.pwrite ~fd ~foff ~length ~buffer ~boff) in
    let close=(fun fd -> run @@ ops.close fd) in
    let rename=(fun ~spath ~sname ~dpath ~dname -> 
        run @@ ops.rename ~spath ~sname ~dpath ~dname) in
    let truncate=(fun ~path ~length -> run @@ ops.truncate ~path ~length) in
    let stat_file=(fun path -> run @@ ops.stat_file path) in
    let kind=(fun path -> run @@ ops.kind path) in
    let reset=(fun () -> run @@ ops.reset ()) in
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat_file; kind; reset }


  (* for small directories *)
  let readdir' ~ops = 
    fun path ->
      let dh = ops.opendir path in
      let es = ref [] in
      let finished = ref false in
      while(not !finished) do
        let (es',f) = ops.readdir dh in
        es:=!es@es';
        finished:=f;
      done;
      ops.closedir dh;
      !es


end
|};


copy_tmp();
tmp#clear;



(* fuse ------------------------------------------------------------- *)

tmp#append {|

open Unix
open LargeFile
open Bigarray
open Fuse

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

let mk_fuse_ops ~run ~ops = 

  let ops = Imp.mk_imperative_ops ~run ~ops in
  let open Imp in

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


  let readdir' = readdir' ~ops in
  let readdir path _ = readdir' path in

  let _ = ops.kind in

  (* FIXME tricky combining create with fopen *)
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
    ops.pread ~fd ~foff:ofs ~length:buf_size ~buffer:buf ~boff:0 |> fun n ->
    ops.close fd;
    n
  in


  let write path buf foff _ : int = 
    path |> fun path ->
    foff |> Int64.to_int |> fun foff -> (* FIXME ofs should be int64 *)
    let buf_size = Bigarray.Array1.dim buf in
    ops.open_ path |> fun fd ->
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

  { default_operations with 
    init = (fun () -> Printf.printf "filesystem started\n%!");
    unlink;
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

|};

copy_tmp();
tmp#clear;

;


(* write to file ---------------------------------------------------- *)

List.iter (fun x -> x#write_to_file) [mem;unix;lwt;abstract]

;;
