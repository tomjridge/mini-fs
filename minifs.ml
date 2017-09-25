(* minimal fs-like thing *)
open Monad

type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }


type is_finished = bool
let finished = true

(* ensure 64 bit system *)
let _ = assert(Sys.int_size = 63)

type length = int (* FIXME in following *)
type offset = int


let wf_ops (type path dh fd buffer t) 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
    ~create ~open_ ~pread ~pwrite ~close ~truncate 
    ~stat_file ~kind ~reset    
  = 
  let root : path = root in
  let unlink : parent:path -> name:string -> (unit,t) m = unlink in
  let mkdir : parent:path -> name:string -> (unit,t) m = mkdir in
  let opendir : path -> (dh,t) m = opendir in
  (* . and .. are returned *)
  let readdir : dh -> ((string list * is_finished),t) m = readdir in
  let  closedir : dh -> (unit,t) m = closedir in
  let create : parent:path -> name:string -> (unit,t) m = create in
  let open_ : path -> (fd,t) m = open_ in
  let pread : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> (int,t) m = pread in
  let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> (int,t) m = pwrite in
  let close : fd -> (unit,t) m = close in
  let truncate : path:path -> length:int -> (unit,t) m = truncate in
  let stat_file : path -> (file_stat,t) m = stat_file in
  let kind : path -> (st_kind,t) m = kind in
  let reset : unit -> (unit,t) m = reset in
  true[@@ocaml.warning "-26"]

let mk_ops 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset
  =
  assert(wf_ops 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset);
  `Ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)

let dest_ops (`Ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)) 
  =
  assert(wf_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset);
  fun k -> 
    k ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset

let dest_ops' ops = dest_ops ops @@ 
  fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset 
  -> 
  (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)

let opendir_readdir_closedir ops =
  dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
  opendir,readdir,closedir



(* imperative operations -------------------------------------------- *)


type 't run = {
    run:'a. ('a,'t) m -> 'a
  }

let wf_imperative_ops (type path dh fd buffer t)  
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset 
  =
  let root : path = root in
  let unlink : parent:path -> name:string -> unit = unlink in
  let mkdir : parent:path -> name:string -> unit = mkdir in
  let opendir : path -> dh = opendir in
  (* . and .. are returned *)
  let readdir : dh -> (string list * is_finished) = readdir in
  let  closedir : dh -> unit = closedir in
  let create : parent:path -> name:string -> unit = create in
  let open_ : path -> fd = open_ in
  let pread : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int = pread in
  let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int = pwrite in
  let close : fd -> unit = close in
  let truncate : path:path -> length:int -> unit = truncate in
  let stat_file : path -> file_stat = stat_file in
  let kind : path -> st_kind = kind in
  let reset : unit -> unit = reset in
  true[@@ocaml.warning "-26"]


let ops_to_imperative run ops =
  dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
  let root= root in
  let unlink=(fun ~parent ~name -> run.run @@ unlink ~parent ~name) in
  let mkdir=(fun ~parent ~name -> run.run @@ mkdir ~parent ~name) in
  let opendir=(fun p -> run.run @@ opendir p) in
  let readdir=(fun dh -> run.run @@ readdir dh) in
  let closedir=(fun dh -> run.run @@ closedir dh) in
  let create=(fun ~parent ~name -> run.run @@ create ~parent ~name) in
  let open_=(fun path -> run.run @@ open_ path) in
  let pread=(fun ~fd ~foff ~length ~buffer ~boff -> run.run @@ pread ~fd ~foff ~length ~buffer ~boff) in
  let pwrite=(fun ~fd ~foff ~length ~buffer ~boff -> run.run @@ pwrite ~fd ~foff ~length ~buffer ~boff) in
  let close=(fun fd -> run.run @@ close fd) in
  let truncate=(fun ~path ~length -> run.run @@ truncate ~path ~length) in
  let stat_file=(fun path -> run.run @@ stat_file path) in
  let kind=(fun path -> run.run @@ kind path) in
  let reset=(fun () -> run.run @@ reset ()) in
  assert(wf_imperative_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset);
  `Imperative_ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)  


let dest_imperative_ops (`Imperative_ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)) 
  =
  assert(wf_imperative_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset);
  fun k -> 
    k ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset





(* extra ops -------------------------------------------------------- *)

(* this is to make top-level interaction a bit smoother *)



(* for small directories *)
let readdir' ~ops = 
  dest_imperative_ops ops @@ 
  fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
  fun path ->
    let dh = opendir path in
    let es = ref [] in
    let finished = ref false in
    while(not !finished) do
      let (es',f) = readdir dh in
      es:=!es@es';
      finished:=f;
    done;
    closedir dh;
    !es



(* following for strings *)
let dirname_basename path = 
  assert(Tjr_string.contains ~sub:"/" path);
  Tjr_string.split_on_last ~sub:"/" path



(* old -------------------------------------------------------------- *)



(*



    let root= ops.root in
    let unlink=(fun ~parent ~name -> run.run @@ ops.unlink ~parent ~name) in
    let mkdir=(fun ~parent ~name -> run.run @@ ops.mkdir ~parent ~name) in
    let opendir=(fun p -> run.run @@ ops.opendir p) in
    let readdir=(fun dh -> run.run @@ ops.readdir dh) in
    let closedir=(fun dh -> run.run @@ ops.closedir dh) in
    let create=(fun ~parent ~name -> run.run @@ ops.create ~parent ~name) in
    let open_=(fun path -> run.run @@ ops.open_ path) in
    let pread=(fun ~fd ~foff ~length ~buffer ~boff -> run.run @@ ops.pread ~fd ~foff ~length ~buffer ~boff) in
    let pwrite=(fun ~fd ~foff ~length ~buffer ~boff -> run.run @@ ops.pwrite ~fd ~foff ~length ~buffer ~boff) in
    let close=(fun fd -> run.run @@ ops.close fd) in
    let truncate=(fun ~path ~length -> run.run @@ ops.truncate ~path ~length) in
    let stat_file=(fun path -> run.run @@ ops.stat_file path) in
    let kind=(fun path -> run.run @@ ops.kind path) in
    let reset=(fun () -> run.run @@ ops.reset ()) in
    k ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
      ~create ~open_ ~pread ~pwrite ~close ~truncate 
      ~stat_file ~kind ~reset


module type S = sig

  type path

  type dh (* dir_handle, for reading dirs *)

  type fd

  type buffer

end
*)


(*
module Make = functor (S:S) -> struct
  module S = S
  open S

  (* FIXME surely unlink just takes a path? *)
  type unlink = parent:path -> name:string -> unit m
  type mkdir = parent:path -> name:string -> unit m
  type opendir = path -> dh m

  (* . and .. are returned *)
  type readdir = dh -> (string list * is_finished) m
  type closedir = dh -> unit m 
  type create = parent:path -> name:string -> unit m
  type open_ = path -> fd m
  type pread = fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m 
  type pwrite = fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m 
  type close = fd -> unit m
  type truncate = path:path -> length:int -> unit m
  type stat_file = path -> file_stat m
  type kind = path -> st_kind m
  type reset = unit -> unit m

  
  type ops = {
    root:path;
    unlink:unlink;
    mkdir:mkdir;
    opendir:opendir;
    readdir:readdir;
    closedir:closedir;
    create:create;
    open_:open_;
    pread:pread;
    pwrite:pwrite;
    close:close;
    truncate:truncate;
    stat_file:stat_file;
    kind:kind;
    reset:reset;
  }


(*    ignore {
      unlink;
      mkdir;
      opendir;
      readdir;
      closedir;
      create;
      open_;
      pread;
      pwrite;
      close;
      truncate;
      stat_file;
      kind;
      reset;
    };
*)
end
*)

