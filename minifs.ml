(* minimal fs-like thing *)
type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }


type is_finished = bool
let finished = true

(* ensure 64 bit system *)
let _ = assert(Sys.int_size = 63)

type length = int (* FIXME in following *)
type offset = int



module type MONAD = sig
  type 'a m
  val return : 'a -> 'a m
  val bind: 'a m -> ('a -> 'b m) -> 'b m
end

module type FS_BASE_TYPES = sig
  type path 
  type dh 
  type fd 
  type buffer
end


(* type buffer = bytes  (* or cstruct? *) *)
module B = struct
type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
end
include B


module Standard_base_types = struct
  type path=string 
  type dh=int 
  type fd=int 
  type buffer=B.buffer
end


module type FS_BASE_TYPES' = FS_BASE_TYPES with type path=string and type dh=int 

module Make = functor(M:MONAD)(F:FS_BASE_TYPES) -> struct
  module F_ = F
  open F_
  module M_ = M
  type 'a m = 'a M_.m

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

(*    
  let mk_ops 
      ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset
    =
    { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
      pread; pwrite; close; rename; truncate; stat_file; kind; reset }

  let dest_ops ops
    =
    fun f -> 
      let (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,rename,truncate,stat_file,kind,reset)
        = (ops.root,ops.unlink,ops.mkdir,ops.opendir,ops.readdir,ops.closedir,ops.create,ops.open_,ops.pread,ops.pwrite,ops.close,ops.rename,ops.truncate,ops.stat_file,ops.kind,ops.reset)
      in
      f ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset

  let _ = dest_ops

(*
  let dest_ops' ops = dest_ops ops @@ 
    fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset 
    -> 
    (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)
*)
(*
  let opendir_readdir_closedir ops =
    dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->
    opendir,readdir,closedir
*)

*)

end

module Make_imp = functor(M:MONAD)(F:FS_BASE_TYPES) -> struct
  include Make(M)(F)
  open F


  (* imperative operations -------------------------------------------- *)

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


  end


  (* extra stuff ----------------------------------------------------- *)

  (* this is to make top-level interaction a bit smoother *)


  (* for small directories *)
  let readdir' ~ops = 
    let open Imp in
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



  (* following for strings *)
  let dirname_basename path = 
    ignore (Tjr_string.starts_with ~prefix:"/" path || failwith __LOC__);
    Tjr_string.split_on_last ~sub:"/" path |> fun (p,c) -> 
    (* the semantics is that dirname is an absolute path *)
    (if p="" then "/" else p),c


end


  
