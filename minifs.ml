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



module Make = functor(M:MONAD) -> struct
  module M_ = M
  type 'a m = 'a M_.m

  let wf_ops (type path dh fd buffer t) 
      ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
      ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate 
      ~stat_file ~kind ~reset    
    = 
    let root : path = root in
    let unlink : parent:path -> name:string -> unit m = unlink in
    let mkdir : parent:path -> name:string -> unit m = mkdir in
    let opendir : path -> dh m = opendir in
    (* . and .. are returned *)
    let readdir : dh -> (string list * is_finished) m = readdir in
    let  closedir : dh -> unit m = closedir in
    let create : parent:path -> name:string -> unit m = create in
    let open_ : path -> fd m = open_ in
    let pread: fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m 
      = pread in
    let pwrite: fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m 
      = pwrite in
    let close : fd -> unit m = close in
    let rename: spath:path -> sname:string -> dpath:path -> dname:string -> unit m 
      = rename in
    let truncate : path:path -> length:int -> unit m = truncate in
    let stat_file : path -> file_stat m = stat_file in
    let kind : path -> st_kind m = kind in
    let reset : unit -> unit m = reset in
    true[@@ocaml.warning "-26"]

  let _ = wf_ops

  let mk_ops 
      ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset
    =
    assert(wf_ops 
             ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset);
    `Ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,rename,truncate,stat_file,kind,reset)

  let dest_ops (`Ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,rename,truncate,stat_file,kind,reset)) 
    =
    assert(wf_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset);
    fun k -> 
      k ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset

  let dest_ops' ops = dest_ops ops @@ 
    fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset 
    -> 
    (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)

  let opendir_readdir_closedir ops =
    dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->
    opendir,readdir,closedir



  (* imperative operations -------------------------------------------- *)

  type run = {
    run:'a. 'a m -> 'a
  }

  let wf_imperative_ops (type path dh fd buffer t)  
      ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset 
    =
    let root : path = root in
    let unlink : parent:path -> name:string -> unit = unlink in
    let mkdir : parent:path -> name:string -> unit = mkdir in
    let opendir : path -> dh = opendir in
    (* . and .. are returned *)
    let readdir : dh -> (string list * is_finished) = readdir in
    let closedir : dh -> unit = closedir in
    let create : parent:path -> name:string -> unit = create in
    let open_ : path -> fd = open_ in
    let pread : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int 
      = pread in
    let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int 
      = pwrite in
    let close : fd -> unit = close in
    let rename : spath:path -> sname:string -> dpath:path -> dname:string -> unit 
      = rename in
    let truncate : path:path -> length:int -> unit = truncate in
    let stat_file : path -> file_stat = stat_file in
    let kind : path -> st_kind = kind in
    let reset : unit -> unit = reset in
    true[@@ocaml.warning "-26"]


  let mk_imperative_ops ~run ~ops =
    dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->
    let run f = try run.run f with e -> (Printexc.to_string e |> print_endline; raise e) in
    let root= root in
    let unlink=(fun ~parent ~name -> run @@ unlink ~parent ~name) in
    let mkdir=(fun ~parent ~name -> run @@ mkdir ~parent ~name) in
    let opendir=(fun p -> run @@ opendir p) in
    let readdir=(fun dh -> run @@ readdir dh) in
    let closedir=(fun dh -> run @@ closedir dh) in
    let create=(fun ~parent ~name -> run @@ create ~parent ~name) in
    let open_=(fun path -> run @@ open_ path) in
    let pread=(fun ~fd ~foff ~length ~buffer ~boff -> 
        run @@ pread ~fd ~foff ~length ~buffer ~boff) in
    let pwrite=(fun ~fd ~foff ~length ~buffer ~boff -> 
        run @@ pwrite ~fd ~foff ~length ~buffer ~boff) in
    let close=(fun fd -> run @@ close fd) in
    let rename=(fun ~spath ~sname ~dpath ~dname -> 
        run @@ rename ~spath ~sname ~dpath ~dname) in
    let truncate=(fun ~path ~length -> run @@ truncate ~path ~length) in
    let stat_file=(fun path -> run @@ stat_file path) in
    let kind=(fun path -> run @@ kind path) in
    let reset=(fun () -> run @@ reset ()) in
    assert(wf_imperative_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset);
    `Imperative_ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,rename,truncate,stat_file,kind,reset)  


  let dest_imperative_ops (`Imperative_ops(root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,rename,truncate,stat_file,kind,reset)) 
    =
    assert(wf_imperative_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset);
    fun k -> 
      k ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset




  (* extra ops -------------------------------------------------------- *)

  (* this is to make top-level interaction a bit smoother *)



  (* for small directories *)
  let readdir' ~ops = 
    dest_imperative_ops ops @@ 
    fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~rename ~truncate ~stat_file ~kind ~reset ->
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
    ignore (Tjr_string.starts_with ~prefix:"/" path || failwith __LOC__);
    Tjr_string.split_on_last ~sub:"/" path |> fun (p,c) -> 
    (* the semantics is that dirname is an absolute path *)
    (if p="" then "/" else p),c


end
