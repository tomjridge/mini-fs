(* minimal fs-like thing *)
type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }


type is_finished = bool
let finished = true

(* ensure 64 bit system *)
let _ = assert(Sys.int_size = 63)

type length = int (* FIXME in following *)
type offset = int

type ('a,'m) m_ = ('a -> 'm) -> 'm


let wf_ops (type path dh fd buffer t m) 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
    ~create ~open_ ~pread ~pwrite ~close ~truncate 
    ~stat_file ~kind ~reset    
  = 
  let root : path = root in
  let unlink : parent:path -> name:string -> (unit,m) m_ = unlink in
  let mkdir : parent:path -> name:string -> (unit,m) m_ = mkdir in
  let opendir : path -> (dh,m) m_ = opendir in
  (* . and .. are returned *)
  let readdir : dh -> ((string list * is_finished),m) m_ = readdir in
  let  closedir : dh -> (unit -> m) -> m = closedir in
  let create : parent:path -> name:string -> (unit,m) m_ = create in
  let open_ : path -> (fd,m) m_ = open_ in
  let pread : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> (int,m) m_ = pread in
  let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> (int,m) m_ = pwrite in
  let close : fd -> (unit,m) m_ = close in
  let truncate : path:path -> length:int -> (unit,m) m_ = truncate in
  let stat_file : path -> (file_stat,m) m_ = stat_file in
  let kind : path -> (st_kind,m) m_ = kind in
  let reset : unit -> (unit,m) m_ = reset in
  true[@@ocaml.warning "-26"]

let _ = wf_ops

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


type 'm run = {
    run:'a. (('a -> 'm) -> 'm) -> 'a
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



(* state passing, no error ------------------------------------------ *)

module Mk_state_passing = functor(W: sig type w end) -> struct
  (* assume world state is 'w, so 'm = 'w -> 'w *)

  module W = W
  open W

  type ww = w -> w

  type 'a m = ('a,ww) m_

  (* NOTE following a bit hairy *)
  let ( >>= ) (x:'a m) (f:'a -> 'b m) : 'b m = 
    fun (g:'b -> w -> w) -> 
    fun (w:w) ->
      let f' : 'a -> ww = fun a -> f a g in
      let x' : ww = x f' in
      let w' : w = x' w in
      w'

  let return (x: 'a) : 'a m = 
    fun (g: 'a -> ww) -> g x


  (* change state before running remainder of computation *)
  let with_state (f:w->w) (x:'a m) : 'a m = 
    fun (g:'a -> w -> w) -> 
    fun (w:w) -> 
      let w' : w = f w in
      x g w'

  (* change state, and derive a parameter required for rest of computation *)
  let with_state' (f:w -> 'b*w) (g:'b -> 'a m) : 'a m = 
    fun (h:'a -> w -> w) ->
    fun (w:w) ->
      let (b,w') = f w in
      (g b) h w'

  let mk_imperative_ops ops w0 = 

    let ref_ = ref w0 in

    (* some bug with ppx not working with local exceptions, so use
       first class modules instead *)

    let run_imperative (type a) (f:a m) : a = 
      let f : (a -> ww) -> ww = f in
      let module M = struct exception E of a end in
      (* a -> ww is the type of the "rest of the computation" *)
      let a_ww : a -> ww = fun a w -> ref_:=w; raise (M.E a) in
      try ignore(f a_ww (!ref_)); failwith __LOC__
      with M.E a -> a
    in

    let run = { run=run_imperative } in

    ops_to_imperative run ops

end



(* steppable -------------------------------------------------------- *)


(* FIXME would like another version which is iso to w->w, but with an
   explicit step operation

(* explicit state passing, steppable, final result via state;
   errors via exception *)

(* NOTE we aim for ('a,'m) m_ = ('a,steppable) m_ = ('a -> steppable) -> steppable)*)


(* sort of co-inductive object *)
type steppable = {step:fs_t -> fs_t * steppable }


(* following could be parameters *)
let with_fs : (fs_t -> fs_t) -> (unit -> 'm) -> 'm = 
  fun f k -> { step=(fun s -> f s, k ()) }


let _ = with_fs


let with_fs': (fs_t -> 'a * fs_t) -> ('a -> 'm) -> 'm = 
  fun f k -> 
    { step=(fun s -> 
          f s |> fun (a,s) ->
          s,k a) }


let _ = with_fs'

(* 'a comp is ('a -> 'm ) -> 'm  where 'm represents the computation *) 

let ( >>= ) x f = fun k -> (x (fun rx -> f rx k))

let bind f x = fun k -> (x (fun rx -> f rx k))

let _ = bind

let return x k = k x


type ('e,'m) extra_ops = {
  err: ' a. 'e -> ('a,'m)m_;
  new_did: unit -> (did,'m)m_;
  new_fid: unit -> (fid,'m)m_;
}


let extra = {
  err=(fun e k -> {step=(fun s -> raise (E e)) });
  new_did=(fun () k -> {step=(
      fun s -> 
        let s' = { s with max_did=(inc_did s.max_did) }in
        let did = s'.max_did in
        (s',k did))});
  new_fid=(fun () k -> {step=(
      fun s -> 
        let s' = { s with max_fid=(inc_fid s.max_fid) } in
        let fid = s'.max_fid in
        (s',k fid))});
}



*)


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


