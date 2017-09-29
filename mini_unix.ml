open Minifs

(* to make integration with fuse easier; extunix supports this in module BA *)
type fuse_buffer = Fuse.buffer

type buffer = fuse_buffer


(* unix impl -------------------------------------------------------- *)

type path = string

(*type t = state*)

type dh = Unix.dir_handle

type fd = Unix.file_descr

(* type buffer = bytes  (* or cstruct? *) *)

(* type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t *)

exception No_such_entry

(* ops -------------------------------------------------------------- *)

type w

type 'a m = ('a -> w -> w) -> w -> w

(* following a bit horrible *)
let bind (am : 'a m) (f:'a -> 'b m) : 'b m = 
  fun bww w ->
    (* apply am *)
    let aww = fun a w -> (f a) bww w in
    am aww w

let ( >>= ) = bind 

type extra_ops = {
  safely: 'a. (unit -> 'a m) -> 'a m;  (* this delays until receives a world *)
(*  with_state: 'a. (state -> 'a * state) -> ('a -> 'm) -> 'm; *)
  return: 'a. 'a -> 'a m;
}


let mk_ops ~extra () = 
  let root : path = "/" in


  let unlink ~parent ~name : unit m = 
    extra.safely @@ fun () ->
    Unix.unlink @@ parent^"/"^name ;
    extra.return ()
  in


  let default_perm = 0o640 in


  let mkdir ~parent ~name = 
    extra.safely @@ fun () -> 
    Unix.mkdir (parent^"/"^name) default_perm;
    extra.return ()
  in



  let mk_dh ~path = Unix.opendir path in

  let opendir path = 
    extra.safely @@ fun () -> extra.return (mk_dh path)
  in


  let readdir dh = 
    extra.safely @@ fun () -> (* safely is just to delay till world available *)
    try Unix.readdir dh |> fun e -> extra.return ([e],not finished)
    with _ -> extra.return ([],finished)
  in


  let closedir dh = 
    extra.safely @@ fun () -> Unix.closedir dh; extra.return ()
  in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name = 
    extra.safely @@ fun () -> 
    let open Unix in
    openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
    close fd;
    extra.return ()
  in


  let mk_fd path = 
    extra.safely @@ fun () ->
    Unix.(openfile path [O_RDWR] default_perm) |> extra.return
  in

  let open_ path = mk_fd path in


  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    extra.safely @@ fun () ->
    (* bigarray pread has no boff, and length is taken from array, so
       resort to slicing *)
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pread fd foff buffer |> fun nread ->
    extra.return nread
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    extra.safely @@ fun () ->
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pwrite fd foff buffer |> fun n ->
    extra.return n
  in


  let close fd = extra.safely @@ fun () ->
    (Unix.close fd; extra.return ()) 
  in 
  (* FIXME record which are open? *)


  let truncate ~path ~length = 
    extra.safely @@ fun () -> 
    Unix.truncate path length; 
    extra.return ()
  in


  let stat_file path = 
    extra.safely @@ fun () -> 
    let open Unix in
    stat path |> fun st ->
    st.st_size |> fun sz ->        
    extra.return {sz}
  in


  let kind path = 
    extra.safely @@ fun () ->
    let open Unix in
    stat path |> fun st ->
    st.st_kind 
    |> (function
        | S_DIR -> (`Dir:st_kind)
        | S_REG -> (`File:st_kind)
        | _ -> `Other) 
    |> extra.return
  in

    
  let reset () = extra.return () in


  ignore(wf_ops 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
    ~create ~open_ ~pread ~pwrite ~close ~truncate 
    ~stat_file ~kind ~reset);
  mk_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset

(* at the moment we just re-raise e; if we alter m we can eg embed in result *)
let safely : 'a. (unit -> 'a m) -> 'a m = fun f aww w -> try f () aww w with e -> raise e

let return: 'a. 'a -> 'a m = fun x -> fun f -> f x

let extra = { safely; return }

let unix_ops = mk_ops ~extra ()


(* imperative ------------------------------------------------------- *)

let the_world : w = Obj.magic ()

let _ = the_world


(* some bug with ppx not working with local exceptions, so use first class modules instead *)
let run_imperative (type a) (f:a m) : a = 
  let module M = struct exception E of a end in
  try ignore(f (fun a w -> raise (M.E a)) the_world); failwith __LOC__
  with M.E a -> a

let _ = run_imperative

let run = { run=run_imperative }

let unix_imperative_ops = ops_to_imperative run unix_ops

let readdir' = readdir' ~ops:unix_imperative_ops
