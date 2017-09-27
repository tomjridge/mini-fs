open Minifs

(* to make integration with fuse easier; extunix supports this in module BA *)
type fuse_buffer = Fuse.buffer

type buffer = fuse_buffer

(* unix impl -------------------------------------------------------- *)



type path = string

(*
type state = {
  dummy: unit;
}
*)

(*type t = state*)

type dh = Unix.dir_handle

type fd = Unix.file_descr

(* type buffer = bytes  (* or cstruct? *) *)

(* type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t *)

(*
type 'a or_error = ('a,exn) result

type 'a m = state -> ('a or_error * state)
*)

(*
let err x = fun s -> (Error x,s)

let exn_err e = err (e|>Printexc.to_string)
*)

(*
let safely : ('a,t) m -> ('a,t) m = fun f s -> 
  try f s 
  with e -> exn_err e s
*)

(*
let with_state : (state -> 'a * state) -> ('a,t) m = fun f -> 
  safely @@ (fun s -> f s |> fun (x,s) -> (Ok x,s))
*)

exception No_such_entry

(* ops -------------------------------------------------------------- *)

type w

type 'a m = ('a -> w -> w) -> w -> w


let ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m = fun x -> failwith ""

type extra_ops = {
  safely: 'a. (unit -> 'a m) -> 'a m;  (* this delays until receives a world *)
(*  with_state: 'a. (state -> 'a * state) -> ('a -> 'm) -> 'm; *)
  return: 'a. 'a -> 'a m;
}


(*

safely is ?

(('a -> 'm) -> 'm) -> (('a -> 'm)-> 'm)

and we want to install an exception handler for the part that computes 'a

this suggests that hat(a) (the repr of a computation producing a) is

('a -> world -> world) -> world -> world

ie 'm in ('a -> 'm) -> 'm is a function type world -> world, and the 

normal or exceptional termination can be handled via exceptions which
return the modified world state

*)

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
  in (* FIXME record which are open? *)


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

let unix_ops = mk_ops ()

(* imperative ------------------------------------------------------- *)

(* run a command against a ref holding a state *)
let run_imperative ~ref_ f = 
  f !ref_ |> fun (x,s) ->
  ref_:=s;
  x|> function
  | Ok x -> x
  | Error e -> failwith e

let _ = run_imperative

(* special case for dummy state *)
let run_imperative f =
  run_imperative ~ref_:(ref ()) f

let _ = run_imperative

(* FIXME TODO 

let run = { run=run_imperative }

let unix_imperative_ops = ops_to_imperative run unix_ops


let readdir' = readdir' ~ops:unix_imperative_ops

*)

(* old -------------------------------------------------------------- *)

(* 

  let mk_toplevel k = 
    let root= ops.root in
    let unlink=(fun ~parent ~name -> run @@ ops.unlink ~parent ~name) in
    let mkdir=(fun ~parent ~name -> run @@ ops.mkdir ~parent ~name) in
    let opendir=(fun p -> run @@ ops.opendir p) in
    let readdir=(fun dh -> run @@ ops.readdir dh) in
    let closedir=(fun dh -> run @@ ops.closedir dh) in
    let create=(fun ~parent ~name -> run @@ ops.create ~parent ~name) in
    let open_=(fun path -> run @@ ops.open_ path) in
    let pread=(fun ~fd ~foff ~length ~buffer ~boff -> run @@ ops.pread ~fd ~foff ~length ~buffer ~boff) in
    let pwrite=(fun ~fd ~foff ~length ~buffer ~boff -> run @@ ops.pwrite ~fd ~foff ~length ~buffer ~boff) in
    let close=(fun fd -> run @@ ops.close fd) in
    let truncate=(fun ~path ~length -> run @@ ops.truncate ~path ~length) in
    let stat_file=(fun path -> run @@ ops.stat_file path) in
    let kind=(fun path -> run @@ ops.kind path) in
    let reset=(fun () -> run @@ ops.reset ()) in
    k ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset

  let (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset) 
    = mk_toplevel @@ fun 
      ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
    (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset) 

  (* for small directories *)
  let readdir path = 
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

  (* FIXME similarly, can change pread/pwrite to work with path, or
     just implement write_string and read_string *)

let ops = mk_ops()

module Extra_ops_ = Extra_ops(struct let ops=ops end)
*)
