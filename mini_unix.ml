open Minifs

(* to make integration with fuse easier; extunix supports this in module BA *)
type fuse_buffer = Fuse.buffer

type buffer = fuse_buffer

(* unix impl -------------------------------------------------------- *)



type path = string

type state = {
  dummy: unit;
}

type t = state

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

type extra_ops = {
  safely: 'a. ('a,t) m -> ('a,t) m;
  with_state: 'a. (state -> 'a * state) -> ('a,t) m;
}

let mk_ops ~extra () = 
  let root : path = "/" in


  let unlink ~parent ~name = 
    extra.with_state @@ fun s ->
    Unix.unlink @@ parent^"/"^name ;
    ((),s)
  in


  let default_perm = 0o640 in


  let mkdir ~parent ~name = 
    extra.with_state @@ fun s -> 
    Unix.mkdir (parent^"/"^name) default_perm;
    ((),s)
  in



  let mk_dh ~path = Unix.opendir path in

  let opendir path = extra.safely (mk_dh path |> return) in


  let readdir dh = 
    try Unix.readdir dh |> fun e -> return ([e],not finished) 
    with _ -> return ([],finished)
  in


  let closedir dh = extra.safely (Unix.closedir dh; return ()) in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name = 
    extra.with_state @@ fun s -> 
    let open Unix in
    openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
    close fd;
    ((), s)
  in


  let mk_fd path = Unix.(openfile path [O_RDWR] default_perm) in

  let open_ path = extra.safely (mk_fd path |> return) in


  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    extra.with_state @@ fun s ->
    (* bigarray pread has no boff, and length is taken from array, so
       resort to slicing *)
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pread fd foff buffer |> fun nread ->
    (nread,s)
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    extra.with_state @@ fun s ->
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pwrite fd foff buffer |> fun n ->
    (n,s)
  in


  let close fd = extra.safely (Unix.close fd; return ()) in (* FIXME record which are open? *)


  let truncate ~path ~length = 
    extra.with_state @@ fun s ->
    Unix.truncate path length; 
    ((),s)
  in


  let stat_file path = 
    extra.with_state @@ fun s -> 
    let open Unix in
    stat path |> fun st ->
    st.st_size |> fun sz ->        
    ({sz},s)
  in


  let kind path = 
    extra.safely @@ 
    let open Unix in
    stat path |> fun st ->
    st.st_kind 
    |> (function
        | S_DIR -> (`Dir:st_kind)
        | S_REG -> (`File:st_kind)
        | _ -> `Other) 
    |> return
  in

    
  let reset () = return () in


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
  run_imperative ~ref_:(ref {dummy=()}) f

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
