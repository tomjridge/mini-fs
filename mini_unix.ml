open Minifs

(* unix impl -------------------------------------------------------- *)

module S = struct

  type path = string

  type state = {
    dummy: unit;
  }

  type dh = Unix.dir_handle

  type fd = Unix.file_descr

  type buffer = bytes  (* or cstruct? *)

  type 'a or_error = ('a,exn) result
  
  type 'a m = state -> ('a or_error * state)

end

module T = Make(S)

open S
open T


let err x = fun s -> (Error x,s)

let safely : 'a m -> 'a m = fun f s -> 
  try f s 
  with e -> (Error e,s)

let with_state : (state -> 'a * state) -> 'a m = fun f -> 
  safely @@ (fun s -> f s |> fun (x,s) -> (Ok x,s))

let bind : ('a -> 'b m) -> 'a m -> 'b m = fun f x s ->
  match x s with
  | (Ok y,s') -> f y s'
  | (Error _,_) as e -> e

let return : 'a -> 'a m = fun x s -> (Ok x,s)


(* run a command against a ref holding a state *)
let run_imperative ~ref_ f = 
  f !ref_ |> fun (x,s) ->
  ref_:=s;
  x|> function
  | Ok x -> x
  | Error e -> raise e

let _ = run_imperative

(* special case for dummy state *)
let run_imperative f =
  run_imperative ~ref_:(ref {dummy=()}) f

let _ = run_imperative

exception No_such_entry

let mk_ops () = 
  let root : path = "/" in


  let unlink ~parent ~name = 
    with_state @@ fun s ->
    Unix.unlink @@ parent^"/"^name ;
    ((),s)
  in


  let default_perm = 0o640 in


  let mkdir ~parent ~name : unit m = 
    with_state @@ fun s -> 
    Unix.mkdir (parent^"/"^name) default_perm;
    ((),s)
  in



  let mk_dh ~path = Unix.opendir path in

  let opendir path = safely (mk_dh path |> return) in


  let readdir dh = 
    try Unix.readdir dh |> fun e -> return ([e],not finished) 
    with _ -> return ([],finished)
  in


  let closedir dh = safely (Unix.closedir dh; return ()) in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name : unit m = 
    with_state @@ fun s -> 
    let open Unix in
    openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
    close fd;
    ((), s)
  in


  let mk_fd path = Unix.(openfile path [O_RDWR] default_perm) in

  let open_ path = safely (mk_fd path |> return) in


  let pread ~fd ~foff ~length ~buffer ~boff = 
    with_state @@ fun s ->
    ExtUnix.All.pread fd foff buffer boff length |> fun nread ->
    (nread,s)
  in


  let pwrite ~fd ~foff ~length ~buffer ~boff = 
    with_state @@ fun s ->
    ExtUnix.All.pwrite fd foff (Bytes.to_string buffer) boff length |> fun n ->
    (n,s)
  in


  let close fd = safely (Unix.close fd; return ()) in (* FIXME record which are open? *)


  let truncate ~path ~length = 
    with_state @@ fun s ->
    Unix.truncate path length; 
    ((),s)
  in


  let stat_file path = 
    with_state @@ fun s -> 
    let open Unix in
    stat path |> fun st ->
    st.st_size |> fun sz ->        
    ({sz},s)
  in


  let kind path : st_kind m = 
    safely @@ 
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


  {
    root;
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
  }  




(* extra ops -------------------------------------------------------- *)

(* this is to make top-level interaction a bit smoother *)

module Extra_ops = functor (S:sig val ops: T.ops end) -> struct

  open S

  let run = run_imperative

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

end

let ops = mk_ops()

module Extra_ops_ = Extra_ops(struct let ops=ops end)
