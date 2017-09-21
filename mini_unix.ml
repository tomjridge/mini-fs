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

let safely : 'a m -> 'a m = fun x -> x  (* wrap in exception handling *)

let with_state : (state -> 'a * state) -> 'a m = fun f -> failwith ""  (* use safely *)

let bind : ('a -> 'b m) -> 'a m -> 'b m = fun f x s ->
  match x s with
  | (Ok y,s') -> f y s'
  | (Error _,_) as e -> e

let return : 'a -> 'a m = fun x s -> (Ok x,s)



exception No_such_entry

let mk_ops () = 
  (* let resolve_path (path:path) : (path * string option) m = failwith "" in *)


  let root : path = "/" in


  (* FIXME make these use "with state", which traps errors, and returns unit *)
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
    try Unix.readdir dh |> fun e -> return ([e],true) 
    with _ -> return ([],false)
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
    Unix.(
      stat path |> fun st ->
      st.st_kind |> (function
          | S_DIR -> (`Dir:st_kind)
          | S_REG -> (`File:st_kind)
          | _ -> `Other) |> return)
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


