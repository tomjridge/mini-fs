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

let with_state : (state -> state) -> unit m = failwith ""

let bind : ('a -> 'b m) -> 'a m -> 'b m = fun f x s ->
  match x s with
  | (Ok y,s') -> f y s'
  | (Error _,_) as e -> e

let return : 'a -> 'a m = fun x s -> (Ok x,s)

exception No_such_entry

let mk_ops () = 
  let resolve_path (path:path) : (path * string option) m = failwith "" in


  let root : path = "/" in


  let unix_unlink ~parent ~name = Unix.unlink @@ parent^"/"^name in


  (* FIXME or just allow unlink with no expectation of the kind? *)
  let unlink ~parent ~name = 
    with_state 
      (fun s ->
         unix_unlink ~parent ~name;
         s)
    |> bind @@ fun () -> return ()
  in


  let default_perm = 0o640 in


  let unix_mkdir ~parent ~name = Unix.mkdir (parent^"/"^name) default_perm in

  let mkdir ~parent ~name : unit m = 
    with_state 
      (fun s -> 
         unix_mkdir ~parent ~name;
         s)
    |> bind @@ fun () -> return ()
  in



  let mk_dh ~path = Unix.opendir path in

  let opendir path = mk_dh path |> return in


  let readdir dh = 
    try Unix.readdir dh |> fun e -> return ([e],true) 
    with _ -> return ([],false)
  in


  let closedir dh = Unix.closedir dh; return () in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name : unit m = 
    with_state 
      Unix.(fun s -> 
         openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
         close fd;
         s)
    |> bind @@ fun () -> return () (* fid *)
  in


  let mk_fd path = Unix.(openfile path [O_RDWR] default_perm) in

  let open_ path = mk_fd path |> return in


  let pread ~fd ~foff ~length ~buffer ~boff = 
    fun s ->
      ExtUnix.All.pread fd foff buffer boff length |> fun nread ->
      (Ok nread,s)
  in


  let pwrite ~fd ~foff ~length ~buffer ~boff = 
    fun s ->
      ExtUnix.All.pwrite fd foff (Bytes.to_string buffer) boff length 
      |> fun n ->
      (Ok n,s)
  in


  let close fd = Unix.close fd; return () in (* FIXME record which are open? *)


  let truncate ~path ~length = 
    fun s ->
      Unix.truncate path length; 
      (Ok (),s)
  in


  let stat_file path = 
    fun s -> Unix.(
        stat path |> fun st ->
        st.st_size |> fun sz ->        
        (Ok{sz},s))
  in


  let kind path : st_kind m = 
    resolve_path path |> bind @@ fun (_,name) ->    
    name |> function 
    | None -> err @@ No_such_entry
    | Some name -> 
      Unix.(
        stat (path^"/"^name) |> fun st ->
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


