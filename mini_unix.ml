open Minifs

(* unix impl -------------------------------------------------------- *)

module S = struct

  type fid = Unix.file_descr
  type did = Unix.dir_handle


  module Map_fid = Map.Make(
    struct 
      type t = fid let compare: t->t->int = Pervasives.compare 
    end)


  module Map_did = Map.Make(
    struct 
      type t = did let compare: t->t->int = Pervasives.compare 
    end)


  module Map_string = Map.Make(
    struct 
      type t = string let compare: t->t->int = Pervasives.compare 
    end)


  module Set_string = Set.Make(
    struct
      type t = string let compare: t->t->int = Pervasives.compare 
    end)


  type id = Fid of fid | Did of did 

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

module X_ = (S:S)


module T = Make(S)

module T' = T

open S

let err x = fun s -> (Error x,s)

let is_fid = function
  | Fid _ -> true
  | _ -> false

let is_did x = not (is_fid x)

let new_did () : did m = failwith ""

let _ = new_did

let new_fid () : fid m = failwith ""

let with_state : (state -> state) -> unit m = failwith ""

let bind : ('a -> 'b m) -> 'a m -> 'b m = fun f x s ->
  match x s with
  | (Ok y,s') -> f y s'
  | (Error _,_) as e -> e

let _ = bind

let return : 'a -> 'a m = fun x s -> (Ok x,s)

let _ = return

let _ = 
  let x : unit m = failwith "" in
  let f : unit -> int m = failwith "" in
  (x |> bind f)


let err x = failwith ""

let ops () = 
  let resolve_path (path:path) : (did * id option) m = failwith "" in

(*
  let resolve_dir_path (path:path) : did m = 
    resolve_path path |> bind @@ function
    | (_,Some (Did did)) -> return did 
    | _ -> err __LOC__
  in

  let resolve_file_path (path:path) : fid m = 
    resolve_path path |> bind @@ function
    | (_,Some (Fid fid)) -> return fid 
    | _ -> err __LOC__
  in
*)

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

  let rmdir ~parent ~name = unlink ~parent ~name in

  let mk_dh ~path = Unix.opendir path in

  let opendir path = mk_dh path |> return in

  let readdir dh = 
    try Unix.readdir dh |> fun e -> return ([e],true) 
    with _ -> return ([],false)
  in

  let closedir dh = Unix.closedir dh; return () in  (* FIXME should we record which dh are valid? ie not closed *)

  let create ~parent ~name : unit m = 
    with_state 
      Unix.(fun s -> 
         openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
         close fd;
         s)
    |> bind @@ fun () -> return () (* fid *)
  in

  let delete ~parent ~name = unlink ~parent ~name in

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

  let truncate ~path i = 
    fun s ->
      Unix.truncate path i; 
      (Ok (),s)
  in

  let stat_file path = 
    fun s -> Unix.(
        stat path |> fun st ->
        st.st_size |> fun sz ->        
        (Ok{sz},s))
  in

  let kind path : st_kind m = 
    resolve_path path |> bind @@ fun (_,id) ->    
    id |> function 
    | None -> err @@ `No_such_entry
    | Some x -> x |> function
      | Fid fid -> return (`File:st_kind)
      | Did did -> return (`Dir:st_kind)
  in
    
  let reset () = return () in
  

  assert(T.wf_ops 
           ~root 
           (* ~resolve_path_relative ~resolve_path *)
           ~unlink ~mkdir ~rmdir ~opendir ~readdir ~closedir 
           ~create ~delete ~open_ ~pread ~pwrite ~close ~truncate
           ~stat_file ~kind ~reset);
  fun k -> k 
      ~root 
      (* ~resolve_path_relative ~resolve_path *)
      ~unlink ~mkdir ~rmdir ~opendir ~readdir ~closedir 
      ~create ~delete ~open_ ~pread ~pwrite ~close ~truncate
      ~stat_file ~kind ~reset

