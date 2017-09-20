(* minimal fs-like thing *)

(*
type error

(* type 'a promise  (* potentially takes a long time *) *)

type 'a comp (* computation that takes steps *)

type 'a err (* computation that may return err *)

(*
let bind: ('a -> 'b m) -> 'a m -> 'b m = failwith ""
let error : error -> 'b m = failwith ""
let return : 'a -> 'a m = failwith ""
(* type state  (* system state; gets passed in the monad *) *)

*)

*)
(*
let id_cases (id:id) ~(fid:fid -> 'a) ~(did:did -> 'a) = failwith ""
*)

(*
type file
type dir
*)


type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }

(* what we assume *)
module type S = sig
  type 'a m (* comp that takes steps and may return err *)

  (* these are intended to be ints; equality comparison ok *)
  (* path resolution is potentially expensive, so we allow clients
     access to an "id"; unlike fds, these are not guaranteed to remain
     valid - its a performance optimization; but in order to avoid the
     possibility of the trivial-do-nothing implementation it is
     guaranteed that at least the last min_valid_fids are valid; so an
     implementation must preserve files even if they are deleted from
     the dir-hierarchy (similar to posix fd behaviour); so a
     straightforward implementation is to open an fd and cache in an
     LRU, and close fd when expunged; alternatively, the connection
     resources can just be reset by the client, if the client is
     trusted not to exhaust resources... or just throw a resource
     exception if we open too many fids? *)
  type fid
  type did
  type id  (* = fid + did *)

  type rd (* readdir_descriptor *)

  type fd

  type buffer

end



module Make = functor (S:S) -> struct
  open S

  (* are rd and fd references? or are they values that can be duplicated
     etc? probably values, since they just record eg a did and an index *)
  let wf_ops (* type did fid rd fd *) 
      ~root ~resolve_path_relative ~resolve_path
      ~unlink ~mkdir ~rmdir ~opendir ~readdir ~closedir 
      ~create ~delete ~open_ ~pread ~pwrite ~close ~truncate
      ~stat_file ~kind ~reset
    =

    let root : did = root in

    (* note return parent; root has itself as parent; either an object
       exists, or we return none indicating that we can create an object *)
    let resolve_path_relative : did:did -> string -> (did * id option) m 
      = resolve_path_relative in

    let resolve_path : string -> (did * id option) m 
      = resolve_path in

    (* FIXME remove rmdir and delete? remove kind? *)
    let unlink : parent:did -> name:string -> kind:[ `Dir | `File ] -> unit m = unlink in

    let mkdir : parent:did -> name:string -> did m = mkdir in

    (* calls unlink *)
    let rmdir : parent:did -> (* did:did -> *) name:string -> unit m = rmdir in

    let opendir : did -> rd m = opendir in

    (* boolean indicates whether more to come *)
    let readdir : rd -> (string list * bool) m = readdir in

    let closedir : rd -> unit m = closedir in

    let create : parent:did -> name:string -> fid m = create in

    (* calls unlink *)
    let delete : parent:did -> (* fid:fid -> *) name:string -> unit m = delete in

    (* FIXME do we really need fid and fd since we only use pread and
       pwrite? fds are temporary, but fids are permanent; some impls may
       like to have fds separate *)
    let open_ : fid -> fd m = open_ in

    (* mutable buffers? really? *)
    let pread : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int 
      -> unit m 
      = pread in

    let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int 
      -> int m 
      = pwrite in

    let close : fd:fd -> unit m = close in

    let truncate : fid:fid -> int -> unit m = truncate in

    let stat_file : fid:fid -> file_stat m = stat_file in

    let kind : id -> st_kind m = kind in

    (* for this connection, forget all fds fids etc ie reduce caching
       and resources to minimal level *)
    let reset : unit -> unit m = reset in   

    true[@@ocaml.warning "-26"]

  let _ = wf_ops
end


