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

type stat = { sz:int; st_kind:st_kind }

(* what we assume *)
module type S = sig
  type 'a m (* comp that takes steps and may return err *)

  (* these are intended to be ints; equality comparison ok *)
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
      ~mkdir ~rmdir ~opendir ~readdir ~closedir 
      ~create ~delete ~open_ ~pread ~pwrite ~close 
      ~stat
    =

    let root : did = root in

    (* note return parent; root has itself as parent; either an object
       exists, or we return none indicating that we can create an object *)
    let resolve_path_relative : did:did -> string list -> (did * id option) m 
      = resolve_path_relative in

    (* or string -> (did * id option) promise m *)
    let resolve_path : string list -> (did * id option) m 
      = resolve_path in

    let mkdir : parent:did -> name:string -> did m = mkdir in

    let rmdir : parent:did -> did:did -> name:string -> unit m = rmdir in

    let opendir : did -> rd m = opendir in

    (* boolean indicates whether more to come *)
    let readdir : rd:rd -> (string list * bool) m = readdir in

    let closedir : rd:rd -> unit m = closedir in


    let create : parent:did -> name:string -> fid m = create in

    let delete : parent:did -> fid:fid -> name:string -> unit m = delete in

    (* FIXME do we really need fid and fd since we only use pread and pwrite? *)
    let open_ : fid -> fd m = open_ in

    let pread : fd:fd -> off:int -> length:int -> buffer:buffer -> buffer m 
      = pread in

    let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int 
      -> int m 
      = pwrite in

    let close : fd:fd -> unit m = close in

    let stat : fid:fid -> stat m = stat in

    true[@@ocaml.warning "-26"]

  let _ = wf_ops
end






(* in-mem impl ------------------------------------------------------ *)

module S = struct

  type fid = int
  type did = int


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


  type state = {
    files: string Map_fid.t;
    dirs: id Map_string.t Map_did.t;
  }

  type rd = did * string list (* inefficient *)

  type fd = fid

  type buffer = bytes  (* or cstruct? *)

  type 'a m = state -> 'a * state

end

module X_ = (S:S)


module T = Make(S)

module T' = T

open S

let new_did () : did m = failwith ""

let with_state : (state -> state) -> unit m = failwith ""

let ( |>> ) : 'a m -> ('a -> 'b m) -> 'b m = failwith ""

let return : 'a -> 'a m = failwith ""

let ops = 
  let 
    root,resolve_path_relative,resolve_path,
    mkdir,rmdir,opendir,readdir,closedir,create,delete,
    open_,pread,pwrite,close,stat = failwith "" 
  in
  let root = 0 in
  let mkdir ~parent ~name = 
    new_did () |>> fun did ->
    let ws = with_state 
        (fun s -> 
           let map = Map_did.find parent s.dirs in
           let map' = Map_string.add name (Did did) map in           
           {s with dirs=Map_did.add did map' s.dirs})
    in
    (* ws |>> fun () -> FIXME typecheck? *) return did
  in
  assert(T.wf_ops 
           ~root ~resolve_path_relative ~resolve_path
           ~mkdir ~rmdir ~opendir ~readdir ~closedir 
           ~create ~delete ~open_ ~pread ~pwrite ~close 
           ~stat);
  fun k -> k 
      ~root ~resolve_path_relative ~resolve_path
      ~mkdir ~rmdir ~opendir ~readdir ~closedir 
      ~create ~delete ~open_ ~pread ~pwrite ~close 
      ~stat
