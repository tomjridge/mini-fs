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
      ~unlink ~mkdir ~rmdir ~opendir ~readdir ~closedir 
      ~create ~delete ~open_ ~pread ~pwrite ~close ~truncate
      ~stat
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

    let stat : id:id -> stat m = stat in

    true[@@ocaml.warning "-26"]

  let _ = wf_ops
end






(* in-mem impl ------------------------------------------------------ *)

module S = struct

  type fid(* = int *)
  type did(* = int *)


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

let err x = failwith ""

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
  | (y,s') -> f y s'

let _ = bind

let return : 'a -> 'a m = fun x s -> (x,s)

let _ = return

let _ = 
  let x : unit m = failwith "" in
  let f : unit -> int m = failwith "" in
  (x |> bind f)


let ops = 
  let 
    resolve_path_relative,resolve_path
    = failwith "" 
  in
  let root : did = failwith "" (* 0 *) in

  (* FIXME or just allow unlink with no expectation of the kind? *)
  let unlink ~parent ~name ~kind = 
    with_state 
      (fun s ->
         s.dirs |> fun dirs ->
         Map_did.find parent dirs |> fun pdir ->
         Map_string.find name pdir |> fun entry ->
         (* FIXME here and elsewhere we need to take care about find etc when key not present *)
         let entry_kind_matches = 
           match () with
           | _ when (is_fid entry && kind=`File) -> true
           | _ when (is_did entry && kind=`Dir) -> true
           | _ -> false
         in
         entry_kind_matches |> function
         | true -> 
           Map_string.remove name pdir |> fun pdir ->
           Map_did.add parent pdir dirs |> fun dirs ->
           {s with dirs}
         | false -> err `Unlink_kind_fails_to_match)
    |> bind @@ fun () -> return ()
  in

  let mkdir ~parent ~name : did m = 
    new_did () |> bind @@ fun (did:did) -> 
    with_state 
      (fun s -> 
         s.dirs |> fun dirs ->
         Map_did.find parent s.dirs |> fun pdir ->
         Map_string.add name (Did did) pdir |> fun pdir ->
         Map_did.add parent pdir dirs |> fun dirs ->
         {s with dirs})
    |> bind @@ fun () -> return did
  in

  let rmdir ~parent ~name = unlink ~parent ~name ~kind:`Dir in

  let mk_rd ~did es = (did,es) in

  let opendir did = ["FIXME"] |> mk_rd ~did |> return in

  let readdir rd = rd |> function (did,es) -> return (es,false) in

  let closedir rd = return () in  (* FIXME should we record which rd are valid? ie not closed *)

  let create ~parent ~name : fid m = 
    new_fid () |> bind @@ fun (fid:fid) -> 
    with_state 
      (fun s -> 
         s.dirs |> fun dirs ->
         Map_did.find parent dirs |> fun pdir ->
         Map_string.add name (Fid fid) pdir |> fun pdir ->
         Map_did.add parent pdir dirs |> fun dirs ->
         {s with dirs})
    |> bind @@ fun () -> return fid
  in

  let delete ~parent ~name = unlink ~parent ~name ~kind:`File in

  let mk_fd fid = fid in

  let open_ fid = mk_fd fid |> return in

  let pread ~fd ~foff ~length ~buffer ~boff = 
    let fid = fd in
    fun s ->
      s.files |> fun map ->
      Map_fid.find fid map |> fun (contents:string) ->
      Bytes.blit_string contents foff buffer boff length;
      ((),s)
  in

  let pwrite ~fd ~foff ~length ~buffer ~boff = 
    let fid = fd in
    fun s ->
      s.files |> fun files ->
      Map_fid.find fid files |> fun contents ->
      let contents = Bytes.of_string contents in
      Bytes.blit buffer boff contents foff length;  (* FIXME extend contents *)
      Bytes.to_string contents |> fun contents ->
      Map_fid.add fid contents files |> fun files ->
      (length,{s with files})
  in

  let close ~fd = return () in  (* FIXME record which are open? *)

  let truncate ~fid i = 
    fun s ->
      s.files |> fun files ->
      Map_fid.find fid files |> fun contents ->
      let contents' = Bytes.create i in
      Bytes.blit_string contents 0 contents' 0 i;
      Bytes.to_string contents' |> fun contents ->
      Map_fid.add fid contents files |> fun files ->
      ((),{s with files})
  in

  let stat ~id s = id |> function
    | Fid fid -> 
      s.files |> fun files ->
      Map_fid.find fid files |> fun contents ->
      String.length contents |> fun sz ->
      ({ sz; st_kind=`File },s)
    | Did did ->
      s.dirs |> fun dirs ->
      Map_did.find did dirs |> fun dir ->
      let sz = -1 in
      ({ sz; st_kind=`Dir },s)
  in


  assert(T.wf_ops 
           ~root ~resolve_path_relative ~resolve_path
           ~unlink ~mkdir ~rmdir ~opendir ~readdir ~closedir 
           ~create ~delete ~open_ ~pread ~pwrite ~close ~truncate
           ~stat);
  fun k -> k 
      ~root ~resolve_path_relative ~resolve_path
      ~mkdir ~rmdir ~opendir ~readdir ~closedir 
      ~create ~delete ~open_ ~pread ~pwrite ~close 
      ~stat
