(* open Tjr_monad *)
open Minifs

(* in-mem impl ------------------------------------------------------ *)

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

type t = state

type dh = did * string list (* inefficient *)


type fd = fid


type path = string


type buffer = bytes  (* or cstruct? *)


let err x = failwith ""

let is_fid = function
  | Fid _ -> true
  | _ -> false

let is_did x = not (is_fid x)

let new_did () : (did -> 'm) -> 'm = err "FIXME"

let new_fid () : (fid -> 'm) -> 'm = err "FIXME"

(* sort of co-inductive object *)
type m = Trans of (state -> state * m)

(* following could be parameters *)
let with_state : (state -> state) -> (unit -> 'm) -> 'm = 
  fun f k -> Trans (fun s ->
      f s, k ())

let _ = with_state

let with_state': (state -> 'a * state) -> ('a -> 'm) -> 'm = 
  fun f k -> 
    Trans (fun s -> 
        f s |> fun (a,s) ->
        s,k a)

let _ = with_state'


let resolve_path : path -> ((did * id option -> 'm)) -> 'm = failwith ""

(* 'a comp is ('a -> 'm ) -> 'm  where 'm represents the computation *) 

let ( >>= ) x f = fun k -> (x (fun rx -> f rx k))

let bind f x = fun k -> (x (fun rx -> f rx k))

let _ = bind

let return x k = k x

let resolve_dir_path (path:path) : (did -> 'm) -> 'm = 
  resolve_path path >>= function
  | (_,Some (Did did)) -> return did 
  | _ -> err __LOC__



let resolve_file_path (path:path) : (fid -> 'm) -> 'm = 
  resolve_path path >>= function
  | (_,Some (Fid fid)) -> return fid 
  | _ -> err __LOC__



let root : path = "/" 


(* FIXME or just allow unlink with no expectation of the kind? *)
let unlink ~parent ~name = 
  resolve_dir_path parent >>= fun parent ->
  with_state 
    (fun s ->
       s.dirs |> fun dirs ->
       Map_did.find parent dirs |> fun pdir ->
       Map_string.find name pdir |> fun entry ->
       (* FIXME here and elsewhere we need to take care about find etc when key not present *)
       Map_string.remove name pdir |> fun pdir ->
       Map_did.add parent pdir dirs |> fun dirs ->
       {s with dirs})
  >>= fun () -> return ()



let mkdir ~parent ~name : (unit -> 'm) -> 'm = 
  resolve_dir_path parent >>= fun parent ->
  new_did () >>= fun (did:did) -> 
  with_state 
    (fun s -> 
       s.dirs |> fun dirs ->
       Map_did.find parent s.dirs |> fun pdir ->
       Map_string.add name (Did did) pdir |> fun pdir ->
       Map_did.add parent pdir dirs |> fun dirs ->
       {s with dirs})
  >>= fun () -> return () (* did *)



let mk_dh ~did es = (did,es) 


let opendir path = 
  resolve_dir_path path >>= fun did ->
  ["FIXME"] |> mk_dh ~did |> return 



let readdir dh = dh |> function (did,es) -> return (es,false) 


let closedir dh = return ()  (* FIXME should we record which rd are valid? ie not closed *)


let create ~parent ~name : (unit -> 'm) -> 'm = 
  resolve_dir_path parent >>= fun parent ->
  new_fid () >>= fun (fid:fid) -> 
  with_state 
    (fun s -> 
       s.dirs |> fun dirs ->
       Map_did.find parent dirs |> fun pdir ->
       Map_string.add name (Fid fid) pdir |> fun pdir ->
       Map_did.add parent pdir dirs |> fun dirs ->
       {s with dirs})
  >>= fun () -> return () (* fid *)



let mk_fd (fid:fid) = fid 


let open_ path = 
  resolve_file_path path >>= fun fid -> 
  fid |> mk_fd |> return 


let pread ~fd ~foff ~length ~buffer ~boff = 
  let fid = fd in
  with_state' @@ fun s ->
    s.files |> fun map ->
    Map_fid.find fid map |> fun (contents:string) ->
    Bytes.blit_string contents foff buffer boff length;
    (length,s)


let pwrite ~fd ~foff ~length ~buffer ~boff = 
  let fid = fd in
  with_state' @@ fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    let contents = Bytes.of_string contents in
    Bytes.blit buffer boff contents foff length;  (* FIXME extend contents *)
    Bytes.to_string contents |> fun contents ->
    Map_fid.add fid contents files |> fun files ->
    (length,{s with files})


let close fd = return () (* FIXME record which are open? *)


let truncate ~path ~length = 
  resolve_file_path path >>= fun fid ->
  with_state' @@ fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    let contents' = Bytes.create length in
    Bytes.blit_string contents 0 contents' 0 length;
    Bytes.to_string contents' |> fun contents ->
    Map_fid.add fid contents files |> fun files ->
    ((),{s with files})



let stat_file path = 
  resolve_file_path path >>= fun fid ->
  with_state' @@ fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    String.length contents |> fun sz ->
    ({ sz },s)



let kind path : (st_kind -> 'm) -> 'm = 
  resolve_path path >>= fun (_,id) ->    
  id |> function 
  | None -> err @@ `No_such_entry
  | Some x -> x |> function
    | Fid fid -> return (`File:st_kind)
    | Did did -> return (`Dir:st_kind)


    
let reset () = return () 
  


let _ = wf_ops 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
    ~create ~open_ ~pread ~pwrite ~close ~truncate 
    ~stat_file ~kind ~reset    
