open Monad
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

let new_did () : (did,t) m = fun s -> failwith ""

let new_fid () : (fid,t) m = fun s -> failwith ""

let with_state : (state -> state) -> (unit,t) m = fun f -> failwith ""


let resolve_path (path:path) : (did * id option,t) m = failwith ""


let resolve_dir_path (path:path) : (did,t) m = 
  resolve_path path |> bind @@ function
  | (_,Some (Did did)) -> return did 
  | _ -> err __LOC__



let resolve_file_path (path:path) : (fid,t) m = 
  resolve_path path |> bind @@ function
  | (_,Some (Fid fid)) -> return fid 
  | _ -> err __LOC__



let root : path = "/" 


(* FIXME or just allow unlink with no expectation of the kind? *)
let unlink ~parent ~name = 
  resolve_dir_path parent |> bind @@ fun parent ->
  with_state 
    (fun s ->
       s.dirs |> fun dirs ->
       Map_did.find parent dirs |> fun pdir ->
       Map_string.find name pdir |> fun entry ->
       (* FIXME here and elsewhere we need to take care about find etc when key not present *)
       Map_string.remove name pdir |> fun pdir ->
       Map_did.add parent pdir dirs |> fun dirs ->
       {s with dirs})
  |> bind @@ fun () -> return ()



let mkdir ~parent ~name : (unit,t) m = 
  resolve_dir_path parent |> bind @@ fun parent ->
  new_did () |> bind @@ fun (did:did) -> 
  with_state 
    (fun s -> 
       s.dirs |> fun dirs ->
       Map_did.find parent s.dirs |> fun pdir ->
       Map_string.add name (Did did) pdir |> fun pdir ->
       Map_did.add parent pdir dirs |> fun dirs ->
       {s with dirs})
  |> bind @@ fun () -> return () (* did *)



let mk_dh ~did es = (did,es) 


let opendir path = 
  resolve_dir_path path |> bind @@ fun did ->
  ["FIXME"] |> mk_dh ~did |> return 



let readdir dh = dh |> function (did,es) -> return (es,false) 


let closedir dh = return ()  (* FIXME should we record which rd are valid? ie not closed *)


let create ~parent ~name : (unit,t) m = 
  resolve_dir_path parent |> bind @@ fun parent ->
  new_fid () |> bind @@ fun (fid:fid) -> 
  with_state 
    (fun s -> 
       s.dirs |> fun dirs ->
       Map_did.find parent dirs |> fun pdir ->
       Map_string.add name (Fid fid) pdir |> fun pdir ->
       Map_did.add parent pdir dirs |> fun dirs ->
       {s with dirs})
  |> bind @@ fun () -> return () (* fid *)



let mk_fd (fid:fid) = fid 


let open_ path = 
  resolve_file_path path |> bind @@ fun fid -> 
  fid |> mk_fd |> return 


let pread ~fd ~foff ~length ~buffer ~boff = 
  let fid = fd in
  fun s ->
    s.files |> fun map ->
    Map_fid.find fid map |> fun (contents:string) ->
    Bytes.blit_string contents foff buffer boff length;
    (Ok length,s)


let pwrite ~fd ~foff ~length ~buffer ~boff = 
  let fid = fd in
  fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    let contents = Bytes.of_string contents in
    Bytes.blit buffer boff contents foff length;  (* FIXME extend contents *)
    Bytes.to_string contents |> fun contents ->
    Map_fid.add fid contents files |> fun files ->
    (Ok length,{s with files})


let close fd = return () (* FIXME record which are open? *)


let truncate ~path ~length = 
  resolve_file_path path |> bind @@ fun fid ->
  fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    let contents' = Bytes.create length in
    Bytes.blit_string contents 0 contents' 0 length;
    Bytes.to_string contents' |> fun contents ->
    Map_fid.add fid contents files |> fun files ->
    (Ok (),{s with files})



let stat_file path = 
  resolve_file_path path |> bind @@ fun fid ->
  fun s ->
    s.files |> fun files ->
    Map_fid.find fid files |> fun contents ->
    String.length contents |> fun sz ->
    (Ok { sz },s)



let kind path : (st_kind,t) m = 
  resolve_path path |> bind @@ fun (_,id) ->    
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
