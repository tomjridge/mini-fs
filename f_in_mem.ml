
(* this is in the monad, so we can just use the unix monad *)
open Tjr_map
open B_step_monad
open C_base
open C_in_mem
open D_mem
open Monad

type 'e extra_ops = {
  err: 'a. 'e -> 'a m;
  new_did: unit -> did m;
  new_fid: unit -> fid m;
  with_fs: 'a. (fs_t -> 'a * fs_t) -> 'a m;  (* ASSUME-ed not to raise exception *)
  internal_err: 'a. string -> 'a m;
  dirs_add: did -> dir_with_parent -> unit m;
  is_parent: parent:did -> child:dir_with_parent -> bool m
}


let mk_ops ~extra = 
  let err = extra.err in
  let ( >>= ) = bind in

  let resolve_did did = 
    extra.with_fs (fun s ->
        (dirs_ops.map_find did s.dirs,s)) >>= fun r -> 
    match r with 
    | None -> extra.internal_err "resolve_did, did not valid mim.l154"
    | Some dir -> return dir
  in

  let _ = resolve_did in

  let resolve_name ~dir ~name : id option = dir_find name dir in

  let rec resolve_names_1 ~parent_id ~names : (did * id option) m = 
    resolve_did parent_id >>= fun parent -> 
    resolve_names_2 ~parent_id ~parent ~names 

  and resolve_names_2 ~parent_id ~parent ~names = 
    match names with
    | [] -> return @@ (parent_id,None)
    | name::names -> 
      begin
        resolve_name ~dir:parent ~name |> function
        | None -> (
            match names with
            | [] -> return @@ (parent_id,None)
            | _ -> err @@ `Error_no_entry name) (* FIXME give full path *)
        | Some (Fid fid) -> (
            match names with 
            | [] -> return @@ (parent_id,Some (Fid fid))
            | _ -> err @@ `Error_not_directory)
        | Some (Did did) -> (
            match names with
            | [] -> return @@ (parent_id,Some (Did did))
            | _ -> resolve_names_1 ~parent_id:did ~names)
      end
  in


  let _ = resolve_names_1 in

  let resolve_path : path -> (did * id option) m = fun p -> 
    (if not @@ String.contains p '/' 
     then extra.internal_err "resolve_path, no slash, mim.l189" 
     else 
       String.split_on_char '/' p |> fun names ->
       if not (List.hd names = "")
       then extra.internal_err "resolve_path, no leading slash, mim.194"
       else 
         let names = List.tl names in
         if names=[] 
         then extra.internal_err "resolve_path, names empty, impossible, mim.198"
         else 
           let names = Tjr_list.(if last names = "" then butlast names else names) in
           (* not sure about special casing root *)
           if names = [] 
           then return @@ (root_did,Some (Did root_did)) 
           else resolve_names_1 ~parent_id:root_did ~names)
  in

  let _ = resolve_path in


  let resolve_dir_path (path:path) : did m = 
    resolve_path path >>= function
    | (_,Some (Did did)) -> return did 
    | _ -> err `Error_not_directory
  in


  let resolve_file_path (path:path) : fid m = 
    resolve_path path >>= function
    | (_,Some (Fid fid)) -> return fid 
    | _ -> err `Error_not_file
  in

  let root : path = "/" in


  (* FIXME or just allow unlink with no expectation of the kind? *)
  let unlink ~parent ~name = 
    resolve_dir_path parent >>= fun pid ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_find pid dirs |> fun pdir ->
        match pdir with 
        | None -> `Internal "impossible, parent not found, mim.233",s
        | Some pdir -> 
          dir_find name pdir |> fun entry ->
          match entry with
          | None -> `Error_no_entry,s
          | Some entry -> 
            dir_remove name pdir |> fun pdir ->
            dirs_ops.map_add pid pdir dirs |> fun dirs ->
            `Ok,{s with dirs}) >>= function 
    | `Ok -> return ()
    | `Internal s -> extra.internal_err s
    | `Error_no_entry -> err @@ `Error_no_entry "unlink, mim.272"
  in

  let _ = unlink in 


  (* FIXME check is already exists etc *)
  let mkdir ~parent ~name : unit m = 
    resolve_dir_path parent >>= fun pid ->
    extra.new_did () >>= fun (did:did) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        (* add new empty dir to dirs *)
        dirs_ops.map_add did (empty_dir ~parent:pid) dirs |> fun dirs ->
        (* add name to parent *)
        dirs_ops.map_find pid s.dirs |> fun pdir ->
        match pdir with 
        | None -> `Internal "impossible, parent not found mim.l259",s
        | Some pdir -> 
          dir_add name (Did did) pdir |> fun pdir ->
          (* update parent in dirs *)
          dirs_ops.map_add pid pdir dirs |> fun dirs ->
          `Ok,{s with dirs}) >>= function
    | `Ok -> return ()
    | `Internal s -> extra.internal_err s
  in


  (* let mk_dh ~did es = (did,es) in *)


  let opendir path = 
    resolve_dir_path path >>= fun did ->
    extra.with_fs (fun s ->
        s.dirs |> fun dirs ->
        dirs_ops.map_find did dirs |> function
        | None -> `Internal "opendir, impossible, dir not found mim.278",s
        | Some dir -> 
          dir_bindings dir |> List.map fst |> fun names ->
          1+s.max_dh |> fun dh ->
          s.dir_handles |> fun handles ->
          dhandles_ops.map_add dh names handles |> fun dir_handles ->
          {s with dir_handles; max_dh=dh } |> fun s ->                            
          `Ok(dh),s) >>= function
    | `Ok dh -> return dh
    | `Internal s -> extra.internal_err s
  in


  let readdir dh = 
    extra.with_fs (fun s ->
        s.dir_handles |> fun handles ->
        dhandles_ops.map_find dh handles |> function
        | None -> `Internal "readdir, impossible, dh not found mim.328",s
        | Some xs -> `Ok xs,s) >>= function
    | `Ok xs -> return (xs,finished)
    | `Internal s -> extra.internal_err s
  in


  let closedir dh = return () in (* FIXME should we record which rd are valid? ie not closed *)


  let create ~parent ~name : unit m = 
    resolve_dir_path parent >>= fun parent ->
    extra.new_fid () >>= fun (fid:fid) -> 
    extra.with_fs (fun s -> 
        s.dirs |> fun dirs ->
        dirs_ops.map_find parent dirs |> function
        | None -> `Internal "create, impossible, parent did not found mim.299",s
        | Some pdir ->
          dir_add name (Fid fid) pdir |> fun pdir ->
          dirs_ops.map_add parent pdir dirs |> fun dirs ->
          {s with dirs} |> fun s ->
          s.files |> fun files -> 
          files_ops.map_add fid "" files |> fun files ->
          `Ok,{s with files}) >>= function
    | `Internal s -> extra.internal_err s
    | `Ok -> return ()
  in


  let mk_fd (fid:fid) = fid (* ExtUnix.All.file_descr_of_int fid *) in

  let fd2int fd = fd (* ExtUnix.All.int_of_file_descr fd *) in


  let open_ path = 
    resolve_file_path path >>= fun fid -> 
    fid |> mk_fd |> return 
  in

  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    let fid = fd2int fd in
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "pread, impossible, no file, mim.325",s
        | Some(contents:string) ->
          (* don't try to read more bytes than available *)
          let max_length = String.length contents - foff in
          let length = min max_length length in
          let error_case = 
            foff+length > String.length contents ||  (* should be impossible *)
            boff+length > Bigarray.Array1.dim buffer  (* FIXME fuse prevents this? *)
          in
          match error_case with
          | true -> `Internal "pread, invalid blit arguments, mim.342",s
          | false -> 
            blit_string_to_bigarray 
              ~src:contents ~soff:foff ~len:length 
              ~dst:buffer ~doff:boff;
            (`Ok length,s)) >>= function
    | `Internal s -> extra.internal_err s
    | `Ok l -> return l
  in

  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    let fid = fd2int fd in
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "pwrite, impossible, no file, mim.339",s
        | Some contents ->
          (* convert contents to bytes, and extend if necessary *)
          let contents = 
            if foff+length > String.length contents 
            then 
              Bytes.create (foff+length) |> fun contents' ->
              blit_string_to_bytes
                ~src:contents ~soff:0 ~len:(String.length contents) 
                ~dst:contents' ~doff:0;
              contents'
            else Bytes.of_string contents 
          in
          let error_case = 
            foff+length > Bytes.length contents ||   (* shouldn't happen *)
            boff+length > Bigarray.Array1.dim buffer  (* presumably fuse prevents this *)
          in
          match error_case with
          | true -> `Internal "pwrite, invalid blit arguments, mim.357",s
          | false -> 
            let contents = Bytes.of_string contents in
            blit_bigarray_to_bytes 
              ~src:buffer ~soff:boff ~len:length ~dst:contents ~doff:foff; 
            (* FIXME extend contents *)
            Bytes.to_string contents |> fun contents ->
            files_ops.map_add fid contents files |> fun files ->
            (`Ok length,{s with files})) >>= function
    | `Internal s -> extra.internal_err s
    | `Ok l -> return l
  in


  let close fd = return () in (* FIXME record which are open? *)


  (* FIXME ddir and sdir may be the same, so we need to be careful to
     always use indirection via did *)
  let rename ~spath ~sname ~dpath ~dname = 
    resolve_dir_path spath >>= fun sdir_did ->
    resolve_dir_path dpath >>= fun ddir_did ->
    resolve_did sdir_did >>= fun sdir ->
    resolve_name ~dir:sdir ~name:sname |> function
    | None -> err `Error_no_src_entry
    | Some resolved_sname -> 
      resolve_did ddir_did >>= fun ddir ->
      resolve_name ~dir:ddir ~name:dname |> fun resolved_dname ->
      let insert_and_remove id =
        match sdir_did = ddir_did with
        | true -> 
          dir_add dname id ddir |> fun ddir ->
          dir_remove sname ddir |> fun ddir ->
          extra.dirs_add ddir_did ddir >>= fun () ->
          return ()
        | false -> 
          dir_add dname id ddir |> fun ddir ->
          dir_remove sname sdir |> fun sdir ->
          extra.dirs_add sdir_did sdir >>= fun () ->
          extra.dirs_add ddir_did ddir >>= fun () ->
          return ()
      in
      match resolved_sname,resolved_dname with
      | Fid fid,None -> insert_and_remove (Fid fid)
      | Fid fid1,Some(Fid fid2) -> 
        if fid1=fid2 
        then return ()
        else insert_and_remove (Fid fid1)
      | Fid fid,Some(Did did) ->
        extra.internal_err "FIXME rename f to d, d should be empty?"
      | Did sdid,None -> 
        (* check not root *)
        if ddir_did=root_did 
        then err `Error_attempt_to_rename_root
        (* check not subdir *)
        else extra.is_parent ~parent:sdid ~child:ddir >>= (function
            | true -> err `Error_attempt_to_rename_to_subdir
            | false -> 
              (* FIXME other checks *)
              resolve_did sdid >>= fun sdir -> 
              (* new directory id *)
              extra.new_did() >>= fun did ->
              (* record new directory with updated parent *)
              extra.dirs_add did { sdir with parent=ddir_did } >>= fun () ->
              insert_and_remove (Did did))
      | Did sdid,Some(Fid fid) -> 
        err `Error_attempt_to_rename_dir_over_file  (* FIXME correct ? *)
      | Did sdid,Some(Did ddid) ->
        if sdid=ddid 
        then return ()
        else extra.internal_err "FIXME rename d to d, dst should be empty?"
  in

  let truncate ~path ~length = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "file not found mim.362",s
        | Some contents ->
          let contents' = Bytes.create length in
          Bytes.blit_string 
            contents 0 contents' 0 (min (Bytes.length contents) length);
          Bytes.to_string contents' |> fun contents ->
          files_ops.map_add fid contents files |> fun files ->
          `Ok (),{s with files}) >>= function
    | `Ok () -> return ()
    | `Internal s -> extra.internal_err s
  in


  let stat_file path = 
    resolve_file_path path >>= fun fid ->
    extra.with_fs (fun s ->
        s.files |> fun files ->
        files_ops.map_find fid files |> function
        | None -> `Internal "file fid not found mim.379",s
        | Some contents ->
          String.length contents |> fun sz ->
          `Ok { sz },s) >>= function
    | `Ok stat -> return stat
    | `Internal s -> extra.internal_err s
  in


  let kind path : st_kind m = 
    resolve_path path >>= fun (_,id) ->    
    id |> function 
    | None -> err @@ `Error_no_entry path
    | Some x -> x |> function
      | Fid fid -> return (`File:st_kind)
      | Did did -> return (`Dir:st_kind)
  in


  let reset () = return () in

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat_file; kind; reset }


(* ------------------------------------------------------------------ *)

module Step_monad = B_step_monad

let with_fs (f:fs_t -> 'a * fs_t) : 'a m = 
  Step_monad.with_state 
    (fun w -> f w.fs |> fun (x,fs') -> x,{w with fs=fs'}) 
    (fun a -> return a)

let _ = with_fs

let new_did () = with_fs (fun fs ->
    { fs with max_did=(inc_did fs.max_did) } |> fun fs' ->
    let did = fs'.max_did in
    did,fs')

let new_fid () = with_fs (fun fs ->
    { fs with max_fid=(inc_fid fs.max_fid) } |> fun fs' ->
    let fid = fs'.max_fid in
    fid,fs')

let _ = new_fid

let internal_err s : 'a m = 
  Step(fun w -> 
      ({ w with internal_error_state=(Some s)}, fun () -> 
          "Fatal error: attempt to step internal errror state mim.449\n" |> fun s ->
          print_endline s;
          failwith s))

let _ = internal_err


let dirs_find k =
  with_fs (fun s ->
      s.dirs |> fun dirs ->
      dirs_ops.map_find k dirs |> fun v ->
      v,s)

(* FIXME we could maintain a list of parents when resolving, of course *)
let is_parent ~parent ~child = 
  let rec is_p (child:dir_with_parent) = 
    if child.parent = parent then return true
    else if child.parent = root_did then return false
    else is_p' child.parent
  and is_p' (child:did) =
    dirs_find child >>= function
    | None ->  internal_err "did not valid, mim.418"
    | Some child -> is_p child
  in
  is_p child

let dirs_add k v =
  with_fs (fun s ->
      s.dirs |> fun dirs ->
      dirs_ops.map_add k v dirs |> fun dirs ->
      (),{s with dirs})

let err e : 'a m = 
  Step(fun w -> 
      ({ w with thread_error_state=(Some e)}, fun () -> 
          "Fatal error: attempt to step thread errror state mim.460\n" |> fun s ->
          print_endline s;
          failwith s))

let extra = { new_did; new_fid; with_fs; internal_err; is_parent; dirs_add; err }

let ops = mk_ops ~extra  

let _ = ops


(* running ---------------------------------------------------------- *)

let is_exceptional w = w.thread_error_state <> None || w.internal_error_state <> None

let log_state = ref false

let rec run w (x:'a m) = 
  match is_exceptional w with
  | true -> `Exceptional w
  | false -> 
    match x with
    | Finished a -> `Finished(a,w)
    | Step f -> 
      f w |> fun (w',rest) ->
      (if !log_state 
       then Printf.printf "run mim.511: result state: %s\n" (t_to_string w'));
      if is_exceptional w' 
      then `Exceptional w' 
      else run w' (rest())


(* imperative ------------------------------------------------------- *)

open D_mem.Imp

let imp_run ref_ : run = {
  run=(fun x -> run (!ref_) x |> function
    | `Exceptional w -> 
      "Run resulted in exceptional state" |> fun s ->
      print_endline s;
      failwith s
    | `Finished(a,w) -> 
      ref_:=w;
      a)
}


let mk_imperative_ops ~ref_ = mk_imperative_ops ~run:(imp_run ref_) ~ops 



(* logging ---------------------------------------------------------- *)

module Msgs = C_msgs

(* FIXME should also log exceptional returns *)
let log msg (x:'a m) = 
  let call = msg |> Msgs.msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string in
  let rec log_return = function
    | Finished a -> 
      Printf.printf "call %s returns\n" call;
      Finished a
    | Step(f) -> Step(
        fun w -> 
          f w |> fun (w',rest) -> 
          (w',fun () -> log_return (rest())))
  in
  let log_call_and_return = Step(
      fun w -> 
        Printf.printf "call %s starts\n" call;
        w,fun () -> log_return x)
  in
  log_call_and_return

let log_op = { log }

