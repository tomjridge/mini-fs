open C_base
open D_unix

let return = Step_monad.return

type extra_ops = {
  safely: 'a. (w -> 'a m) -> 'a m;  (* this delays until receives a world *)
}

let mk_ops ~extra = 
  let root : path = "/" in


  let unlink ~parent ~name : unit m = 
    extra.safely @@ fun _ ->
    Unix.unlink @@ parent^"/"^name ;
    return ()
  in


  let default_perm = 0o640 in


  let mkdir ~parent ~name = 
    extra.safely @@ fun _ -> 
    Unix.mkdir (parent^"/"^name) default_perm;
    return ()
  in


  let mk_dh ~path = Unix.opendir path in

  let opendir path = 
    extra.safely @@ fun _ -> return (mk_dh path)
  in


  let readdir dh = 
    extra.safely @@ fun _ -> (* safely is just to delay till world available *)
    try Unix.readdir dh |> fun e -> return ([e],not finished)
    with _ -> return ([],finished)
  in


  let closedir dh = 
    extra.safely @@ fun _ -> Unix.closedir dh; return ()
  in  
  (* FIXME should we record which dh are valid? ie not closed *)


  let create ~parent ~name = 
    extra.safely @@ fun _ -> 
    let open Unix in
    openfile (parent^"/"^name) [O_CREAT] default_perm |> fun fd ->
    close fd;
    return ()
  in


  let mk_fd path = 
    extra.safely @@ fun _ ->
    Unix.(openfile path [O_RDWR] default_perm) |> return
  in

  let open_ path = mk_fd path in


  let pread ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    extra.safely @@ fun _ ->
    (* bigarray pread has no boff, and length is taken from array, so
       resort to slicing *)
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pread fd foff buffer |> fun nread ->
    return nread
  in


  let pwrite ~fd ~foff ~length ~(buffer:buffer) ~boff = 
    extra.safely @@ fun _ ->
    Bigarray.Array1.sub buffer boff length |> fun buffer -> 
    ExtUnix.All.BA.pwrite fd foff buffer |> fun n ->
    return n
  in


  let close fd = extra.safely @@ fun _ ->
    (Unix.close fd; return ()) 
  in 
  (* FIXME record which are open? *)


  let rename ~spath ~sname ~dpath ~dname = failwith "FIXME" in

  let truncate ~path ~length = 
    extra.safely @@ fun _ -> 
    Unix.truncate path length; 
    return ()
  in


  let stat_file path = 
    extra.safely @@ fun _ -> 
    let open Unix in
    stat path |> fun st ->
    st.st_size |> fun sz ->        
    return {sz}
  in


  let kind path = 
    extra.safely @@ fun _ ->
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
  

  { root; unlink; mkdir; opendir; readdir; closedir; create; open_;
    pread; pwrite; close; rename; truncate; stat_file; kind; reset }


let safely : 'a. (w -> 'a m) -> 'a m = 
  let open Step_monad in
  fun f -> 
    Step(fun w -> 
        try 
          match f w with
          | Finished a -> w,fun () -> Finished a
          | Step f' -> f' w
        with e -> {w with error_state=Some e }, fun() -> failwith __LOC__)
(* NOTE we should never step a state that has an error *)

let extra = { safely }

let unix_ops = mk_ops ~extra 


(* imperative ------------------------------------------------------- *)

let dest_exceptional w = w.error_state 

include struct
  open D_unix.Imp_ops_type

  let run ref_ (x:'a m) = 
    Step_monad.run ~dest_exceptional !ref_ x |> function
    | `Finished (w,a) -> (ref_:=w; a)
    | `Exceptional (e,w) -> 
      e |> Printexc.to_string |> fun s->
      Printf.printf "funix.162: run resulted in exceptional: %s\n" s;
      raise e

  let ref_ = ref initial_world
      
  let run x = run ref_ x

  let run : run = { run }

  let unix_imperative_ops = mk_imperative_ops ~run ~ops:unix_ops

end

