(* log calls and returns -------------------------------------------- *)

open Minifs
open Mini_nfs

let log (c:msg_from_client) : ('a,'m) m_ -> ('a,'m) m_ = 
  fun (x : ('a,'m) m_) ->
  fun (k : 'a -> 'm) ->
    let k = 
      fun a ->
        try
          c |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string
          |> print_endline;
          let r = k a in
          r 
        with e -> (
            Printexc.to_string e |> print_endline;
            raise e)
          
    in
    x k

let dh' = -99

let mk_logged_ops (type m) ~ops = 
  dest_ops ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
  assert(wf_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset);
  
  let unlink ~parent ~name = log (Unlink(parent,name)) (unlink ~parent ~name) in
  let mkdir ~parent ~name = log (Mkdir(parent,name)) (mkdir ~parent ~name) in
  let opendir p = log (Opendir(p)) (opendir p) in
  let readdir dh = log (Readdir(dh')) (readdir dh) in 
  let closedir dh = log (Closedir(dh')) (closedir dh) in
  let create ~parent ~name = log (Create(parent,name)) (create ~parent ~name) in
  let open_ p = log (Open(p)) (open_ p) in
  let pread ~fd ~foff ~length ~buffer ~boff =
    log (Pread(fd,foff,length)) (pread ~fd ~foff ~length ~buffer ~boff) in
  let pwrite ~fd ~foff ~length ~buffer ~boff =
    (* FIXME log (Pwrite(fd,foff,data???)) *) (pwrite ~fd ~foff ~length ~buffer ~boff) in
  let close fd = log (Close(fd)) (close fd) in
  let truncate ~path ~length = log (Truncate(path,length)) (truncate ~path ~length) in
  (* FIXME log the rest as well *)
  let stat_file path = log(Stat_file(path)) (stat_file path) in
  let kind path = log(Kind(path)) (kind path) in
  Minifs.mk_ops ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset
  
  
  


