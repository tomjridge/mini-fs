(* minimal fs-like thing *)

type st_kind = [`Dir | `File | `Symlink | `Other ]

type file_stat = { sz:int }


module type S = sig
  type 'a m 
  (* comp that takes steps and may return err *)

  type path

  type dh (* dir_handle, for reading dirs *)

  type fd

  type buffer

end



module Make = functor (S:S) -> struct
  open S

  let wf_ops 
      ~root 
      ~unlink ~mkdir (* ~rmdir *) ~opendir ~readdir ~closedir 
      ~create (* ~delete *) ~open_ ~pread ~pwrite ~close ~truncate
      ~stat_file ~kind ~reset
    =

    let root : path = root in

    (* FIXME remove rmdir and delete? remove kind? *)
    let unlink : parent:path -> name:string -> unit m = unlink in

    let mkdir : parent:path -> name:string -> unit m = mkdir in

    (* calls unlink *)
    (* let rmdir : parent:path -> name:string -> unit m = rmdir in *)

    let opendir : path -> dh m = opendir in

    (* boolean indicates whether more to come *)
    let readdir : dh -> (string list * bool) m = readdir in

    let closedir : dh -> unit m = closedir in

    let create : parent:path -> name:string -> unit m = create in

    (* calls unlink *)
    (* let delete : parent:path -> name:string -> unit m = delete in *)

    let open_ : path -> fd m = open_ in

    (* mutable buffers? really? *)
    let pread : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int 
      -> int m 
      = pread in

    let pwrite : fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int 
      -> int m 
      = pwrite in

    let close : fd -> unit m = close in

    let truncate : path:path -> int -> unit m = truncate in

    let stat_file : path -> file_stat m = stat_file in

    let kind : path -> st_kind m = kind in

    (* for this connection, forget all fds fids etc ie reduce caching
       and resources to minimal level *)
    let reset : unit -> unit m = reset in   

    true[@@ocaml.warning "-26"]

  let _ = wf_ops
end


