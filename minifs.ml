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


type is_finished = bool
let finished = true


module Make = functor (S:S) -> struct
  open S

  (* FIXME surely unlink just takes a path? *)
  type unlink = parent:path -> name:string -> unit m
  type mkdir = parent:path -> name:string -> unit m
  type opendir = path -> dh m

  (* . and .. are returned *)
  type readdir = dh -> (string list * is_finished) m
  type closedir = dh -> unit m 
  type create = parent:path -> name:string -> unit m
  type open_ = path -> fd m
  type pread = fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m 
  type pwrite = fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> int m 
  type close = fd -> unit m
  type truncate = path:path -> length:int -> unit m
  type stat_file = path -> file_stat m
  type kind = path -> st_kind m
  type reset = unit -> unit m

  
  type ops = {
    root:path;
    unlink:unlink;
    mkdir:mkdir;
    opendir:opendir;
    readdir:readdir;
    closedir:closedir;
    create:create;
    open_:open_;
    pread:pread;
    pwrite:pwrite;
    close:close;
    truncate:truncate;
    stat_file:stat_file;
    kind:kind;
    reset:reset;
  }


(*    ignore {
      unlink;
      mkdir;
      opendir;
      readdir;
      closedir;
      create;
      open_;
      pread;
      pwrite;
      close;
      truncate;
      stat_file;
      kind;
      reset;
    };
*)
end


