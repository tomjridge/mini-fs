open Base_
open R_
open Error_types

(* sig -------------------------------------------------------------- *)

(* NOTE R is expected to be result, but for imperative ops we
   eliminate the 'e component and throw an exception instead *)
module type OPS_TYPE = sig
  include Monad_type_.MONAD
  include BASE_TYPES
  include R
  type ops = {
    root: path;
    unlink : path -> (unit,unlink_err)r_ m;
    mkdir : path -> (unit,mkdir_err)r_ m;
    opendir : path -> (dh,opendir_err)r_ m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)r_ m;
    closedir : dh -> (unit,closedir_err)r_ m;
    create : path -> (unit,create_err)r_ m;
    open_ : path -> (fd,open_err)r_ m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)r_ m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)r_ m;
    close : fd -> (unit,close_err)r_ m;
    rename: path -> path -> (unit,rename_err)r_ m;
    truncate : path:path -> length:int -> (unit,truncate_err)r_ m;
    stat : path -> (stat_record,stat_err)r_ m;
    reset : unit -> unit m;
  }
end

