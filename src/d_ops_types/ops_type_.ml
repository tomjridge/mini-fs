open Tjr_monad.Monad
open Base_
open R_
open Error_types

(* sig -------------------------------------------------------------- *)

type path = string

(* NOTE R is expected to be result, but for imperative ops we might
   eliminate the 'e component and throw an exception instead

   FIXME even so, probably easier just to use result everywhere

*)
module type OPS_TYPE = sig
  type ('fd,'dh,'w) ops = {
    root: path;
    unlink : path -> ((unit,unlink_err)r_,  'w) m;
    mkdir : path -> ((unit,mkdir_err)r_,  'w) m;
    opendir : path -> (('dh,opendir_err)r_,  'w) m;
    (* . and .. are returned *)
    readdir : 'dh -> ((string list * is_finished,readdir_err)r_,  'w) m;
    closedir : 'dh -> ((unit,closedir_err)r_,  'w) m;
    create : path -> ((unit,create_err)r_,  'w) m;
    open_ : path -> (('fd,open_err)r_,  'w) m;
    pread: 
      fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      ((int,pread_err)r_,  'w) m; 
    pwrite: 
      fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      ((int,pwrite_err)r_,  'w) m;
    close : 'fd -> ((unit,close_err)r_,  'w) m;
    rename: path -> path -> ((unit,rename_err)r_,  'w) m;
    truncate : path:path -> length:int -> ((unit,truncate_err)r_,  'w) m;
    stat : path -> ((stat_record,stat_err)r_,  'w) m;
    reset : unit -> (unit,  'w) m;
  }
end


(* FIXME duplication with sig *)
(* don't use this module - just use the included type in this file *)
module Ops_type_with_result' = struct
  type ('fd,'dh,'w) ops = {
    root: path;
    unlink : path -> ((unit,unlink_err)r_,  'w) m;
    mkdir : path -> ((unit,mkdir_err)r_,  'w) m;
    opendir : path -> (('dh,opendir_err)r_,  'w) m;
    (* . and .. are returned *)
    readdir : 'dh -> ((string list * is_finished,readdir_err)r_,  'w) m;
    closedir : 'dh -> ((unit,closedir_err)r_,  'w) m;
    create : path -> ((unit,create_err)r_,  'w) m;
    open_ : path -> (('fd,open_err)r_,  'w) m;
    pread: 
      fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      ((int,pread_err)r_,  'w) m; 
    pwrite: 
      fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      ((int,pwrite_err)r_,  'w) m;
    close : 'fd -> ((unit,close_err)r_,  'w) m;
    rename: path -> path -> ((unit,rename_err)r_,  'w) m;
    truncate : path:path -> length:int -> ((unit,truncate_err)r_,  'w) m;
    stat : path -> ((stat_record,stat_err)r_,  'w) m;
    reset : unit -> (unit,  'w) m;
  }
end

include Ops_type_with_result'
