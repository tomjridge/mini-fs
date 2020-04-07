(** Minifs main types; safe to open.

There is a lot of overlap with {!Stdlib.Unix}. We want our code to run
   in contexts where Unix is not available. Thus, we have to define
   many equiv types. We also want to use eg the OCaml Fuse bindings,
   which use {!Stdlib.Unix} types. So we need to be able to convert
   between our types and the existing types in Unix.

FIXME a lot of these types should be moved to fs_shared

*)

(** {2 Misc} *)


(** Alias *)
type buffer = Bigarray_buffer.buffer
type buf = buffer

(** Alias for result FIXME why? *)
type ('a,'e)r_ = ('a,'e)result


(** fd and dh represented by ints... which is the usual underlying repn *)
module Int_base_types = struct
  type fd = int
  type dh = int

  let fd2i (x:fd) : int = x
  let i2fd (x:int) : fd = x
  let dh2i (x:dh) : int = x
  let i2dh (x:int) : dh = x
end

type finished = {finished:bool}


(** Some utility functions *)
module Base_extra = struct

  (* ensure 64 bit system *)
  let _ = assert(Sys.int_size = 63)


  let exit_1 = failwith  (* hopefully not be caught FIXME remove this *)


  (* following for strings *)
  let dirname_basename path = 
    assert (String_.starts_with ~prefix:"/" path);
    String_.split_on_last ~sub:"/" path |> fun (p,c) -> 
    (* the semantics is that dirname is an absolute path *)
    (if p="" then "/" else p),c


  type length = int (* FIXME in following *)
  type offset = int


  (* FIXME replace path and dh with int-like and string-like *)
  open Bin_prot.Std
  type path=string [@@deriving bin_io, yojson]
  type dh=int  (* FIXME why specialize here? *)
end
let exit_1 = Base_extra.exit_1

(*
module Finished = struct
  (* FIXME remove these? *)
  module Export = struct
    type is_finished = {is_finished:bool}
  end
  include Export
  let finished = {is_finished=true}
  let unfinished = {is_finished=false}
  (* let not x = { is_finished=not x.is_finished} *)
end
include Finished.Export
*)


(** {2 Errors} *)

module Error_ = struct

  (** Typical errors we encounter *)
  type exn_ = [ 
    | `Error_no_entry
    | `Error_not_directory
    | `Error_not_file
    | `Error_not_symlink
    | `Error_attempt_to_rename_dir_over_file
    | `Error_attempt_to_rename_root
    | `Error_attempt_to_rename_to_subdir
    | `Error_no_src_entry
    | `Error_path_resolution
    | `Error_not_empty
    | `Error_exists
    | `Error_is_directory
    | `Error_other
  ] [@@deriving bin_io, yojson]

  let exn__to_string e = 
    e |> exn__to_yojson |> Yojson.Safe.pretty_to_string

  (** Convert exn_ to a unix exception FIXME some refinement needed FIXME move to unix_convs? *)
  let mk_unix_exn (e:exn_) = Unix.(e |> function
    | `Error_no_entry                        -> Unix_error(ENOENT, "154","")
    | `Error_not_directory                   -> Unix_error(ENOTDIR, "155","")
    | `Error_not_file                        -> Unix_error(EINVAL, "156","") (* FIXME *)
    | `Error_not_symlink                     -> Unix_error(EINVAL, "156b","") (* FIXME *)
    | `Error_attempt_to_rename_dir_over_file -> Unix_error(EINVAL, "157","") (* FIXME *)
    | `Error_attempt_to_rename_root          -> Unix_error(EINVAL, "158","") (* FIXME *)
    | `Error_attempt_to_rename_to_subdir     -> Unix_error(EINVAL, "159","") (* FIXME *)
    | `Error_no_src_entry                    -> Unix_error(ENOENT, "160","")
    | `Error_path_resolution                 -> Unix_error(EUNKNOWNERR 999,"162","") (* FIXME *)
    | `Error_not_empty                       -> Unix_error(ENOTEMPTY,"163","")
    | `Error_exists                          -> Unix_error(EEXIST,"163","")
    | `Error_is_directory                    -> Unix_error(EISDIR,"165","")
    | `Error_other                           -> Unix_error(EUNKNOWNERR 999,"161",""))

  (* NOTE going the other way, for unix_ops, we want to trap a
     particular error and return the corresponding exn_ *)

  (* First are errors we can deal with; Second need context *)

  (** Convert a Unix exception to one of two classes: First are
       those that we can deal with by mapping to our [exn] type; the
       second are those that we have to handle manually *)
  let map_error = function
    | Unix.EEXIST -> First `Error_exists
    | EINVAL -> Second `EINVAL  (* !!! *)
    | EISDIR -> First `Error_is_directory
    | ENOENT -> First `Error_no_entry
    | ENOTDIR -> First `Error_not_directory
    | ENOTEMPTY -> First `Error_not_empty
    | e -> 
      (* FIXME maybe add more here *)
      Printf.printf "Unknown error: %s\n%!" (Unix.error_message e);
      Second `SOME_OTHER_ERROR  (* !!! *)

  let _ = map_error

  (** NOTE these are the errors that we have to handle manually when
      they are caught from Unix. calls *)
  type map_error_second = [ `EINVAL | `SOME_OTHER_ERROR ]

end
(* include Error_ *)
type exn_ = Error_.exn_

(** Errors for each API function... an attempt to refine the different
   types of error each call can return FIXME needs further refinement
   *)
(* FIXME combine with Error_ ? *)
module Call_specific_errors = struct

  (* FIXME the following should be refined *)
  type err_         = Error_.exn_
  type unlink_err   = err_
  type mkdir_err    = err_
  type opendir_err  = err_
  type readdir_err  = err_
  type closedir_err = err_
  type create_err   = err_
  type open_err     = err_
  type pread_err    = err_
  type pwrite_err   = err_
  type close_err    = err_ (* EBADF, but for valid fd, fd will be closed *)
  type rename_err   = err_
  type truncate_err = err_
  type stat_err     = err_
  type symlink_err  = err_
  type readlink_err = err_

  (* we often need to map our errors into standard unix errors eg when
     dealing with fuse; in the unix module, we need to construct the
     reverse mapping *)

  (* Our version of the Unix_error exception; we want to pattern match
     exhaustively and name the type *)
  type unix_error_ = [`Unix_error of Unix.error * string * string ]

  let unknown_error = `Unix_error(Unix.EUNKNOWNERR 999,"FIXME","FIXME")
end




(** {2 Stat records} *)

module Kind = struct
  type st_kind = [`Dir | `File | `Symlink | `Other ] [@@deriving bin_io,yojson]
end
open Kind

module Times = struct
  open Bin_prot.Std
  type times = {
    atim:float; 
    (* ctim:unit;   *)
    (* by default, we don't use this, but return ctim as mtim since atim
       apparently doesn't affect atim*)
    mtim:float;
  } [@@deriving bin_io,yojson]
end
type times = Times.times
open Times

(** Simplified stat record *)
module Stat_record = struct
  open Bin_prot.Std
  type stat_record = { sz:int; kind:st_kind; times:times } 
  [@@deriving bin_io,yojson]
end
open Stat_record
type stat_record = Stat_record.stat_record
(* include Stat_record *)


(** Conversions to/from Unix equivalents *)
module St_convs = struct
  open Log_

  let unix2times st = Unix.LargeFile.{
      atim=st.st_atime;
      (* ctim=(); *)
      mtim=st.st_mtime;
    }

  let unix2kind = function
    | Unix.S_DIR -> (`Dir:st_kind)
    | S_REG -> (`File:st_kind)
    | S_LNK -> (`Symlink)
    | _ -> `Other

    let kind2unix = function
      | `Dir -> Unix.S_DIR
      | `File -> S_REG
      | `Symlink -> S_LNK
      | `Other -> S_BLK  (* FIXME *)

  (** Default file and directory stat records *)
  module Default = struct
    open Unix
    open LargeFile

    let default_dir_stats = LargeFile.stat "."

    let default_file_stats = 
      (* try to take them from the current directory *)
      { default_dir_stats with
        st_kind=S_REG;
        st_perm= 0o660;
        st_nlink= 1;
      }

    let stat2unix (stat:stat_record) = 
      match stat.kind with
      | `Dir -> 
        {default_dir_stats with 
         st_size=Int64.of_int stat.sz;  (* FIXME use int64 *)
         st_kind=kind2unix stat.kind; 
         st_atime=stat.times.atim; 
         st_ctime=stat.times.mtim;
         st_mtime=stat.times.mtim;
        }
      | `File -> 
        {default_file_stats with
         st_size=Int64.of_int stat.sz; 
         st_kind=kind2unix stat.kind; 
         st_atime=stat.times.atim; 
         st_ctime=stat.times.mtim;
         st_mtime=stat.times.mtim;
        }
      | `Symlink -> 
        {default_file_stats with
         st_size=Int64.of_int stat.sz;
         st_kind=kind2unix stat.kind;
         st_atime=stat.times.atim; 
         st_ctime=stat.times.mtim;
         st_mtime=stat.times.mtim;
        }
      | `Other -> 
        log_.log ("Unknown stat, `Other, at "^__LOC__);
        (* FIXME what to do here? *)
        {default_file_stats with
         st_kind=kind2unix stat.kind }


    let unix2stat stat = 
      match stat.st_kind with
      | S_DIR -> { sz=1; kind=`Dir; times=unix2times stat }
      | S_REG -> { sz=Int64.to_int stat.st_size; kind=`File; times=unix2times stat }
      | S_LNK -> { sz=Int64.to_int stat.st_size; kind=`Symlink; times=unix2times stat }
      | _ -> { sz=Int64.to_int stat.st_size; kind=`Other; times=unix2times stat }
  end

  let stat2unix = Default.stat2unix
  let unix2stat = Default.unix2stat

end




(** {2 Msgs btwn client and server} *)

module Msgs = struct
  open Base_extra

  open Bin_prot.Std

  type length = int[@@deriving bin_io, yojson]
  type offset = int[@@deriving bin_io, yojson]


  (* for messages going to server, fd and dh are ints *)
  type dh = int [@@deriving bin_io, yojson]

  type fd = int [@@deriving bin_io, yojson]

  type data = string[@@deriving bin_io, yojson]

  (** Messages sent from the client *)
  type msg_from_client = 
    | Unlink of path
    | Mkdir of path
    | Opendir of path 
    | Readdir of dh 
    | Closedir of dh
    | Create of path
    | Open of path 
    | Pread of fd * offset * length
    | Pwrite of fd * offset * data
    | Close of fd
    | Rename of path * path
    | Truncate of path * length
    | Stat of path
    | Symlink of string * path
    | Readlink of path
    | Reset
  [@@deriving bin_io, yojson]


  (** Messages received from the server *)
  type msg_from_server' = 
    | Unit
    | Int of int
    | Dh of dh
    | Readdir' of string list * bool
    | Open' of fd
    | Pread' of data
    | Stat' of stat_record
    | Kind' of st_kind 
    | Readlink' of string
  [@@deriving bin_io, yojson]


  (* or just use error in monad? *)
  (** Messages received from the server *)
  type msg_from_server = 
    | Ok_ of msg_from_server' 
    | Error_ of Error_.exn_ 
  [@@deriving bin_io, yojson]
  (* FIXME Error is probably a bad choice of constructor - prefer Exn_ *)


  (** Convert server msg to string *)
  let msg_s_to_string m = 
    m |> msg_from_server_to_yojson |> Yojson.Safe.pretty_to_string

  (** Convert string to server msg *)
  let string_to_msg_s s = 
    s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
    |> function
    | Ok x -> x
    | Error _e -> 
      exit_1 __LOC__        

  (** Convert client msg to string *)
  let msg_c_to_string m = 
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string

  (** Convert string to message from client *)
  let string_to_msg_c s = 
    s |> Yojson.Safe.from_string |> msg_from_client_of_yojson
    |> function
    | Ok x -> x
    | Error _e -> 
      exit_1 __LOC__ 
end






(** {2 The main filesystem operations: unlink, mkdir etc} *)

module Ops_type = struct
  open Call_specific_errors

  type path = string  

  type ('fd,'dh,'w) ops = {
    root     : path;
    unlink   : path -> ((unit,unlink_err)r_, 'w) m;
    mkdir    : path -> ((unit,mkdir_err)r_,'w) m;
    opendir  : path -> (('dh,opendir_err)r_, 'w) m;
    readdir  : 'dh  -> ((string list * finished,readdir_err)r_, 'w) m;
    (** NOTE . and .. are returned *)

    closedir : 'dh  -> ((unit,closedir_err)r_,  'w) m;
    create   : path -> ((unit,create_err)r_,  'w) m;
    (** create is to create a file; use mkdir for a dir *)

    open_    : path -> (('fd,open_err)r_,  'w) m;
    pread    : fd:'fd -> foff:int -> len:int -> buf:buf -> boff:int -> 
      ((int,pread_err)r_, 'w) m; 
    pwrite   : fd:'fd -> foff:int -> len:int -> buf:buf -> boff:int -> 
      ((int,pwrite_err)r_, 'w) m;
    close    : 'fd  -> ((unit,close_err)r_,  'w) m;

    rename   : path -> path -> ((unit,rename_err)r_,  'w) m;

    truncate : path -> int -> ((unit,truncate_err)r_,  'w) m;
    (** truncate a file to a given len *)

    stat     : path -> ((stat_record,stat_err)r_,  'w) m;
    symlink  : path -> path -> ((unit,symlink_err)r_, 'w) m;
    readlink : path -> ((string,readlink_err)r_,'w) m;
    reset    : unit -> (unit,  'w) m;
  }


end


type ('fd,'dh,'t) ops = ('fd,'dh,'t) Ops_type.ops

type path = Ops_type.path



(** Extract a value from a monad (Fuse lives in the real-world, so we
   need a way to project from a monad like lwt) *)
type 'w co_eta = {
  co_eta: 'a. ('a,'w) m -> 'a
}
