type ('a,'e)r_ = ('a,'e)result

module Error_ = struct
  (* open Bin_prot.Std *)

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

  include struct
    open Unix
    let mk_unix_exn (e:exn_) = e |> function
      | `Error_no_entry -> Unix_error(ENOENT, "154","")
      | `Error_not_directory -> Unix_error(ENOTDIR, "155","")
      | `Error_not_file -> Unix_error(EINVAL, "156","") (* FIXME *)
      | `Error_not_symlink -> Unix_error(EINVAL, "156b","") (* FIXME *)
      | `Error_attempt_to_rename_dir_over_file -> Unix_error(EINVAL, "157","") (* FIXME *)
      | `Error_attempt_to_rename_root -> Unix_error(EINVAL, "158","") (* FIXME *)
      | `Error_attempt_to_rename_to_subdir -> Unix_error(EINVAL, "159","") (* FIXME *)
      | `Error_no_src_entry -> Unix_error(ENOENT, "160","")
      | `Error_path_resolution -> Unix_error(EUNKNOWNERR 999,"162","") (* FIXME *)
      | `Error_not_empty -> Unix_error(ENOTEMPTY,"163","")
      | `Error_exists -> Unix_error(EEXIST,"163","")
      | `Error_is_directory -> Unix_error(EISDIR,"165","")
      | `Error_other -> Unix_error(EUNKNOWNERR 999,"161","")
  end

  (* NOTE going the other way, for unix_ops, we want to trap a
     particular error and return the corresponding exn_ *)

  (* First are errors we can deal with; Second need context *)
  include struct
    (* open Tjr_either *)
    open Unix
    let map_error = function
      | EEXIST -> First `Error_exists
      | EINVAL -> Second `EINVAL  (* !!! *)
      | EISDIR -> First `Error_is_directory
      | ENOENT -> First `Error_no_entry
      | ENOTDIR -> First `Error_not_directory
      | ENOTEMPTY -> First `Error_not_empty
      | e -> 
        (* FIXME maybe add more here *)
        Printf.sprintf "Unknown error: %s\n" (Unix.error_message e) |> print_endline;
        Second `SOME_OTHER_ERROR  (* !!! *)
    let _ = map_error

    (* NOTE these are the errors that we have to handle manually when
       they are caught from Unix. calls *)
    type map_error_second = [ `EINVAL | `SOME_OTHER_ERROR ]
  end
end
include Error_

module Stat_record = struct
  open Log_
  open Bin_prot.Std

  type st_kind = [`Dir | `File | `Symlink | `Other ] [@@deriving bin_io,yojson]


  type meta = {
    atim:float; 

    ctim:unit;  
    (* by default, we don't use this, but return ctim as mtim since atim
       apparently doesn't affect atim*)

    mtim:float;
  } [@@deriving bin_io,yojson]


  include struct
    open Unix.LargeFile
    let unix2meta st = {
      atim=st.st_atime;
      ctim=();
      mtim=st.st_mtime;
    }
  end



  type stat_record = { sz:int; kind:st_kind; meta:meta } 
  [@@deriving bin_io,yojson]



  include struct
    open Unix
    let unix2kind = function
      | S_DIR -> (`Dir:st_kind)
      | S_REG -> (`File:st_kind)
      | S_LNK -> (`Symlink)
      | _ -> `Other

    let kind2unix = function
      | `Dir -> S_DIR
      | `File -> S_REG
      | `Symlink -> S_LNK
      | `Other -> S_BLK  (* FIXME *)
  end


  module Default = struct
    open Unix
    open LargeFile
    let default_file_stats = 
      (* ASSUMES this file is present *)
      LargeFile.stat "tmp.txt"  

    let default_file_stats = 
      { default_file_stats with 
        st_nlink = 1;
        st_kind=Unix.S_REG;
        st_perm = 0o640;
      }

    let default_dir_stats = LargeFile.stat "."

    let stat2unix stat = 
      match stat.kind with
      | `Dir -> 
        {default_dir_stats with 
         st_size=Int64.of_int stat.sz;  (* FIXME use int64 *)
         st_kind=kind2unix stat.kind; 
         st_atime=stat.meta.atim; 
         st_ctime=stat.meta.mtim;
         st_mtime=stat.meta.mtim;
        }
      | `File -> 
        {default_file_stats with
         st_size=Int64.of_int stat.sz; 
         st_kind=kind2unix stat.kind; 
         st_atime=stat.meta.atim; 
         st_ctime=stat.meta.mtim;
         st_mtime=stat.meta.mtim;
        }
      | `Symlink -> 
        {default_file_stats with
         st_size=Int64.of_int stat.sz;
         st_kind=kind2unix stat.kind;
         st_atime=stat.meta.atim; 
         st_ctime=stat.meta.mtim;
         st_mtime=stat.meta.mtim;
        }
      | `Other -> 
        log_.log ("Unknown stat, `Other, at "^__LOC__);
        (* FIXME what to do here? *)
        {default_file_stats with
         st_kind=kind2unix stat.kind }


    let unix2stat stat = 
      match stat.st_kind with
      | S_DIR -> { sz=1; kind=`Dir; meta=unix2meta stat }
      | S_REG -> { sz=Int64.to_int stat.st_size; kind=`File; meta=unix2meta stat }
      | S_LNK -> { sz=Int64.to_int stat.st_size; kind=`Symlink; meta=unix2meta stat }
      | _ -> { sz=Int64.to_int stat.st_size; kind=`Other; meta=unix2meta stat }
  end


  let stat2unix = Default.stat2unix
  let unix2stat = Default.unix2stat


end
include Stat_record

module Base_extra = struct
  (* ensure 64 bit system *)
  let _ = assert(Sys.int_size = 63)


  let exit_1 = failwith  (* hopefully not be caught *)


  (* following for strings *)
  let dirname_basename path = 
    assert (String_.starts_with ~prefix:"/" path);
    String_.split_on_last ~sub:"/" path |> fun (p,c) -> 
    (* the semantics is that dirname is an absolute path *)
    (if p="" then "/" else p),c


  (* include Bigarray_buffer *)



  type length = int (* FIXME in following *)
  type offset = int


  (* FIXME replace path and dh with int-like and string-like *)
  include struct
    open Bin_prot.Std
    type path=string [@@deriving bin_io, yojson]
    type dh=int  (* FIXME why specialize here? *)
  end


  (* logging ---------------------------------------------------------- *)

  include Log_
end
let exit_1 = Base_extra.exit_1

module Finished = struct
  (* FIXME remove these? *)
  type is_finished = {is_finished:bool}
  let finished = {is_finished=true}
  let not x = { is_finished=not x.is_finished}
end
include Finished


module Msgs = struct
  (* open Error_ *)
  open Base_extra
  open Stat_record

  open Bin_prot.Std


  type length = int[@@deriving bin_io, yojson]
  type offset = int[@@deriving bin_io, yojson]


  (* for messages going to server, fd and dh are ints *)
  type dh = int [@@deriving bin_io, yojson]

  type fd = int [@@deriving bin_io, yojson]

  type data = string[@@deriving bin_io, yojson]

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
  type msg_from_server = 
    | Ok_ of msg_from_server' 
    | Error_ of Error_.exn_ [@@deriving bin_io, yojson]
  (* FIXME Error is probably a bad choice of constructor - prefer Exn_ *)



  let msg_s_to_string m = 
    m |> msg_from_server_to_yojson |> Yojson.Safe.pretty_to_string

  let string_to_msg_s s = 
    s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
    |> function
    | Ok x -> x
    | Error _e -> 
      exit_1 __LOC__        

  let msg_c_to_string m = 
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string

  let string_to_msg_c s = 
    s |> Yojson.Safe.from_string |> msg_from_client_of_yojson
    |> function
    | Ok x -> x
    | Error _e -> 
      exit_1 __LOC__ 


end



module Error_types = struct

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
  include struct
    open Unix

    (* Our version of the Unix_error exception; we want to pattern match
       exhaustively and name the type *)
    type unix_error_ = [`Unix_error of error * string * string ]

    let unknown_error = `Unix_error(EUNKNOWNERR 999,"FIXME","FIXME")
  end

end


module Int_base_types = struct
  type fd = int
  type dh = int

  let fd2i x = x
  let i2fd x = x
  let dh2i x = x
  let i2dh x = x
end



module Ops_type_ = struct
  (* open Tjr_monad.Monad *)
  (* open Base_ *)
  (* open R_ *)
  open Error_types
  open Bigarray_buffer
  open Stat_record

  (* sig -------------------------------------------------------------- *)

  type path = string

  module type OPS_TYPE = sig
    type ('fd,'dh,'w) ops = {
      root     : path;
      unlink   : path -> ((unit,unlink_err)r_,  'w) m;
      mkdir    : path -> ((unit,mkdir_err)r_,  'w) m;
      opendir  : path -> (('dh,opendir_err)r_,  'w) m;
      (* . and .. are returned *)
      readdir  : 'dh -> ((string list * is_finished,readdir_err)r_,  'w) m;
      closedir : 'dh -> ((unit,closedir_err)r_,  'w) m;
      create   : path -> ((unit,create_err)r_,  'w) m;
      open_    : path -> (('fd,open_err)r_,  'w) m;
      pread    : 
        fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
        ((int,pread_err)r_,  'w) m; 
      pwrite   : 
        fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
        ((int,pwrite_err)r_,  'w) m;
      close    : 'fd -> ((unit,close_err)r_,  'w) m;
      rename   : path -> path -> ((unit,rename_err)r_,  'w) m;
      truncate : path:path -> length:int -> ((unit,truncate_err)r_,  'w) m;
      stat     : path -> ((stat_record,stat_err)r_,  'w) m;
      symlink  : path -> path -> ((unit,symlink_err)r_, 'w) m;
      readlink : path -> ((string,readlink_err)r_,'w) m;
      reset    : unit -> (unit,  'w) m;
    }
  end


  (* FIXME duplication with sig *)
  (* don't use this module - just use the included type in this file *)
  module Ops_type_with_result' = struct
    type ('fd,'dh,'w) ops = {
      root     : path;
      unlink   : path -> ((unit,unlink_err)r_,  'w) m;
      mkdir    : path -> ((unit,mkdir_err)r_,  'w) m;
      opendir  : path -> (('dh,opendir_err)r_,  'w) m;
      (* . and .. are returned *)
      readdir  : 'dh -> ((string list * is_finished,readdir_err)r_,  'w) m;
      closedir : 'dh -> ((unit,closedir_err)r_,  'w) m;
      create   : path -> ((unit,create_err)r_,  'w) m;
      open_    : path -> (('fd,open_err)r_,  'w) m;
      pread    : 
        fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
        ((int,pread_err)r_,  'w) m; 
      pwrite   : 
        fd:'fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
        ((int,pwrite_err)r_,  'w) m;
      close    : 'fd -> ((unit,close_err)r_,  'w) m;
      rename   : path -> path -> ((unit,rename_err)r_,  'w) m;
      truncate : path:path -> length:int -> ((unit,truncate_err)r_,  'w) m;
      stat     : path -> ((stat_record,stat_err)r_,  'w) m;
      symlink  : path -> path -> ((unit,symlink_err)r_, 'w) m;
      readlink : path -> ((string,readlink_err)r_,'w) m;
      reset    : unit -> (unit,  'w) m;
    }
  end

  include Ops_type_with_result'

end


type buffer = Bigarray_buffer.buffer
