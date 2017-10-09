open Mini_pervasives

open Bin_prot.Std


type length = int[@@deriving bin_io, yojson] (* FIXME in following *)
type offset = int[@@deriving bin_io, yojson]

(* FIXME *)
(* type path = string [@@deriving bin_io, yojson] *)
type dh = int [@@deriving bin_io, yojson]
type fd = int [@@deriving bin_io, yojson]

type data = string[@@deriving bin_io, yojson]
type file_stat = int [@@deriving bin_io, yojson]

type st_kind = [`Dir | `File | `Symlink | `Other ] [@@deriving bin_io, yojson]

type msg_from_client = 
  | Unlink of path * string
  | Mkdir of path * string
  | Opendir of path 
  | Readdir of dh 
  | Closedir of dh
  | Create of path * string
  | Open of path 
  | Pread of fd * offset * int
  | Pwrite of fd * offset * data
  | Close of fd
  | Truncate of path * length
  | Stat_file of path
  | Kind of path 
  | Reset
[@@deriving bin_io, yojson]
               

type msg_from_server' = 
  | Unit
  | Int of int
  | Dh of dh
  | Readdir' of string list * bool
  | Open' of fd
  | Pread' of data
  | Stat_file' of file_stat
  | Kind' of st_kind [@@deriving bin_io, yojson]


(* or just use error in monad? *)
type msg_from_server = Msg of msg_from_server' | Error of string
