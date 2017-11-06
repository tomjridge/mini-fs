open Error_
open Base_

open Bin_prot.Std


type length = int[@@deriving bin_io, yojson] (* FIXME in following *)
type offset = int[@@deriving bin_io, yojson]

(* FIXME *)
(* type path = string [@@deriving bin_io, yojson] *)
type dh = int [@@deriving bin_io, yojson]

(* for messages going to server, fd is int *)
type fd = int [@@deriving bin_io, yojson]
type data = string[@@deriving bin_io, yojson]
type file_stat = Base_.file_stat = { sz:int } [@@deriving bin_io, yojson]

type st_kind = (* C_base.st_kind FIXME = *)
  [`Dir | `File | `Symlink | `Other ] [@@deriving bin_io, yojson]


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
  | Rename of path * string * path * string
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
type msg_from_server = 
  | Ok_ of msg_from_server' 
  | Error_ of exn_ [@@deriving bin_io, yojson]
(* FIXME Error is probably a bad choice of constructor - prefer Exn_ *)


include struct
  let msg_to_string m = 
    m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string

  let string_to_msg s = 
    s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
    |> function
    | Ok x -> x
    | Error e -> 
      exit_1 __LOC__        
end
