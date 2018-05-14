open Error_
open Base_

open Bin_prot.Std


type length = int[@@deriving bin_io, yojson]
type offset = int[@@deriving bin_io, yojson]


(* for messages going to server, fd and dh are ints *)
type dh = int [@@deriving bin_io, yojson]

type fd = int [@@deriving bin_io, yojson]

type data = string[@@deriving bin_io, yojson]

type stat_record = Base_.stat_record 

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
  | Kind' of st_kind [@@deriving bin_io, yojson]


(* or just use error in monad? *)
type msg_from_server = 
  | Ok_ of msg_from_server' 
  | Error_ of exn_ [@@deriving bin_io, yojson]
(* FIXME Error is probably a bad choice of constructor - prefer Exn_ *)



let msg_s_to_string m = 
  m |> msg_from_server_to_yojson |> Yojson.Safe.pretty_to_string

let string_to_msg_s s = 
  s |> Yojson.Safe.from_string |> msg_from_server_of_yojson
  |> function
  | Ok x -> x
  | Error e -> 
    exit_1 __LOC__        

let msg_c_to_string m = 
  m |> msg_from_client_to_yojson |> Yojson.Safe.pretty_to_string

let string_to_msg_c s = 
  s |> Yojson.Safe.from_string |> msg_from_client_of_yojson
  |> function
  | Ok x -> x
  | Error e -> 
    exit_1 __LOC__ 

