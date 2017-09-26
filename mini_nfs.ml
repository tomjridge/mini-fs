(* mini network filesystem ------------------------------------------ *)
open Tjr_monad
open Minifs

(* messages *)

type msg_buffer = string 

(* type ('a,'t) m = msg_buffer  (* call message, expecting response of type 'a *) *)



open Bin_prot.Std

type length = int[@@deriving bin_io] (* FIXME in following *)
type offset = int[@@deriving bin_io]

(* FIXME *)
type path = Path of string [@@deriving bin_io]
type dh = Dh of int [@@deriving bin_io]
type fd = Fd of int [@@deriving bin_io]

type data = string[@@deriving bin_io]
type file_stat = int [@@deriving bin_io]

type st_kind = [`Dir | `File | `Symlink | `Other ] [@@deriving bin_io]

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
[@@deriving bin_io]
               

type msg_from_server' = 
  | Unit
  | Int of int
  | Dh of dh
  | Readdir' of string list * bool
  | Open' of fd
  | Pread' of data
  | Stat_file' of file_stat
  | Kind' of st_kind [@@deriving bin_io]


(* or just use error in monad? *)
type msg_from_server = Msg of msg_from_server' | Error of string

type 'a m = ('a,string) Tjr_monad.m

(* construct message, send, recv *)
let mk_msg_ops (type t) 
    ~(call:msg_from_client -> msg_from_server' m)
    ~data_length
    ~data_of_buffer
    ~blit_data_to_buffer
  = 
  let root : path = failwith "FIXME" in
  let ret_unit : msg_from_server' -> unit m = function
    | Unit -> return ()
    | _ -> err __LOC__
  in
  let unlink ~parent ~name = Unlink(parent,name) |> call >>= ret_unit in
  let mkdir ~parent ~name = Mkdir(parent,name) |> call >>= ret_unit in
  let opendir p = Opendir(p) |> call >>=function
    | Dh dh -> return dh
    | _ -> err __LOC__
  in
  let readdir dh = Readdir(dh) |> call >>= function
    | Readdir' (xs,b) -> return (xs,b)
    | _ -> err __LOC__
  in
  let closedir dh = Closedir(dh) |> call >>= ret_unit in
  let create ~parent ~name = Create(parent,name) |> call >>= ret_unit in
  let open_ p = Open(p) |> call >>= function
    | Open' fd -> return fd
    | _ -> err __LOC__
  in
  let pread ~fd ~foff ~length ~buffer ~boff =
    Pread(fd,foff,length) |> call >>= function
    | Pread' data -> 
      blit_data_to_buffer data buffer boff;
      return (data_length data)
    | _ -> err __LOC__
  in
  let pwrite ~fd ~foff ~length ~buffer ~boff =
    data_of_buffer buffer boff length |> fun data ->
    Pwrite(fd,foff,data) |> call >>= function
    | Int nwritten -> return nwritten
    | _ -> err __LOC__
  in
  let close fd = Close fd |> call >>= ret_unit in
  let truncate ~path ~length = Truncate(path,length) |> call >>= ret_unit in
  let stat_file p = Stat_file p |> call >>= function
    | Stat_file' sz -> return {sz}
    | _ -> err __LOC__
  in
  let kind p = Kind p |> call >>= function
    | Kind' k -> return k
    | _ -> err __LOC__
  in
  let reset () = Reset |> call >>= ret_unit in

  assert(
    Minifs.wf_ops 
    ~root ~unlink ~mkdir ~opendir ~readdir ~closedir 
    ~create ~open_ ~pread ~pwrite ~close ~truncate 
    ~stat_file ~kind ~reset);
  ()

