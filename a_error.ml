open Bin_prot.Std

type exn_ = [ 
    | `Error_no_entry of string 
    | `Error_not_directory
    | `Error_not_file
    | `Error_attempt_to_rename_dir_over_file
    | `Error_attempt_to_rename_root
    | `Error_attempt_to_rename_to_subdir
    | `Error_no_src_entry
] [@@deriving bin_io, yojson]

let exn__to_string e = 
  e |> exn__to_yojson |> Yojson.Safe.pretty_to_string
