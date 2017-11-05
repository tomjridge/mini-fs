open Bin_prot.Std

type exn_ = [ 
    | `Error_no_entry of string 
    | `Error_not_directory
    | `Error_not_file
    | `Error_attempt_to_rename_dir_over_file
    | `Error_attempt_to_rename_root
    | `Error_attempt_to_rename_to_subdir
    | `Error_no_src_entry
    | `Error_path_resolution
    | `Error_other
] [@@deriving bin_io, yojson]

let exn__to_string e = 
  e |> exn__to_yojson |> Yojson.Safe.pretty_to_string

include struct
  open Unix
  let mk_unix_exn (e:exn_) = e |> function
    | `Error_no_entry _ -> Unix_error(ENOENT, "154","")
    | `Error_not_directory -> Unix_error(ENOTDIR, "155","")
    | `Error_not_file -> Unix_error(EINVAL, "156","") (* FIXME *)
    | `Error_attempt_to_rename_dir_over_file -> Unix_error(EINVAL, "157","") (* FIXME *)
    | `Error_attempt_to_rename_root -> Unix_error(EINVAL, "158","") (* FIXME *)
    | `Error_attempt_to_rename_to_subdir -> Unix_error(EINVAL, "159","") (* FIXME *)
    | `Error_no_src_entry -> Unix_error(ENOENT, "160","")
    | `Error_path_resolution -> Unix_error(EUNKNOWNERR 999,"162","") (* FIXME *)
    | `Error_other -> Unix_error(EUNKNOWNERR 999,"161","")
end

