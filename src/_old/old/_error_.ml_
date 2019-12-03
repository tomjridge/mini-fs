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
