(* test; find all files below a given directory *)
open Minifs

let ( / ) dir n = dir^"/"^n

let find ~kind ~readdir =
  let rec f ~dir =
    readdir dir |> fun es ->
    es |> List.iter (function
        | "." -> ()
        | ".." -> ()
        | e when kind (dir/e) = `File -> print_endline (dir/e)
        | e when kind (dir/e) = `Dir -> f ~dir:(dir/e)
        | e -> Printf.printf "%s: unknown file type for %s/%s\n" __LOC__ dir e)
  in
  f 


(* for unix *)

open Mini_unix
open Extra_ops_
let unix_find = find ~kind ~readdir
  
  
