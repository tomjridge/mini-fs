(* test; find all files below a given directory *)
(*
open Minifs

let ( / ) dir n = dir^"/"^n

(* readdir should return all entries *)
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
let ops = unix_imperative_ops

let unix_find = 
  dest_imperative_ops unix_imperative_ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
  find ~kind ~readdir:readdir'
  
  
*)
