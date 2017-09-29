#require "tjr_minifs";;

open Tjr_minifs;;

#use "test.ml";;

open Minifs
open Mini_unix

let _ = 
  dest_imperative_ops unix_imperative_ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset ->
  let dh = opendir "." in
  let _ = readdir dh in
  let _ = readdir' "." |> List.map print_endline in
  let _ = readdir' "asdasds" |> List.map print_endline in
  ()

