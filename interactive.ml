#require "Fuse";;


(*

# #require "Fuse";;
/home/tr61/.opam/4.04.2/lib/ocaml/bigarray.cma: loaded
/home/tr61/.opam/4.04.2/lib/camlidl: added to search path
/home/tr61/.opam/4.04.2/lib/Fuse: added to search path
/home/tr61/.opam/4.04.2/lib/Fuse/Fuse.cma: loaded
Cannot load required shared library dllFuse_stubs.
Reason: /home/tr61/.opam/4.04.2/lib/stublibs/dllFuse_stubs.so: /home/tr61/.opam/4.04.2/lib/stublibs/dllFuse_stubs.so: undefined symbol: camlidl_malloc_string.

*)

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



(* in-mem testing --------------------------------------------------- *)

(* avoid fuse in top-level *)

#require "tjr_lib";;

(* extunix,extlib,Fuse,tjr_lib,core,ppx_bin_prot *)

#mod_use "minifs.ml";;
#mod_use "in_mem.ml";;

open In_mem;;

let x = Minifs.dest_imperative_ops imperative_ops @@ fun ~root ~unlink ~mkdir ~opendir ~readdir ~closedir ~create ~open_ ~pread ~pwrite ~close ~truncate ~stat_file ~kind ~reset -> (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset)

let (root,unlink,mkdir,opendir,readdir,closedir,create,open_,pread,pwrite,close,truncate,stat_file,kind,reset) = x

let x = Minifs.readdir' ~ops:imperative_ops root  (* FIXME doesn't work because resolves as the empty name *)

let _ = mkdir ~parent:"/" ~name:"tmp"

let _ = Minifs.readdir' ~ops:imperative_ops "/"

let _ = Minifs.readdir' ~ops:imperative_ops "/tmp"

let _ = (!ref_).dirs |> Map_did.bindings;;

let dir = (!ref_).dirs |> Map_did.bindings |> fun [(_,dir)] -> dir

let _ = Map_string.bindings dir

let _ = (!ref_).files |> Map_fid.bindings;;


(*

# let _ = Minifs.readdir' ~ops:imperative_ops "/tmp";;
Exception: Not_found.
Raised at file "map.ml", line 122, characters 16-25
Called from file "in_mem.ml", line 231, characters 4-25
Called from file "minifs.ml", line 162, characters 19-22
Called from file "minifs.ml", line 144, characters 19-23
Called from file "minifs.ml", line 144, characters 19-23
Called from file "minifs.ml", line 144, characters 19-23
Called from file "minifs.ml", line 177, characters 16-32
Called from file "minifs.ml", line 270, characters 13-25
Called from file "toplevel/toploop.ml", line 180, characters 17-56

*)

let _ = mkdir ~parent:"/tmp" ~name:"tmp2"

let x = Minifs.readdir' ~ops:imperative_ops "/tmp/tmp2"

let _ = String.split_on_char '/' "/"   (* ["";""] *)


let _ = String.split_on_char '/' "/tmp"   (* ["";"tmp"] *)


let _ = String.split_on_char '/' "/tmp/"   (* ["";"tmp";""] *)

let dh = opendir "/"
