(* fuse in-mem filesystem ------------------------------------------- *)

open Tjr_minifs
open G_fuse_in_mem

let ops = F_in_mem.logged_ops
    
let () =
  Fuse.main Sys.argv Mini_fuse.in_mem_fuse_ops
