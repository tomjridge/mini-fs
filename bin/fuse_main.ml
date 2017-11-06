(* fuse in-mem filesystem ------------------------------------------- *)

open Tjr_minifs

let () = Fuse.main Sys.argv Fuse_in_mem.fuse_ops
