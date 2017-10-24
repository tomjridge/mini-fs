(* fuse in-mem filesystem ------------------------------------------- *)

open Tjr_minifs
open G_fuse_in_mem

let ref_ = ref E_in_mem.init_t

let () = Fuse.main Sys.argv (fuse_ops ~ref_)
