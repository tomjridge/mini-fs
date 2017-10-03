open Tjr_minifs
open Minifs
    
let () =
  Fuse.main Sys.argv Mini_fuse.in_mem_fuse_ops
