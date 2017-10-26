open E_in_mem

let mk_unix_exn = A_error.mk_unix_exn

include struct
  open Unix
  open Imp_ops_type
  (* similar to E_in_mem.imp_run, but translate errors *)
  let run ref_ : run = {
    run=(fun x -> E_in_mem.run (!ref_) x |> function
      | `Exn_ (e,w) -> (
          "Run resulted in exceptional state" |> fun s ->
          print_endline s;
          match w.internal_error_state with 
          | None -> (
              match w.thread_error_state with
              | None -> 
                "impossible, gfuse.161" |> fun s ->
                print_endline s;
                raise @@ Unix_error(EUNKNOWNERR 99, s, s)
              | Some e ->
                "gfuse.165, thread error: "^(A_error.exn__to_string e) |> fun s ->
                print_endline s;
                raise @@ mk_unix_exn e)
          | Some s ->
            "thread error gfuse.170: "^s) |> fun s ->
          print_endline s;
          raise @@ Unix_error(EUNKNOWNERR 99, s, s)
      | `Finished(a,w) -> 
        ref_:=w;
        a)
  }
end


module Fuse' = G_fuse_common.Make_fuse(struct
    include Monad
    include Mem_base_types
    include Imp_ops_type
  end)

include Fuse'

let fuse_ops ~ref_ = 
  mk_fuse_ops (mk_imperative_ops ~ref_ ~ops:logged_ops)

let _ : ref_:t ref -> Fuse.operations = fuse_ops
