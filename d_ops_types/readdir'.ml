
(*
module Make_readdir'(I:Ops_types.IMP_OPS_TYPE) = struct
  open I
  (* for small directories *)
  let readdir' ~ops = 
    fun path ->
      let dh = ops.opendir path in
      let es = ref [] in
      let finished = ref false in
      while(not !finished) do
        let (es',f) = ops.readdir dh in
        es:=!es@es';
        finished:=f;
      done;
      ops.closedir dh;
      !es
end
*)

(* Read all directory entries at once; obviously not a good idea if
   there are a large number of entries. *)
module Make_readdir'(I:Ops_types.OPS_TYPE_WITH_RESULT) = struct
  open I
  (* for small directories *)
  let readdir' ~ops = 
    let ( >>= ) = bind in
    fun path ->
      ops.opendir path >>= function Error e -> return (Error e) | Ok dh ->
        let es = ref [] in
        let rec f () = 
          ops.readdir dh 
          >>= function Error e -> return (Error e) | Ok (es',finished) ->
            es:=!es@es';
            if finished then return (Ok !es) else f ()
        in
        f() >>= fun x -> 
        ops.closedir dh >>= fun _ -> 
        return x
end
