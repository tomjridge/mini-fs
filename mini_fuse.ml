(* fuse filesystem backed by minifs *)

open Unix
open LargeFile
open Bigarray
open Fuse

(* fuse ------------------------------------------------------------- *)

open Minifs

module type S = Minifs.S with type 'a m = 'a

module T = Minifs.Make(S)
open S
open T


let bind : ('a -> 'b m) -> 'a m -> 'b m = fun f x -> failwith ""

let return: 'a -> 'a m = fun x -> failwith ""

let default_stats = LargeFile.stat "."


let default_file_stats size = 
  { default_stats with 
    st_nlink = 1;
    st_kind = S_REG;
    st_perm = 0o640;
    st_size = size }


let mk_fuse_ops ~path_to_string ~readdirall ~(ops:ops) = 
  let get_attr path = 
    ops.kind path |> bind @@ function
    | `File -> 
      ops.stat_file path |> bind @@ fun x -> 
      x.sz |> Int64.of_int |> default_file_stats |> return
    | `Dir -> return default_stats
    | _ -> raise @@ Unix_error (ENOENT,"readdir",path|>path_to_string)
  in

  let readdir path _ = readdirall path in

  let fopen path flags = 
    ops.kind path |> bind @@ function
    | `File -> None
    | _ -> raise @@ Unix_error (ENOENT,"open",path|>path_to_string)
  in


(* assume all reads are block-aligned *)
let do_read path buf ofs _ = safely (fun () ->     
    let buf_size = Bigarray.Array1.dim buf in
    let wf = 
      path = "/"^Img.fn &&
      buf_size >= blk_sz &&  (* allow attempts to read more, but only read blk_sz *)
      Int64.rem ofs (Int64.of_int blk_sz) = Int64.zero       
    in 
    let msg () = 
      `List[`String "read"; `String path; `Int (Int64.to_int ofs);
            `Int buf_size]
      |> Yojson.Safe.to_string 
    in
    Test.test(fun () -> 
      msg () |> print_endline);
    (if buf_size > blk_sz then
       Test.warn (__LOC__^": buf_size > blk_sz"));
    let open Btree_api.Imperative_map_ops in
    match () with
    | _ when (not wf) -> (
        Test.warn (__LOC__^": not wf");
        raise (Unix_error (ENOENT,"read",path)))
    | _ -> (
        try
          (* we want to return a single block FIXME to begin with *)
          let i = ((Int64.to_int ofs) / blk_sz) in
          let blk = imap_ops.find i in
          let blk = 
            match blk with
            | None -> Blk.of_string ""
            | Some blk -> blk
          in
          let blk = Blk.to_string blk in
          for j = 0 to blk_sz -1 do  (* copy to buf FIXME use blit *)
            buf.{j} <- String.get blk j 
          done;
          blk_sz
        with e -> (
            print_endline "do_read:!"; 
            msg ()|>print_endline; 
            e|>Printexc.to_string|>print_endline;
            Printexc.get_backtrace() |> print_endline;
            ignore(exit 1);  (* exit rather than allow fs to continue *)
            raise (Unix_error (ENOENT,"read",path)))))

(* NOTE ofs is offset in file (pointer to by path); loopback writes
   may be non-block-aligned, and less than the block size *)
let do_write path (buf:('x,'y,'z)Bigarray.Array1.t) ofs _fd = safely (fun () -> 
    let buf_size = Bigarray.Array1.dim buf in
    let blk_id = ((Int64.to_int ofs) / blk_sz) in
    let offset_within_block = Int64.rem ofs (Int64.of_int blk_sz) |> Int64.to_int in
    let wf = 
      path = "/"^Img.fn &&
      buf_size > 0 &&  (* allow < blk_sz, but not 0 *)
      true (* offset_within_block = 0  FIXME may want to allow writing at an offset *)
    in 
    let msg () = 
      `List[`String "write"; `String path; `Int (Int64.to_int ofs); 
            `String "buf_size,blk_id,offset_within_block"; `Int buf_size; `Int blk_id; `Int offset_within_block]
      |> Yojson.Safe.to_string 
    in
    Test.test (fun () -> msg () |> print_endline);
    (if (offset_within_block + buf_size > blk_sz) then
       Test.warn (__LOC__^": offset_within_block + buf_size > blk_sz"));
    let open Btree_api.Imperative_map_ops in
    match () with
    | _ when (not wf) -> (
        Test.warn (__LOC__^": not wf");
        raise (Unix_error (ENOENT,"write",path)))
    | _ -> (
        try
          let blk = 
            match offset_within_block = 0 with
            | true -> Bytes.create blk_sz
            | false -> (
                (* have to read existing block *)
                imap_ops.find blk_id |> function None -> Bytes.create blk_sz | Some blk -> blk|>Blk.to_string)
          in
          let n = min (blk_sz - offset_within_block) buf_size in  (* allow writing < blk_sz *)
          for j = 0 to n -1 do
            Bytes.set blk (offset_within_block+j) buf.{j}  (* FIXME use blit *)
          done;
          blk |> Bytes.to_string |> Blk.of_string |> fun blk ->
          imap_ops.insert blk_id blk |> (fun () -> n)
        with e -> (
            print_endline "do_write:!"; 
            msg ()|>print_endline;
            e|>Printexc.to_string|>print_endline;
            Printexc.get_backtrace() |> print_endline;
            ignore(exit 1);
            raise (Unix_error (ENOENT,"write",path))) ))


(* following apparently required for a block device backed by a file on a fuse fs ? *)
(* let do_fsync path ds hnd = () *)

(* let do_getxattr s1 s2 = "?" *)


let _ =
  main Sys.argv 
    { default_operations with 
      getattr = do_getattr;
      readdir = do_readdir;
      fopen = do_fopen;
      read = do_read;
      write = do_write;
      truncate = (fun _ _ -> print_endline "truncate"; ());
      (*      fsync = do_fsync;
              getxattr = do_getxattr; *)
    }


(* opendir = (fun path flags -> 
   (Unix.openfile path flags 0 |> Unix.close);
   None); (* FIXME from fusexmp; not sure needed *) *)
