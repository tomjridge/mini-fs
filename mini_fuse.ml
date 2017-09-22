(* fuse filesystem backed by minifs *)

open Unix
open LargeFile
open Bigarray
open Fuse
open Tjr_btree
open Btree_api
open Page_ref_int
open Params
open Block

module Blk=Blk4096
open Blk

let default_stats = LargeFile.stat "."

let safely f = 
  try f () with e -> Printexc.to_string e |> print_endline; raise e


(* image file that appears in fuse ---------------------------------- *)

(* aka curr *)

module Img = struct  

  (* appears as a single file in fuse mounted dir *)
  let fn = "image"  

  let _MB = Int64.((of_int 1024) |> mul (of_int 1024))

  (* size of curr as it appears in fuse mounted dir; for 4096 blk_sz,
     needs to be >10MB *)
  let size = Int64.(mul (of_int 1000) _MB)

end


(* global state ----------------------------------------------------- *)

type k = Map_blkid_blk.k
type v = Map_blkid_blk.v

open Cache

(* like map_on_fd.global_state but with cache *)
type t = {
  fd: Disk_on_fd.fd;
  free:page_ref;
  root:page_ref;
  cache: (k,v) cache_state
}
type snapdev=t

module Ops = struct
  open Monad
  let fd_ops = {
    get=(fun () -> fun t -> (t,Ok t.fd));
    set=(fun fd -> failwith "Don't use!");
  }

  let free_ops = {
    get=(fun () -> fun t -> (t,Ok t.free));
    set=(fun free -> fun t -> ({t with free}, Ok ()));
  }
  
  let page_ref_ops = {
    get=(fun () -> fun t -> (t,Ok t.root));
    set=(fun root -> fun t -> ({t with root},Ok ()));
  }

end

open Ops
(* open Map_on_fd.Default_implementation *)


(* data ------------------------------------------------------------- *)

(* stores all versions of data blocks and id->id map, using underlying
   fd *)

module Data = struct

  (* where data is stored in the base filesystem; grows without limit! *)
  let fn = "./data_blocks"

  let fd = safely (
      fun () -> Unix.openfile fn [O_CREAT;O_RDWR] 0o640)

  let disk_ops : snapdev Btree_api.disk_ops = Disk_on_fd.make_disk blk_sz fd_ops

end



(* map blkid->blk ---------------------------------------- *)

(* map from img.blkid to block; in order to avoid two syncs, we store
   the map data with the actual data blocks *)

module Map_ = struct

  open Monad

  let write_blk = (fun blk ->
    free_ops.get () |> bind (fun free ->
      Data.disk_ops.write free blk |> bind (fun () ->
        free_ops.set (free+1) |> bind (fun () -> 
          return free))))

  let read_blk = (fun i ->
    Data.disk_ops.read i |> bind (fun blk -> return (Some blk))) 


  (* implement blkid->blkid *)
  let ps = Int_int_map_on_fd.ps

  open Data

  let ii_store_ops = Disk_to_store.disk_to_store ~ps ~disk_ops ~free_ops

  let ii_map_ops : ('k,'v,'t) map_ops = 
    Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops:ii_store_ops

  let map_ops : (k,v,snapdev) map_ops = 
    Map_blkid_blk.mk_blk_id_blk_map ~write_blk ~read_blk ~map_ops:ii_map_ops

end

(* cache ------------------------------------------------------------ *)

module Cache_ = struct

  open Cache
  open Monad

  let cache_ops : (k,v,snapdev) cache_ops = {
    get=(fun () -> fun t -> (t,Ok t.cache));
    set=(fun cache -> fun t -> ({t with cache},Ok ()));
  }

  open Map_

  let (map_ops,evict_hook) = make_cached_map ~map_ops ~cache_ops 
      ~kk:(fun ~cached_map_ops ~evict_hook -> (cached_map_ops,evict_hook))

  let _ = evict_hook := fun () -> Test.warn "Cache evicted!"

  let init_cache = 
    mk_initial_cache Block.compare_blk_id 
    |> fun x -> {x with max_size=100 * 1000 (* blocks *); evict_count=1000 }

end

let init_state = ref (
    let fd = Data.fd in
    Frame.Leaf_frame [] |> fun frm -> 
    let p = frame_to_page Map_.ps blk_sz frm in
    Disk_on_fd.write ~fd ~blk_sz ~blk_id:0 ~blk:p;
    { fd; free=1; root=0; cache=Cache_.init_cache })

let imap_ops = Imperative_map_ops.of_map_ops Cache_.map_ops init_state


(* disable testing *)
let _ = Test.disable ()


(* fuse ------------------------------------------------------------- *)

let do_getattr path = safely (fun () ->
    if path = "/" then default_stats
    else if path = "/" ^ Img.fn then 
      { default_stats with 
        st_nlink = 1;
        st_kind = S_REG;
        st_perm = 0o640;
        st_size = Img.size }
    else raise (Unix_error (ENOENT,"stat",path)))

let do_readdir path _ = safely (fun () -> 
    if path = "/" then [".";"..";Img.fn]
    else raise (Unix_error (ENOENT,"readdir",path)))

let do_fopen path flags = safely (fun () -> 
    if path = "/"^Img.fn then None
    else raise (Unix_error (ENOENT,"open",path)))

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
