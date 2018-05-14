open Log_
open Bin_prot.Std

type st_kind = [`Dir | `File | `Symlink | `Other ] [@@deriving bin_io,yojson]


type meta = {
  atim:float; 

  ctim:unit;  
  (* by default, we don't use this, but return ctim as mtim since atim
     apparently doesn't affect atim*)

  mtim:float;
} [@@deriving bin_io,yojson]


include struct
  open Unix.LargeFile
  let unix2meta st = {
    atim=st.st_atime;
    ctim=();
    mtim=st.st_mtime;
  }
end
           


type stat_record = { sz:int; kind:st_kind; meta:meta } 
[@@deriving bin_io,yojson]



include struct
  open Unix
  let unix2kind = function
    | S_DIR -> (`Dir:st_kind)
    | S_REG -> (`File:st_kind)
    | S_LNK -> (`Symlink)
    | _ -> `Other

  let kind2unix = function
    | `Dir -> S_DIR
    | `File -> S_REG
    | `Symlink -> S_LNK
    | `Other -> S_BLK  (* FIXME *)
end


module Default = struct
  open Unix
  open LargeFile
  let default_file_stats = 
    (* ASSUMES this file is present *)
    LargeFile.stat "tmp.txt"  

  let default_file_stats = 
    { default_file_stats with 
      st_nlink = 1;
      st_kind=Unix.S_REG;
      st_perm = 0o640;
    }

  let default_dir_stats = LargeFile.stat "."

  let stat2unix stat = 
    match stat.kind with
    | `Dir -> 
      {default_dir_stats with 
       st_size=Int64.of_int stat.sz;  (* FIXME use int64 *)
       st_kind=kind2unix stat.kind; 
       st_atime=stat.meta.atim; 
       st_ctime=stat.meta.mtim;
       st_mtime=stat.meta.mtim;
      }
    | `File -> 
      {default_file_stats with
       st_size=Int64.of_int stat.sz; 
       st_kind=kind2unix stat.kind; 
       st_atime=stat.meta.atim; 
       st_ctime=stat.meta.mtim;
       st_mtime=stat.meta.mtim;
      }
    | `Symlink -> 
      {default_file_stats with
       st_size=Int64.of_int stat.sz;
       st_kind=kind2unix stat.kind;
       st_atime=stat.meta.atim; 
       st_ctime=stat.meta.mtim;
       st_mtime=stat.meta.mtim;
      }
    | `Other -> 
      log_.log ("Unknown stat, `Other, at "^__LOC__);
      (* FIXME what to do here? *)
      {default_file_stats with
       st_kind=kind2unix stat.kind }
       

  let unix2stat stat = 
    match stat.st_kind with
    | S_DIR -> { sz=1; kind=`Dir; meta=unix2meta stat }
    | S_REG -> { sz=Int64.to_int stat.st_size; kind=`File; meta=unix2meta stat }
    | S_LNK -> { sz=Int64.to_int stat.st_size; kind=`Symlink; meta=unix2meta stat }
    | _ -> { sz=Int64.to_int stat.st_size; kind=`Other; meta=unix2meta stat }
end


let stat2unix = Default.stat2unix
let unix2stat = Default.unix2stat

