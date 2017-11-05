open C_base

(* operations ------------------------------------------------------- *)


(* FIXME the following should be refined *)
type err_ = [ `EOTHER ]
type unlink_err = err_
type mkdir_err = err_
type opendir_err = err_
type readdir_err = err_
type closedir_err = err_
type create_err = err_
type open_err = err_
type pread_err = err_
type pwrite_err = err_
type close_err = err_ (* EBADF, but for valid fd, fd will be closed *)
type rename_err = err_
type truncate_err = err_
type stat_file_err = err_
type kind_err = err_


module type R = sig
  type ('a,'e)r_  (* result *)
end


(* NOTE R is expected to be result *)
module type OPS_TYPE = sig
  include MONAD
  include BASE_TYPES
  include R
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)r_ m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)r_ m;
    opendir : path -> (dh,opendir_err)r_ m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)r_ m;
    closedir : dh -> (unit,closedir_err)r_ m;
    create : parent:path -> name:string -> (unit,create_err)r_ m;
    open_ : path -> (fd,open_err)r_ m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)r_ m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)r_ m;
    close : fd -> (unit,close_err)r_ m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)r_ m;
    truncate : path:path -> length:int -> (unit,truncate_err)r_ m;
    stat_file : path -> (file_stat,stat_file_err)r_ m;
    kind : path -> (st_kind,kind_err)r_ m;
    reset : unit -> unit m;
  }
end


module type MB = sig
  include MONAD
  include BASE_TYPES
end

module type MBR = sig
  include MB
  include R
end


module Make_ops_type(MBR:MBR) = struct
  open MBR
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)r_ m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)r_ m;
    opendir : path -> (dh,opendir_err)r_ m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)r_ m;
    closedir : dh -> (unit,closedir_err)r_ m;
    create : parent:path -> name:string -> (unit,create_err)r_ m;
    open_ : path -> (fd,open_err)r_ m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)r_ m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)r_ m;
    close : fd -> (unit,close_err)r_ m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)r_ m;
    truncate : path:path -> length:int -> (unit,truncate_err)r_ m;
    stat_file : path -> (file_stat,stat_file_err)r_ m;
    kind : path -> (st_kind,kind_err)r_ m;
    reset : unit -> unit m;
  }
end


module Make_ops_type_with_result(MB:MB) = struct
  open MB
  type ('a,'e)r_ = ('a,'e)result
  type ops = {
    root: path;
    unlink : parent:path -> name:string -> (unit,unlink_err)r_ m;
    mkdir : parent:path -> name:string -> (unit,mkdir_err)r_ m;
    opendir : path -> (dh,opendir_err)r_ m;
    (* . and .. are returned *)
    readdir : dh -> (string list * is_finished,readdir_err)r_ m;
    closedir : dh -> (unit,closedir_err)r_ m;
    create : parent:path -> name:string -> (unit,create_err)r_ m;
    open_ : path -> (fd,open_err)r_ m;
    pread: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pread_err)r_ m; 
    pwrite: 
      fd:fd -> foff:int -> length:int -> buffer:buffer -> boff:int -> 
      (int,pwrite_err)r_ m;
    close : fd -> (unit,close_err)r_ m;
    rename: 
      spath:path -> sname:string -> dpath:path -> dname:string -> 
      (unit,rename_err)r_ m;
    truncate : path:path -> length:int -> (unit,truncate_err)r_ m;
    stat_file : path -> (file_stat,stat_file_err)r_ m;
    kind : path -> (st_kind,kind_err)r_ m;
    reset : unit -> unit m;
  }

end



(* imperative operations -------------------------------------------- *)

(* NOTE we change these to include the error value *)

module Types_without_monad = struct
  type 'a m' = 'a
  let bind: 'a -> ('a -> 'b) -> 'b = fun a b -> a |> b
  let return: 'a -> 'a = fun a -> a
  module type OPS_TYPE = OPS_TYPE with type 'a m := 'a m'
end


module Types_without_result = struct
  open Types_without_monad
  type ('a,'e) r' = 'a
  module type OPS_TYPE = OPS_TYPE with type ('a,'e)r_ := ('a,'e)r'
end


module Make_readdir'(I:Types_without_result.OPS_TYPE) = struct
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


