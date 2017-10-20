set -a # export all vars
# set -x # debug

libname=tjr_minifs
Libname=Tjr_minifs
#src_subdirs=`echo {ac,ag,c,d,e,f,h,i,j,n}_*`
#mls_in_subdirs=`ls {ac,ag,c,d,e,f,h,i,j,n}_*/*.ml`
meta_description="Minimal file-system-like thing"

required_packages="extunix,extlib,Fuse,tjr_lib,core,ppx_bin_prot,ppx_deriving_yojson"

natives=""
bytes=""

source bash_env.common

mls="bigarray_buffer.ml mini_error.ml minifs.ml mini_pervasives.ml msgs.ml step_monad.ml test.ml mini_log.ml mini_in_mem.ml mini_fuse.ml  mini_unix.ml mini_nfs.ml "


