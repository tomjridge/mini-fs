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


# clean generated 
function clean_hook() {
    rm -f msg_{lwt,unix,abstract}.ml
}



function link() {
    ln -s generated/*.ml .
}


mls=`ls *.ml |sort|tr '\n' ' '`
