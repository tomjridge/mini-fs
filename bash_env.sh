set -a # export all vars
# set -x # debug

libname=tjr_minifs
Libname=Tjr_minifs
#src_subdirs=`echo {ac,ag,c,d,e,f,h,i,j,n}_*`
#mls_in_subdirs=`ls {ac,ag,c,d,e,f,h,i,j,n}_*/*.ml`
meta_description="Minimal file-system-like thing"

required_packages="extunix,extlib,Fuse,tjr_lib,core,ppx_bin_prot,ppx_deriving_yojson,lwt,lwt.unix,tjr_net"

natives=""
bytes=""

source bash_env.common


# clean generated 
function clean_links() {
    find . -maxdepth 1 -type l -exec rm -f \{\} \;
    rm -f links
}



function mk_links() {
#    ln -s {a_,d_,e_,f_,g_,h_,m_}*/*.ml .
    ln -s {a_,d_,e_,f_}*/*.ml .
#    ln -s {a_,d_,e_}*/*.ml .
#    ln -s {a_,d_}*/*.ml .
    touch links
}


#mls=`test -f links && (ls *.ml |sort|tr '\n' ' ')`

#echo "mls are: $mls"
