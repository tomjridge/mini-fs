set -a # export all vars
# set -x # debug

libname=tjr_minifs
Libname=Tjr_minifs
meta_description="Minimal file-system-like thing"

# NOTE Fuse is the ocamlfind package, but opam needs ocamlfuse
# FIXME need to ensure that META is correct
required_packages="extunix,extlib,Fuse,tjr_lib,core,ppx_bin_prot,ppx_deriving_yojson,lwt,lwt.unix,tjr_net,tjr_path_resolution"

natives=""
bytes=""

# clean generated 
function clean_links() {
    find . -maxdepth 1 -type l -exec rm -f \{\} \;
    rm -f links
}



function mk_links() {
    ln -s {a_,d_,e_,f_,g_}*/*.ml .

#    ln -s {a_,d_,e_,f_,g_}*/*.ml .

#    ln -s {a_,d_,e_,f_,g_,h_,m_,o_}*/*.ml .
#    ln -s {a_,d_,e_,f_,g_}*/*.ml .
#    ln -s {a_,d_,e_}*/*.ml .
#    ln -s {a_,d_}*/*.ml .
    touch links
}


#mls=`test -f links && (ls *.ml |sort|tr '\n' ' ')`

#echo "mls are: $mls"














# generic --------------------------------------------------------------


# set -x

# set these env vars before including the file
function check_env_vars () {
    # http://stackoverflow.com/questions/31164284/shell-script-exiting-script-if-variable-is-null-or-empty
    : ${libname?Need a value}
    : ${Libname?Need a value}
#    : ${src_subdirs?Need a value}
#    : ${mls_in_subdirs?Need a value}
    : ${meta_description?Need a value}
    : ${required_packages?Need a value}
}
check_env_vars

PKGS="-package $required_packages"


# root=$(realpath $(dirname $BASH_SOURCE))/../..
# 
#  # if using nix, this may not be present
# test -f $root/config.sh && source $root/config.sh


SYNTAX="" # "-syntax camlp4o" # simplify: use for every file
FLGS="-g -thread -bin-annot" 
INLINE="-inline 0" # inline 0 for debugging native
FOR_PACK="-for-pack $Libname"

    # 8~"pattern-matching is not exhaustive"; 
    # 11~"this match case is unused";
    # 26~"unused variable s2"
    # 40~It is not visible in the current scope, and will not be selected if the type becomes unknown.
WARN="-w @f@p@u@s@40-8-11-26-40"

    # these include syntax, so should work on all files; may be
    # overridden in ocamlc.sh; FIXME don't need -bin-annot twice
  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -bin-annot         $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -bin-annot $INLINE $FLGS $FOR_PACK $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"


# mls ----------------------------------------

#mls="step_monad.ml bigarray_buffer.ml mini_pervasives.ml minifs.ml msgs.ml mini_nfs.ml mini_log.ml mini_in_mem.ml mini_unix.ml mini_fuse.ml"
# mls="tjr_monad.ml minifs.ml mini_unix.ml"
mls=$(ocamlfind ocamldep -sort -one-line *.ml)
echo "mls are: $mls"

cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"


# cma,cmxa -------------------------------------------------------------

function mk_cma() {
         # NOTE -bin-annot
	$DISABLE_BYTE ocamlfind ocamlc -bin-annot -pack -o $libname.cmo $cmos
  $DISABLE_BYTE ocamlfind ocamlc -g -a -o $libname.cma $libname.cmo
}

function mk_cmxa() {
	$DISABLE_NTVE ocamlfind ocamlopt -pack -o $libname.cmx $cmxs
  $DISABLE_NTVE ocamlfind ocamlopt -g -a -o $libname.cmxa $libname.cmx
}


# # depend ----------------------------------------
# 
# function mk_depend() {
#     mkdir -p _depend
#     for f in ${src_subdirs}; do
#         (cd $f && ocamldep -one-line -sort *.ml > ../_depend/$f)
#     done
#     touch _depend/xxx
# }
# 
# function mk_depend() {
#     mkdir -p _depend
#     for f in ${src_subdirs}; do
#         (cd $f && ocamldep -one-line -sort *.ml > ../_depend/$f)
#     done
#     touch _depend/xxx
# }



# # links ----------------------------------------
# 
# function init() {
#     link_files="${mls_in_subdirs}"
# }
# 
# function mk_links() {
#     echo "mk_links..."
#     init
#     ln -s $link_files .
# }
# 
# 
# function rm_links() {
#     echo "rm_links..."
#     init
#     for f in $link_files; do rm -f `basename $f`; done
# }
# 

# # mlis ----------------------------------------
# 
# function mk_mlis() {
#     echo "mk_mlis..."
#     for f in $mls_in_subdirs; do $ocamlc -i $f > tmp/${f/.ml/.mli}; done
# }



# meta ----------------------------------------

function mk_meta() {
local gv=`git rev-parse HEAD`
local d=`date`
cat >META <<EOF
name="$libname"
description="$meta_description"
version="$d $gv"
requires="$required_packages"  # opam ocamlfuse is ocamlfind Fuse
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF

}


# codetags ----------------------------------------

function mk_codetags() {
    init # link_files
    for f in XXX TODO FIXME NOTE QQQ; do     # order of severity
        grep --line-number $f $link_files || true; 
    done
}


# doc ----------------------------------------------------

function mk_doc() {
    ocamlfind ocamldoc $PKGS $WARN -html -intro n_doc/intro.odoc `cat _depend/*`
}


# clean ----------------------------------------------------------------

function clean() {
	rm -f *.{cmi,cmo,cmx,o,cmt,cmti} a.out *.cma *.cmxa *.a *.byte *.native
}

# ocamlfind install, remove, reinstall --------------------

function install() {
    # assumes packing
	  ocamlfind install $libname META $libname.{cmi,cma,cmxa,a} *.cmi *.cmt *.ml
    # FIXME how to install cmt file for tjr_btree?
}

function remove() {
    ocamlfind remove $libname
}
