SHELL:=/bin/bash
BASH_ENV=bash_env.sh
export BASH_ENV

# shouldn't shell builtin source bash_env anyway?
libname:=$(shell source bash_env.sh && echo $${libname})#

# generate: 
# 	$$ocamlopt -linkpkg literate.mli literate.ml
# 	./a.out .

all: links
	$$ocamlc -c $$mls
	mk_cma
	$$ocamlopt -c $$mls
	mk_cmxa

#	$(MAKE) install
#	$(MAKE) -C ../bin  # FIXME probably we don't want to do this from here

run: all
	$(MAKE) -C ../bin run

links:
	mk_links

install:
	mk_meta
	-remove
	install

uninstall:
	remove

clean: FORCE
	clean
	$(MAKE) -C ../bin clean


real_clean: clean
	clean_links
	rm -f META
	rm -f *.html


# doc ------------------------------------------------------------------

# NOTE make these after making everything else 

# NOTE minor simplifications of types etc
recapitulation.mli.generated: FORCE
	$$ocamlc  -i recapitulation.ml | \
	  sed -e "s/Error_types[.]//g" \
	    -e "s/Base_.//g" \
	    -e "s/Unix_ops.MBR/Unix_MBR/g" \
	    -e "s/Ops_types.//g" >$@

recapitulation.mli.patched: recapitulation.mli.generated FORCE
	cp recapitulation.mli.generated recapitulation.mli.patched
	patch -u recapitulation.mli.patched < recapitulation.patch

doc:
	ocamlfind ocamldoc $$PKGS $$WARN -html -d ../ocamldoc *.ml

# temporary do not use ------------------------------------------------

prereqs:
	echo "Making minifs prereqs"
	$(MAKE) -C ../tjr_lib
	$(MAKE) -C ../tjr_net

FORCE:
