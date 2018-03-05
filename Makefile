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
	$(MAKE) install
	$(MAKE) -C bin

run: all
	$(MAKE) -C bin run

links:
	mk_links

install:
	mk_meta
	-remove
	install


clean:
	clean
	clean_links
	rm -f *.html
	$(MAKE) -C bin clean


# NOTE make this after making everything else 
doc:
	ocamlfind ocamldoc $$PKGS $$WARN -html -d /tmp *.ml

# temporary do not use ------------------------------------------------

prereqs:
	echo "Making minifs prereqs"
	$(MAKE) -C ../tjr_lib
	$(MAKE) -C ../tjr_net
