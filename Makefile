SHELL:=/bin/bash
BASH_ENV=bash_env.sh
export BASH_ENV

# shouldn't shell builtin source bash_env anyway?
libname:=$(shell source bash_env.sh && echo $${libname})#

# generate: 
# 	$$ocamlopt -linkpkg literate.mli literate.ml
# 	./a.out .

all:
	$$ocamlc -c $$mls
	mk_cma
	$$ocamlopt -c $$mls
	mk_cmxa
	$(MAKE) install
	$(MAKE) -C bin

run: all
	$(MAKE) -C bin run

link:
	link

install:
	mk_meta
	-remove
	install


clean:
	clean
	rm -f *.html
	$(MAKE) -C bin clean

