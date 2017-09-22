SHELL:=/bin/bash
BASH_ENV=bash_env.sh
export BASH_ENV

# shouldn't shell builtin source bash_env anyway?
libname:=$(shell source bash_env.sh && echo $${libname})#


all:
	$$ocamlc -c $$mls
	mk_cma
	$(MAKE) install

install:
	mk_meta
	-remove
	install


clean:
	clean
	rm -f *.html

