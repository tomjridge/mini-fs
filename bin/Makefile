SHELL:=bash

all: fuse_main.native


fuse_main.native: FORCE
	ocamlfind ocamlopt -w -58 -thread -package tjr_minifs -linkpkg -o fuse_main.native fuse_main.ml


run: fuse_main.native
	mkdir -p fuse_mount
	./fuse_main.native -s -f -o allow_other ./fuse_mount 

# -f is foreground
# -s is single-threaded


clean: 
	rm -f *.native a.out *.{cmi,cmx,o}

FORCE:
