

all: fuse_main.native


fuse_main.native: FORCE
	ocamlfind ocamlopt -w -58 -thread -package tjr_minifs -linkpkg -o fuse_main.native fuse_main.ml


run: fuse_main.native
	mkdir -p fuse_mount
	./fuse_main.native -f -o allow_other ./fuse_mount


FORCE:
