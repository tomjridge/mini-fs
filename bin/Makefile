SHELL:=bash
BASH_ENV=bash_env.sh
export BASH_ENV

all: natives


natives: fuse_main.native fuse_nfs_client.native nfs_server.native nfs_client.native
	cp *.native /tmp


fuse_main.native: FORCE
	ocamlfind ocamlopt -w -58 -thread -package tjr_minifs -linkpkg -o fuse_main.native fuse_main.ml


%.native: %.ml FORCE
	ocamlfind ocamlopt -w -58 -thread -package tjr_minifs,tjr_net -linkpkg -o $*.native $*.ml


run: fuse_main.native
	mkdir -p fuse_mount
	./fuse_main.native -s -f -o allow_other ./fuse_mount 

# -f is foreground
# -s is single-threaded


clean: 
	rm -f *.native a.out *.{cmi,cmx,o}

FORCE:
