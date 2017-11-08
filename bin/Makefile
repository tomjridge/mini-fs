SHELL:=bash
BASH_ENV=bash_env.sh
export BASH_ENV

all: natives


natives: fuse_in_mem.native fuse_nfs_client.native nfs_server_in_mem.native nfs_client_test.native nfs_server_unix.native
	cp *.native /tmp


fuse_main.native: FORCE
	ocamlfind ocamlopt -w -58 -thread -package tjr_minifs -linkpkg -o fuse_main.native fuse_main.ml


%.native: %.ml FORCE
	ocamlfind ocamlopt -w -58 -thread -package tjr_minifs,tjr_net -linkpkg -o $*.native $*.ml


# fuse options:
# -f is foreground
# -s is single-threaded

run_fuse_in_mem: fuse_in_mem.native
	mkdir -p fuse_mount
	./fuse_in_mem.native -s -f -o allow_other ./fuse_mount 


run_fuse_nfs_client: fuse_nfs_client.native
	-fusermount -u ./fuse_mount
	mkdir -p fuse_mount
	./fuse_nfs_client.native -s -f -o allow_other ./fuse_mount 


# expose host filesystem on network
run_nfs_server_unix: nfs_server_unix.native
	./nfs_server_unix.native 


clean: 
	rm -f *.native a.out *.{cmi,cmx,o}

FORCE:
