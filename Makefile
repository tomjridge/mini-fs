TMP_DOC_DIR:=/tmp/minifs
#scratch:=/tmp/l/github/scratch

default: all
-include Makefile.ocaml

all::
	$(DUNE) build @bin/all_exes
	find _build -name "*.exe" -exec cp \{\} . \;  # copy exes to this directory


# nfs fuse client and server -----------------------------------------

run_nfs_server: 
	 ./nfs_server_in_mem.exe

# direct stderr to stdout
run_fuse_nfs_client: 
	 ./fuse_nfs_client.exe -s -f -o auto_unmount -o sync_read -o debug fuse_mount/ 2>&1


# run:
# 	cd test && $(test)

# for auto-completion of Makefile target
clean::
	rm -f *.exe
