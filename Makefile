TMP_DOC_DIR:=/tmp/tjr_path_resolution
#scratch:=/tmp/l/github/scratch

default: all
-include Makefile.ocaml

all::
	$(DUNE) build @bin/all_exes
	find _build -name "*.exe" -exec cp \{\} . \;  # copy exes to this directory

# run:
# 	cd test && $(test)

# for auto-completion of Makefile target
clean::
	rm -f *.exe
