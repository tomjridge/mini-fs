TMP_DOC_DIR:=/tmp/tjr_path_resolution
#scratch:=/tmp/l/github/scratch

default: all
-include Makefile.ocaml

all::
	$(MAKE) -C bin

# run:
# 	cd test && $(test)

# for auto-completion of Makefile target
clean::
