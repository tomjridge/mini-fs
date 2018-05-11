#!/bin/bash

# build all dependent projects; assumes these live in ..

# NOTE this should match the relevant Dockerfile; FIXME move Dockerfile here?

DST=..

(cd $DST/tjr_btree && ./build_parent.sh)
make -C $DST/tjr_btree clean
make -C $DST/tjr_btree
