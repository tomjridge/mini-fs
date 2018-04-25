#!/bin/bash

# build all dependent projects; assumes these live in ..

# NOTE this should match the relevant Dockerfile; FIXME move Dockerfile here?

DST=..

for f in tjr_step_monad tjr_fs_shared path_resolution tjr_btree tjr_net tjr_lib; do
    make -C $DST/$f clean
    make -C $DST/$f 
done
