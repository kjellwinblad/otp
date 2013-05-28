#!/bin/bash

BEAM_DIR=erts/emulator/beam

rm -rf c_maps

git clone git://github.com/kjellwinblad/c_maps.git

cd c_maps

git checkout -b concurrent_structure origin/concurrent_structure

cd ..

cp c_maps/src/maps/*.c $BEAM_DIR
cp c_maps/src/maps/*.h $BEAM_DIR

cp c_maps/src/utils/*.c $BEAM_DIR
cp c_maps/src/utils/*.h $BEAM_DIR
