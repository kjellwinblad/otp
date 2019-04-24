#!/bin/bash

CORECOUNT=0

for i in `seq 1 100000`;
do
    rm core
    echo SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS starting run $i
    (cd $TESTSERVER && cerl -rr -debug +Muatags true -eval "erts_debug:df(ets_SUITE), ts:run(stdlib, ets_SUITE,   [batch]), erlang:halt()")
    echo KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK run $i ended
    if [ -f core ]; then
        CORECOUNT=$(expr $CORECOUNT + 1)
        echo CCCCCCCCCCCCCCCCCCCCCC $CORECOUNT cores found in $i runs
        break
    else
        echo CCCCCCCCCCCCCCCCCCCCCC $CORECOUNT cores found in $i runs
    fi
done


echo Core count $CORECOUNT



