#!/bin/bash

# echo "/home/kjell/src/work/otp_2/core" > /proc/sys/kernel/core_pattern

CORECOUNT=0

for i in `seq 1 10`;
do
    rm core
    echo SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS starting run $i
    (cd $TESTSERVER && cerl  +Muatags true -eval "erts_debug:df(ets_SUITE), ts:run(stdlib, ets_SUITE,   [batch]), erlang:halt()")
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



