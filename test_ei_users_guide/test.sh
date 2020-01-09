#!/bin/bash

source ~/src/notes/otp_env.source

#cc -c  -I$ERL_TOP/lib/erl_interface/include myprog.c

gcc -g -Wall -I$ERL_TOP/lib/erl_interface/include -pedantic -L$ERL_TOP/lib/erl_interface/obj/x86_64-unknown-linux-gnu/ myprog.c -lei -lpthread -o my_test

pwd

./my_test
