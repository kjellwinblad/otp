#!/bin/bash

git clean -d -x -f

source otp_env.source

./otp_build autoconf

./configure

make

source otp_env.source

erlc ets_SUITE.erl

erl -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"
