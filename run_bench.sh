#!/bin/bash

sudo apt-get update

sudo apt-get install -y xsltproc fop openssl libssl-dev unixodbc-dev autoconf build-essential m4 libssh-dev unixodbc-dev

git clean -d -x -f

source otp_env.source

./otp_build autoconf

./configure

make

source otp_env.source

erlc ets_SUITE.erl

erl -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"
