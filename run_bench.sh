#!/bin/bash

sudo apt-get update

sudo apt-get install -y xsltproc fop openssl libssl-dev unixodbc-dev autoconf build-essential m4 libssh-dev unixodbc-dev libncurses5-dev libncursesw5-dev

git clean -d -x -f

source otp_env.source

./otp_build autoconf

./configure

make

source otp_env.source

erlc ets_SUITE.erl

erl +sbt nnts -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"
