#!/bin/bash




sudo apt-get update

sudo apt-get install -y xsltproc fop openssl libssl-dev unixodbc-dev autoconf build-essential m4 libssh-dev unixodbc-dev libncurses5-dev libncursesw5-dev numactl tmux htop numactl

git clean -d -x -f

numactl --hardware >> ets_SUITE_data/numactl_hardware

cat /proc/cpuinfo >> ets_SUITE_data/proc_cpuinfo

cat /proc/meminfo >> ets_SUITE_data/proc_meminfo

lsb_release -a >> ets_SUITE_data/os_info

echo LINUX >> ets_SUITE_data/os_info

uname -r >> ets_SUITE_data/os_info

source otp_env.source

export CFLAGS="-O2 -g"

./otp_build autoconf

./configure --with-ets-write-concurrency-locks=256

make

source otp_env.source

erlc ets_SUITE.erl

# Spread as much as possible
#erl +sbt s -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"

# Same as db (fills one chip at a time)
erl +sbt tnnps -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"
erl +sbt tnnps -eval "ets_SUITE:lookup_catree_par_vs_seq_init_benchmark(),erlang:halt()"
# # Second round with random join disabled

# make clean

# export CFLAGS="-O2 -g -DERTS_DB_CA_TREE_NO_RANDOM_JOIN_WITH_LOW_PROBABILITY=1"

# ./otp_build autoconf

# ./configure --with-ets-write-concurrency-locks=256

# make

# source otp_env.source

# erlc ets_SUITE.erl

# # Spread as much as possible
# #erl +sbt s -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"

# # Same as db (fills one chip at a time)
# erl +sbt tnnps -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"
# erl +sbt tnnps -eval "ets_SUITE:lookup_catree_par_vs_seq_init_benchmark(),erlang:halt()"
