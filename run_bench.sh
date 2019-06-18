#!/bin/bash




sudo apt-get update

sudo apt-get install -y xsltproc fop openssl libssl-dev unixodbc-dev autoconf build-essential m4 libssh-dev unixodbc-dev libncurses5-dev libncursesw5-dev numactl tmux htop

git clean -d -x -f

numactl --hardware >> ets_SUITE_data/numactl_hardware

cat /proc/cpuinfo >> ets_SUITE_data/proc_cpuinfo

cat /proc/meminfo >> ets_SUITE_data/proc_meminfo

lsb_release -a >> ets_SUITE_data/os_info

echo LINUX >> ets_SUITE_data/os_info

uname -r >> ets_SUITE_data/os_info


#git checkout OTP-21.3.8.3

#source otp_env.source

#./otp_build autoconf

#./configure

#make

#source otp_env.source

#erlc ets_SUITE.erl

# Same as db (fills one chip at a time)
#erl +sbt tnnps -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"


#git checkout kjell/bench/21_vs_22_ordered_set

git clean -d -x -f

source otp_env.source

./otp_build autoconf

./configure

make

source otp_env.source

erlc ets_SUITE.erl

# Same as db (fills one chip at a time)
erl +sbt tnnps -eval "ets_SUITE:throughput_benchmark(),erlang:halt()"
