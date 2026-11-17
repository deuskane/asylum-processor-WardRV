#!/bin/bash
#-----------------------------------------------------------------------------
# Title      : Hooks of the WardRV OpenOCD + GDB co-simulation
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : cosim.sh
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Hooks of the sim_dbg_openocd_* targets (cwd = work root, files
#              in openocd/) :
#                build : pre_build, librbb.so (remote_bitbang.c) and the
#                        firmware openocd/fw/fw.elf, fw.hex (RISC-V toolchain)
#                start : pre_run, starts in the background : wait for the
#                        remote_bitbang port of the simulation, OpenOCD
#                        (openocd.log), GDB with scenario.py (gdb.log) and the
#                        configuration written by the testbench (cosim.cfg),
#                        then OpenOCD shutdown : the simulation ends
#                check : post_run, prints the logs, fails unless the
#                        scenario passed
#              Needs openocd and gdb-multiarch (with Python).
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-11-17  1.0      mrosiere Created
#-----------------------------------------------------------------------------

# True when the TCP port is listening
listening() {
    awk -v p=$(printf ':%04X' $1) '$2 ~ p"$" && $4 == "0A" {f = 1} END {exit !f}' /proc/net/tcp
}

# Wait (seconds) until the port listens or the process $2 ends
wait_port() {
    for t in $(seq 1 900); do
        listening $1 && return 0
        kill -0 $2 2>/dev/null || return 1
        sleep 1
    done
    return 1
}

background() {
    wait_port 9823 $1                                      || { echo "FAIL : no simulation"      > result; return; }
    openocd -f openocd/wardrv.cfg > openocd.log 2>&1 &
    wait_port 3333 $!                                      || { echo "FAIL : OpenOCD not ready"  > result; }
    if [ ! -s result ]; then
        # Debug options of the testbench, then the scenario
        set -a; . ./cosim.cfg; set +a
        timeout 1800 gdb-multiarch -nx -batch -x openocd/scenario.py > gdb.log 2>&1
        grep -qx "SCENARIO PASS" gdb.log && echo PASS > result || echo "FAIL : see gdb.log" > result
    fi
    # TCL command shutdown : OpenOCD quits, the simulation ends
    printf 'shutdown\032' > /dev/tcp/127.0.0.1/6666
    wait
}

case $1 in
    build)
        gcc -shared -fPIC -O2 -Wall -o librbb.so openocd/remote_bitbang.c &&
        make -C openocd/fw ;;
    start)
        rm -f result openocd.log gdb.log
        setsid bash $0 background $PPID < /dev/null > cosim.log 2>&1 &
        echo "cosim.sh : OpenOCD and GDB started in the background" ;;
    background)
        background $2 ;;
    check)
        for t in $(seq 1 60); do [ -s result ] && break; sleep 1; done
        for f in openocd.log gdb.log; do echo "===== $f"; cat $f; done
        echo "===== co-simulation : $(cat result)"
        [ "$(cat result)" = PASS ] ;;
esac
