#!/bin/bash
#-----------------------------------------------------------------------------
# Title      : Debugger side of the WardRV co-simulation (pre_run hook)
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : launch.sh
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: pre_run hook of the cosim_openocd target (cwd = fusesoc
#              work_root). Starts in the background (setsid) and returns at
#              once, so that fusesoc starts the simulation :
#              1. wait until the simulation listens on RBB_PORT
#                 (remote_bitbang server of tb_WardRV_openocd)
#              2. openocd -f openocd/wardrv_sim.cfg          -> openocd.log
#              3. gdb-multiarch -batch -x openocd/scenario.py -> gdb.log
#              4. scenario.result = PASS / FAIL
#              5. OpenOCD "shutdown" command (TCL port) : OpenOCD sends Q to
#                 the remote_bitbang server and the simulation ends
#              Launcher trace : launch.log. Result checked by check.sh
#              (post_run hook).
# Environment: RBB_PORT (9823), GDB_PORT (3333), TCL_PORT (6666),
#              WAIT_TIMEOUT (900 s : compilation, elaboration and start of
#              the simulation), GDB_TIMEOUT (1800 s : scenario),
#              OPENOCD (openocd), GDB (gdb-multiarch)
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-09  1.0      mrosiere Created
#-----------------------------------------------------------------------------

export RBB_PORT=${RBB_PORT:-9823}
export GDB_PORT=${GDB_PORT:-3333}
export TCL_PORT=${TCL_PORT:-6666}
WAIT_TIMEOUT=${WAIT_TIMEOUT:-900}
GDB_TIMEOUT=${GDB_TIMEOUT:-1800}
OPENOCD=${OPENOCD:-openocd}
GDB=${GDB:-gdb-multiarch}

DIR=$(dirname "$0")
export FW_ELF=${FW_ELF:-$DIR/fw.elf}
export OPENOCD_LOG=openocd.log
export SCENARIO_VERDICT=scenario.verdict

log() {
    echo "[launch $(date +%T)] $*"
}

# True when a TCP port is in LISTEN state (st 0A) in this network namespace
port_listening() {
    local hex
    hex=$(printf ':%04X' "$1")
    awk -v p="$hex" '$2 ~ p"$" && $4 == "0A" { found = 1 } END { exit !found }' \
        /proc/net/tcp /proc/net/tcp6 2>/dev/null
}

result() {
    echo "$1" > scenario.result
    log "scenario.result = $1${2:+ ($2)}"
}

# Ask OpenOCD to quit (it sends Q to the simulation), kill it if needed
shutdown_openocd() {
    local pid=$1 i
    if kill -0 "$pid" 2>/dev/null; then
        log "OpenOCD shutdown (TCL port $TCL_PORT)"
        # TCL RPC : command terminated by 0x1a
        ( printf 'shutdown\032' > "/dev/tcp/127.0.0.1/$TCL_PORT" ) 2>/dev/null \
            || log "TCL port $TCL_PORT not reachable"
        for i in $(seq 1 60); do
            kill -0 "$pid" 2>/dev/null || break
            sleep 1
        done
        if kill -0 "$pid" 2>/dev/null; then
            log "OpenOCD still running : kill"
            kill "$pid"
        fi
    fi
    wait "$pid" 2>/dev/null
    log "OpenOCD exit status $?"
}

background() {
    local parent=$1 t openocd_pid gdb_status

    log "waiting for the simulation (remote_bitbang port $RBB_PORT, timeout $WAIT_TIMEOUT s)"
    for t in $(seq 1 "$WAIT_TIMEOUT"); do
        port_listening "$RBB_PORT" && break
        if ! kill -0 "$parent" 2>/dev/null; then
            result FAIL "fusesoc ended before the simulation started"
            return
        fi
        sleep 1
    done
    if ! port_listening "$RBB_PORT"; then
        result FAIL "remote_bitbang port $RBB_PORT not open after $WAIT_TIMEOUT s"
        return
    fi
    log "simulation listening after $t s"

    log "starting $OPENOCD -f $DIR/wardrv_sim.cfg"
    "$OPENOCD" -f "$DIR/wardrv_sim.cfg" > openocd.log 2>&1 &
    openocd_pid=$!
    for t in $(seq 1 300); do
        port_listening "$GDB_PORT" && break
        kill -0 "$openocd_pid" 2>/dev/null || break
        sleep 1
    done
    if ! port_listening "$GDB_PORT"; then
        result FAIL "OpenOCD gdb port $GDB_PORT not open (see openocd.log)"
        shutdown_openocd "$openocd_pid"
        return
    fi
    log "OpenOCD ready after $t s"

    log "starting $GDB -batch -x $DIR/scenario.py (timeout $GDB_TIMEOUT s)"
    timeout "$GDB_TIMEOUT" "$GDB" -nx -batch -x "$DIR/scenario.py" > gdb.log 2>&1
    gdb_status=$?
    log "GDB exit status $gdb_status"

    if [ "$gdb_status" -eq 0 ] && grep -qx "SCENARIO: PASS" scenario.verdict 2>/dev/null; then
        result PASS
    elif [ "$gdb_status" -eq 124 ]; then
        result FAIL "GDB timeout after $GDB_TIMEOUT s"
    else
        result FAIL "see gdb.log"
    fi

    shutdown_openocd "$openocd_pid"
}

if [ "$1" = "--background" ]; then
    background "$2"
    exit 0
fi

# pre_run hook : clean the previous results, start the background part
rm -f scenario.result scenario.verdict openocd.log gdb.log launch.log
setsid bash "$0" --background "$PPID" < /dev/null > launch.log 2>&1 &
echo "launch.sh : OpenOCD + GDB started in the background (launch.log, openocd.log, gdb.log)"
exit 0
