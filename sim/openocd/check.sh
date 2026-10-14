#!/bin/bash
#-----------------------------------------------------------------------------
# Title      : Result of the WardRV co-simulation (post_run hook)
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : check.sh
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: post_run hook of the cosim_openocd target (cwd = fusesoc
#              work_root) : prints the logs of launch.sh, OpenOCD and GDB,
#              fails unless scenario.result is PASS.
# Environment: RESULT_TIMEOUT (60 s : wait for scenario.result),
#              OPENOCD_LOG_LINES (200 : last lines of openocd.log printed)
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-09  1.0      mrosiere Created
#-----------------------------------------------------------------------------

RESULT_TIMEOUT=${RESULT_TIMEOUT:-60}
OPENOCD_LOG_LINES=${OPENOCD_LOG_LINES:-200}

for i in $(seq 1 "$RESULT_TIMEOUT"); do
    [ -s scenario.result ] && break
    sleep 1
done

section() {
    echo ""
    echo "======================================================================"
    echo "== $1 ($(pwd)/$2)"
    echo "======================================================================"
}

section "OpenOCD (last $OPENOCD_LOG_LINES lines)" openocd.log
tail -n "$OPENOCD_LOG_LINES" openocd.log 2>/dev/null || echo "(no openocd.log)"
section "GDB scenario" gdb.log
cat gdb.log 2>/dev/null || echo "(no gdb.log)"
section "Launcher" launch.log
cat launch.log 2>/dev/null || echo "(no launch.log)"

result=$(cat scenario.result 2>/dev/null)
echo ""
if [ "$result" = "PASS" ]; then
    echo "cosim_openocd : PASS"
    exit 0
fi
echo "cosim_openocd : FAIL (scenario.result = '${result:-missing}')"
exit 1
