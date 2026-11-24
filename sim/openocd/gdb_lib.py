#-----------------------------------------------------------------------------
# Title      : GDB helpers of the WardRV OpenOCD co-simulations
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : gdb_lib.py
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Generic part of the GDB scenarios (gdb-multiarch -batch -x
#              <scenario>.py, started by rbb/cosim.sh of the kit
#              asylum:verification:openocd) on the GDB server of OpenOCD
#              connected to a WardRV hart (RISC-V Debug 1.0).
#              The default target of asylum:processor:WardRV copies it to
#              openocd/gdb_lib.py of the work root : the co-simulations of
#              WardRV and of the cores that depend on it (PicoSoC) import
#              the same file. A scenario in openocd/ :
#                import os
#                import sys
#                sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
#                from   gdb_lib import check, run, u32, halt, resume, main
#                ...
#                main([step_a, step_b])
#              main runs the steps (a gdb.error is a failed check), then
#              prints SCENARIO PASS (PASS line of cosim.sh) or SCENARIO FAIL.
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-11-24  1.0      mrosiere Created from the generic part of scenario.py
#-----------------------------------------------------------------------------

import os
import re
import gdb

# dcsr.cause
EBREAK, TRIGGER, HALTREQ, STEP, RESETHALTREQ = 1, 2, 3, 4, 5

errors        = 0

#--------------------------------------------
#--------------------------------------------
def check(cond, msg):
    global errors
    print(("  ok    : " if cond else "  ERROR : ") + msg, flush=True)
    errors += not cond

#--------------------------------------------
#--------------------------------------------
def run(cmd):
    """
    Execute a GDB command
    :param cmd: GDB command
    :return: its output
    """
    print(f"(gdb) {cmd}", flush=True)
    out = gdb.execute(cmd, to_string=True)
    print(out, end="", flush=True)
    return out

#--------------------------------------------
#--------------------------------------------
def fails(cmd):
    """
    Execute a GDB command that must fail
    :param cmd: GDB command
    :return: True when the command raised an error
    """
    try:
        run(cmd)
    except gdb.error as e:
        print(f"      error : {e}", flush=True)
        return True
    return False

#--------------------------------------------
#--------------------------------------------
def u32(expr):
    return int(gdb.parse_and_eval(expr)) & 0xFFFFFFFF

#--------------------------------------------
#--------------------------------------------
def flush():
    """Forget the registers and the memory cached by GDB (after monitor commands)"""
    gdb.execute("maintenance flush register-cache", to_string=True)
    gdb.execute("maintenance flush dcache", to_string=True)

#--------------------------------------------
#--------------------------------------------
def monitor_value(cmd):
    """
    :param cmd: OpenOCD command
    :return: the last hexadecimal number it prints
    """
    return int(re.findall(r"0x[0-9a-fA-F]+", run(f"monitor {cmd}"))[-1], 16)

#--------------------------------------------
#--------------------------------------------
def mdw(addr):
    """
    :param addr: address
    :return: 32-bit word read by OpenOCD ("addr: value"), None if it failed
    """
    try:
        found = re.search(rf"0x{addr:08x}: ([0-9a-f]{{8}})", run(f"monitor mdw 0x{addr:08x}"))
    except gdb.error as e:
        print(f"      error : {e}", flush=True)
        return None
    return int(found.group(1), 16) if found else None

#--------------------------------------------
#--------------------------------------------
def cause():
    return (u32("$dcsr") >> 6) & 7

#--------------------------------------------
#--------------------------------------------
def connect(elf=None, port=None):
    """
    Connect GDB to the GDB server of OpenOCD (the hart halts)
    :param elf: program loaded in GDB (symbols), None : no symbol
    :param port: GDB port of the target, None : $GDB_PORT or 3333
    """
    port = port or int(os.environ.get("GDB_PORT", "3333"))
    for cmd in ("set pagination off", "set confirm off", "set width 0", "set remotetimeout 300"):
        run(cmd)
    if elf:
        run(f"file {elf}")
    run(f"target extended-remote localhost:{port}")

#--------------------------------------------
#--------------------------------------------
def halt():
    """Halt the hart (OpenOCD), forget the cached registers"""
    run("monitor halt")
    flush()

#--------------------------------------------
#--------------------------------------------
def resume():
    """Resume the hart (OpenOCD) : it runs until the next halt"""
    run("monitor resume")

#--------------------------------------------
#--------------------------------------------
def stop_at(location, hardware=False):
    """
    Continue to a temporary breakpoint
    :param location: GDB location (a function : after its prologue)
    :param hardware: hardware breakpoint (trigger), else software (memory access)
    """
    run(f"{'thbreak' if hardware else 'tbreak'} {location}")
    addr = int(gdb.breakpoints()[-1].locations[0].address)
    run("continue")
    check(u32("$pc") == addr, f"stop at {location} : pc 0x{addr:08X}")

#--------------------------------------------
#--------------------------------------------
def openocd_log():
    """
    :return: the log of OpenOCD (openocd.log of rbb/cosim.sh, work root)
    """
    return open("openocd.log").read()

#--------------------------------------------
#--------------------------------------------
def main(steps):
    """
    Run the steps of a scenario, print its result, detach
    :param steps: functions without parameter (a gdb.error is a failed check)
    """
    for step in steps:
        print(f"===== {step.__name__}", flush=True)
        try:
            step()
        except gdb.error as e:
            check(False, f"{step.__name__} : {e}")
    print("SCENARIO PASS" if errors == 0 else f"SCENARIO FAIL ({errors} errors)", flush=True)
    try:
        run("detach")
    except gdb.error:
        pass
