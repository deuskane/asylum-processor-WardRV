#-----------------------------------------------------------------------------
# Title      : GDB scenario of the WardRV co-simulation
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : scenario.py
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: gdb-multiarch -batch -x scenario.py, on the GDB server of
#              OpenOCD connected to tb_WardRV_openocd (firmware esw/openocd).
#              The debug options of the testbench (environment, cosim.cfg)
#              are checked against OpenOCD. Every feature present is used,
#              every absent one must fail with an error :
#                memory      : program buffer (halted) or System Bus Access
#                              (also while the core runs)
#                triggers    : hbreak, watch (more than 2 together if
#                              there are)
#                without     : halt, resume, registers, monitor step
#              Prints ok / ERROR for each check, then SCENARIO PASS / FAIL.
#              Generic helpers in gdb_lib.py (next to this file).
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-11-17  1.0      mrosiere Created
# 2026-11-24  1.1      mrosiere Generic helpers moved to gdb_lib.py
#-----------------------------------------------------------------------------

import os
import re
import sys
import gdb

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from   gdb_lib import check, run, fails, u32, flush, monitor_value, mdw, cause
from   gdb_lib import connect, halt, resume, stop_at, openocd_log, main
from   gdb_lib import EBREAK, TRIGGER, HALTREQ, STEP, RESETHALTREQ

PROGBUF       = int(os.environ["PROGBUF_SIZE"])
DATA_COUNT    = int(os.environ["DATA_COUNT"])
SBA           = os.environ["SBA_ENABLE"] == "1"
TRIGGERS      = int(os.environ["NB_TRIGGERS"])
MEMORY        = PROGBUF > 0 or SBA                 # Memory access when halted

IDCODE        = 0x1DEB0001                         # tb_WardRV_openocd
HARTID        = 0x900DC0DE
RESET_ADDR    = 0x80000000
PATTERN       = [0x11111111, 0x22222222, 0x33333333, 0x44444444]

#--------------------------------------------
# Steps
#--------------------------------------------
def step_connect():
    connect("openocd/fw/fw.elf")
    log = openocd_log()
    check(f"tap/device found: 0x{IDCODE:08x}" in log, f"IDCODE 0x{IDCODE:08X}")
    check("Examined RISC-V core" in log, "RISC-V core examined")
    check(f"datacount={DATA_COUNT} progbufsize={PROGBUF}" in log, f"OpenOCD found {DATA_COUNT} data, {PROGBUF} program buffer words")
    dmstatus = monitor_value("riscv dmi_read 0x11")
    check(dmstatus & 0xF == 3 and (dmstatus >> 22) & 1 == 1, "dmstatus : version 1.0, impebreak")
    check((monitor_value("riscv dmi_read 0x38") >> 29 == 1) == SBA, f"sbcs.sbversion : SBA {SBA}")

#--------------------------------------------
#--------------------------------------------
def step_reset_halt():
    run("monitor reset halt")
    flush()
    check(u32("$pc") == RESET_ADDR, f"pc = 0x{RESET_ADDR:08X} after reset halt")
    check(cause() in (HALTREQ, RESETHALTREQ), "dcsr.cause : haltreq or resethaltreq")

#--------------------------------------------
#--------------------------------------------
def step_registers():
    for reg, value in (("a0", 0x12345678), ("t1", 0xCAFEF00D), ("s1", 0x0BADC0DE)):
        run(f"set ${reg} = 0x{value:08x}")
        flush()
        check(u32(f"${reg}") == value, f"{reg} = 0x{value:08X}")
    check(u32("$zero") == 0, "x0 = 0")
    check(u32("$mhartid") == HARTID, f"mhartid = 0x{HARTID:08X}")
    run("set $mscratch = 0xa5a5a5a5")
    flush()
    check(u32("$mscratch") == 0xA5A5A5A5, "mscratch written")
    dcsr = u32("$dcsr")
    check(dcsr >> 28 == 4 and dcsr & 3 == 3, f"dcsr : debugver 4, prv 3 (0x{dcsr:08X})")
    run(f"set $dcsr = 0x{(dcsr | 1 << 11) & ~(7 << 6):08x}")
    flush()
    check(u32("$dcsr") == dcsr | 1 << 11, "dcsr.stepie written, dcsr.cause read-only")
    run(f"set $dcsr = 0x{dcsr:08x}")
    run("set $pc = &hb_func")
    flush()
    check(u32("$dpc") == u32("(unsigned int)&hb_func"), "pc written : dpc")
    run(f"set $pc = 0x{RESET_ADDR:08x}")

#--------------------------------------------
#--------------------------------------------
def step_load():
    if not MEMORY:
        check(fails("load"), "no memory access : load fails")
        check(fails("x/4wx &pattern"), "no memory access : x fails")
        return
    words = (u32("(unsigned int)&_bss_start") - RESET_ADDR) // 4
    for method in ["progbuf"] * (PROGBUF > 0) + ["sysbus"] * SBA:
        run(f"monitor riscv set_mem_access {method}")
        run(f"monitor mww 0x{RESET_ADDR:08x} 0xdeadbeef {words}")
        flush()
        check("MIS-MATCHED" in run("compare-sections"), f"{method} : image overwritten")
        run("load")
        check("MIS-MATCHED" not in run("compare-sections"), f"{method} : load, compare-sections matched")
    run("monitor riscv set_mem_access progbuf sysbus abstract")
    run(f"set $pc = 0x{RESET_ADDR:08x}")

#--------------------------------------------
#--------------------------------------------
def step_breakpoints():
    bp = gdb.Breakpoint("bp_func")
    if MEMORY:
        for hit in range(2):
            run("continue")
            check(u32("$pc") == bp.locations[0].address and cause() == EBREAK, f"break bp_func {hit}, dcsr.cause ebreak")
            check(u32("x") == hit and u32("bp_hits") == hit, f"x = bp_hits = {hit}")
    else:
        check(fails("continue"), "no memory access : software breakpoint refused")
    bp.delete()

    if TRIGGERS:
        # The trigger fires before the store, GDB then steps over it
        run("watch watched")
        wp = gdb.breakpoints()[-1]
        check(wp.type == gdb.BP_HARDWARE_WATCHPOINT, "hardware watchpoint")
        run("continue")
        if MEMORY:
            check(wp.hit_count == 1 and u32("$pc") > u32("(unsigned int)&main"), "watch : hit, stop in main")
            check(u32("watched") == 1 + 2 and u32("counter") == 1, "watched = 3 after the iteration 1")
        else:
            # OpenOCD 0.12 reads the store instruction to find the watchpoint
            check(cause() == TRIGGER, "watch : the trigger fires, GDB sees a SIGTRAP (no memory access)")
        wp.delete()
        run("hbreak hb_func")
        hb = gdb.breakpoints()[-1]
        run("continue")
        check(u32("$pc") == hb.locations[0].address and cause() == TRIGGER, "hbreak hb_func, dcsr.cause trigger")
        hb.delete()
    else:
        run("hbreak hb_func")
        check(fails("continue"), "no trigger : hbreak refused")
        gdb.breakpoints()[-1].delete()
        run("watch watched")
        wp = gdb.breakpoints()[-1]
        check(wp.type == gdb.BP_HARDWARE_WATCHPOINT, "no software watchpoint")
        check(fails("continue"), "no trigger : watch refused")
        wp.delete()

#--------------------------------------------
#--------------------------------------------
def step_stepping():
    if MEMORY or TRIGGERS:
        stop_at("step_test", not MEMORY)
    else:
        # No breakpoint : jump to step_test, return to end_marker
        run("set $pc = &step_test")
        run("set $ra = &end_marker")
    base = u32("(unsigned int)&step_test")
    # (offset, t0) after each instruction of step_test (start.S)
    for offset, t0 in ((4, 1), (8, 3), (12, 6), (20, 6), (24, 10), (28, 10)):
        run("stepi" if MEMORY else "monitor step")
        flush()
        check(u32("$pc") == base + offset and u32("$t0") == t0, f"step : step_test+{offset}, t0 = {t0}")
    run("monitor step")
    flush()
    check(u32("$a0") == 10 and cause() == STEP, "monitor step (dcsr.step) : ret, a0 = 10, dcsr.cause step")
    if MEMORY:
        stop_at("next_test")
        line = gdb.selected_frame().find_sal().line
        for n in (1, 2):
            run("next")
            check(gdb.selected_frame().find_sal().line == line + n and u32("next_calls") == n, f"next : line +{n}")

#--------------------------------------------
#--------------------------------------------
def step_memory():
    if not MEMORY:
        return
    check([u32(f"pattern[{i}]") for i in range(4)] == PATTERN, "x pattern")
    run("set var limit = 3")
    run("set var pattern[1] = 0x01020304")
    run("set var *(unsigned char *)&pattern[0] = 0x99")
    run("set var *(unsigned short *)&pattern[2] = 0xbeef")
    for method in ["progbuf"] * (PROGBUF > 0) + ["sysbus"] * SBA:
        run(f"monitor riscv set_mem_access {method}")
        flush()
        check([u32(f"pattern[{i}]") for i in range(3)] == [0x11111199, 0x01020304, 0x3333BEEF] and u32("limit") == 3,
              f"{method} : 8, 16 and 32-bit writes read back")
    run("monitor riscv set_mem_access progbuf sysbus abstract")

#--------------------------------------------
#--------------------------------------------
def step_end():
    expected = (0x11111199 + 0x01020304 + 0x3333BEEF) if MEMORY else sum(PATTERN)
    if MEMORY or TRIGGERS:
        stop_at("end_marker", not MEMORY)
        check(u32("$a0") == expected & 0xFFFFFFFF, f"result 0x{expected & 0xFFFFFFFF:08X} (argument of end_marker)")
    else:
        resume()
        halt()
        end = u32("(unsigned int)&end_marker")
        check(end <= u32("$pc") < end + 0x40, "the firmware runs in end_marker")
    # Memory while the core runs
    ticks = u32("(unsigned int)&ticks")
    resume()
    if SBA:
        run("monitor riscv set_mem_access sysbus")
        first = mdw(ticks)
        check(first is not None and mdw(ticks) > first, "System Bus Access : ticks read while the core runs")
    else:
        check(mdw(ticks) is None, "no memory access while the core runs")
    halt()

    # OpenOCD enumerates the triggers when the first breakpoint is set
    found = re.search(r"Found (\d+) triggers", openocd_log())
    check(found is not None and int(found.group(1)) == TRIGGERS, f"OpenOCD found {TRIGGERS} triggers")

#--------------------------------------------
#--------------------------------------------
def step_many_triggers():
    """From the reset : 4 hbreak and 1 watch set together, all hit"""
    run("monitor reset halt")
    flush()
    for location in ("hb_func", "step_test", "next_test", "helper"):
        run(f"hbreak {location}")
    bps = {bp.location: bp for bp in gdb.breakpoints()}
    for location in ("hb_func", "step_test", "next_test", "helper", "helper"):
        run("continue")
        check(u32("$pc") == bps[location].locations[0].address and cause() == TRIGGER, f"hbreak {location}")
        if location == "hb_func":
            run("watch finished")                    # .bss cleared by _start before
            wp = gdb.breakpoints()[-1]
    run("continue")
    check(wp.hit_count == 1 and u32("finished") == u32("result"), "watch finished")
    run("delete")

#--------------------------------------------
#--------------------------------------------
def step_refused_without_memory():
    """
    Last step : after a refused memory write, the next resume of OpenOCD 0.12
    fails once ("Failed to read priv register"), and a refused stepi leaves
    GDB in the middle of a software single step
    """
    check(fails("set var limit = 3"), "no memory access : set var fails")
    check(fails("stepi"), "no memory access : stepi (software single step) fails")

#--------------------------------------------
# Main
#--------------------------------------------
print(f"Configuration : PROGBUF_SIZE {PROGBUF}, SBA_ENABLE {SBA}, NB_TRIGGERS {TRIGGERS}", flush=True)
steps = [step_connect, step_reset_halt, step_registers, step_load, step_breakpoints,
         step_stepping, step_memory, step_end]
steps += [step_many_triggers] * (TRIGGERS > 2) + [step_refused_without_memory] * (not MEMORY)
main(steps)
