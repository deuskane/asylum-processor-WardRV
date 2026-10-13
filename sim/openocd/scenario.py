#-----------------------------------------------------------------------------
# Title      : GDB scenario of the WardRV co-simulation
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : scenario.py
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: GDB Python script (gdb-multiarch -nx -batch -x scenario.py),
#              run by sim/openocd/launch.sh on the gdb server of OpenOCD,
#              itself connected to the simulation (tb_WardRV_openocd).
#              Firmware : esw/openocd (fw.elf). Steps :
#              1. connection : IDCODE, Debug Module examined, version 1.0
#              2. monitor reset halt : pc = RESET_ADDR
#              3. load + compare-sections, through the program buffer then
#                 through the System Bus Access
#              4. GPR and CSR read / write (dcsr, dpc, mscratch)
#              5. software breakpoint (break) on a function
#              6. watchpoint (store trigger) and hardware breakpoint
#                 (execute trigger)
#              7. stepi and next
#              8. memory : x/ and set var (8, 16, 32 bits)
#              9. continue to the end marker, effect of set var
#              Every check prints ok / ERROR, every step PASS / FAIL, then
#              "SCENARIO: PASS" or "SCENARIO: FAIL", also written in
#              SCENARIO_VERDICT (scenario.verdict).
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-09  1.0      mrosiere Created
#-----------------------------------------------------------------------------

import os
import re
import sys
import time

import gdb

FW_ELF      = os.environ.get("FW_ELF", "openocd/fw.elf")
GDB_PORT    = os.environ.get("GDB_PORT", "3333")
OPENOCD_LOG = os.environ.get("OPENOCD_LOG", "openocd.log")
VERDICT     = os.environ.get("SCENARIO_VERDICT", "scenario.verdict")

IDCODE      = 0x1DEB0001        # tb_WardRV_openocd C_IDCODE
HARTID      = 0x900DC0DE        # tb_WardRV_pkg C_HARTID
RESET_ADDR  = 0x80000000        # tb_WardRV_pkg C_FIRMWARE_ADDR
LOOP_COUNT  = 8                 # esw/openocd/main.c
PATTERN     = [0x11111111, 0x22222222, 0x33333333, 0x44444444]

# dcsr.cause
CAUSE_EBREAK, CAUSE_TRIGGER, CAUSE_HALTREQ, CAUSE_STEP, CAUSE_RESETHALTREQ = 1, 2, 3, 4, 5

T0 = time.time()
results = []                    # (step name, PASS / FAIL)
last_stop = {}


def say(msg):
    sys.stdout.write(msg + "\n")
    sys.stdout.flush()


def on_stop(event):
    last_stop.clear()
    last_stop["event"] = type(event).__name__
    if isinstance(event, gdb.BreakpointEvent):
        last_stop["breakpoints"] = [b.number for b in event.breakpoints]
    if isinstance(event, gdb.SignalEvent):
        last_stop["signal"] = event.stop_signal


gdb.events.stop.connect(on_stop)


class StepFailed(Exception):
    pass


class Step:
    """One step of the scenario : a list of checks"""

    def __init__(self, name):
        self.name = name
        self.errors = 0

    def __enter__(self):
        say("")
        say("=" * 72)
        say("STEP %s (t = %.1f s)" % (self.name, time.time() - T0))
        say("=" * 72)
        return self

    def check(self, cond, msg):
        if cond:
            say("  ok    : " + msg)
        else:
            say("  ERROR : " + msg)
            self.errors += 1
        return cond

    def require(self, cond, msg):
        if not self.check(cond, msg):
            raise StepFailed(msg)

    def __exit__(self, exc_type, exc, tb):
        if exc_type is not None:
            self.errors += 1
            say("  ERROR : exception %s: %s" % (exc_type.__name__, exc))
        verdict = "PASS" if self.errors == 0 else "FAIL"
        results.append((self.name, verdict))
        say("STEP %s : %s (t = %.1f s)" % (self.name, verdict, time.time() - T0))
        # Go on with the next step (a gdb error ends only this step)
        return exc_type is None or issubclass(exc_type, Exception)


def run(cmd):
    """Execute a GDB command, print and return its output"""
    say("(gdb) " + cmd)
    out = gdb.execute(cmd, to_string=True) or ""
    for line in out.rstrip("\n").splitlines():
        say("      " + line)
    return out


def u32(expr):
    return int(gdb.parse_and_eval(expr)) & 0xFFFFFFFF


def pc():
    return u32("$pc")


def flush():
    """Forget the registers and the memory cached by GDB (after monitor commands)"""
    gdb.execute("maintenance flush register-cache", to_string=True)
    gdb.execute("maintenance flush dcache", to_string=True)


def dcsr_cause():
    return (u32("$dcsr") >> 6) & 7


def func_name():
    f = gdb.selected_frame()
    return f.name() if f is not None else None


def bp_address(bp):
    return int(bp.locations[0].address) & 0xFFFFFFFF


def monitor_value(cmd):
    """Value printed by a monitor command (last hexadecimal number)"""
    out = run("monitor " + cmd)
    values = re.findall(r"0x[0-9a-fA-F]+", out)
    if not values:
        raise ValueError("no value in the output of 'monitor %s'" % cmd)
    return int(values[-1], 16)


def read_words(expr, count):
    """Read count 32-bit words with x/"""
    out = run("x/%dwx %s" % (count, expr))
    words = []
    for line in out.splitlines():
        words += [int(w, 16) for w in re.findall(r"\s(0x[0-9a-fA-F]{8})\b", line.split(":", 1)[-1])]
    return words


def cont():
    last_stop.clear()
    return run("continue")


#-----------------------------------------------------------------------------
# Scenario
#-----------------------------------------------------------------------------
def step_connect(s):
    run("set pagination off")
    run("set confirm off")
    run("set width 0")
    run("set remotetimeout 300")
    run("set code-cache off")
    run("set stack-cache off")
    run("file " + FW_ELF)
    run("target extended-remote localhost:" + GDB_PORT)
    try:
        with open(OPENOCD_LOG) as f:
            log = f.read()
    except OSError:
        log = ""
    s.check(re.search(r"tap/device found: 0x%08x" % IDCODE, log, re.I) is not None,
            "OpenOCD log : IDCODE 0x%08X found" % IDCODE)
    s.check("Examined RISC-V core" in log, "OpenOCD log : RISC-V core examined")
    out = run("monitor scan_chain")
    s.check(("0x%08x" % IDCODE) in out.lower(), "scan_chain : IDCODE 0x%08X" % IDCODE)
    dmstatus = monitor_value("riscv dmi_read 0x11")
    s.check(dmstatus & 0xF == 3, "dmstatus.version = 3 (Debug Spec 1.0) : dmstatus = 0x%08X" % dmstatus)
    s.check((dmstatus >> 7) & 1 == 1, "dmstatus.authenticated = 1")
    abstractcs = monitor_value("riscv dmi_read 0x16")
    s.check((abstractcs >> 24) & 0x1F == 2 and abstractcs & 0xF == 2,
            "abstractcs : progbufsize = 2, datacount = 2 (0x%08X)" % abstractcs)
    sbcs = monitor_value("riscv dmi_read 0x38")
    s.check(sbcs >> 29 == 1 and (sbcs >> 5) & 0x7F == 32,
            "sbcs : sbversion = 1, sbasize = 32 (0x%08X)" % sbcs)


def step_reset_halt(s):
    run("monitor reset halt")
    flush()
    s.check(pc() == RESET_ADDR, "pc = RESET_ADDR 0x%08X after reset halt : 0x%08X" % (RESET_ADDR, pc()))
    cause = dcsr_cause()
    s.check(cause in (CAUSE_HALTREQ, CAUSE_RESETHALTREQ), "dcsr.cause = haltreq (3) or resethaltreq (5) : %d" % cause)


def step_load(s):
    image_end = u32("(unsigned int)&_bss_start")
    words = (image_end - RESET_ADDR) // 4
    s.require(0 < words < 0x1000, "image size %d words" % words)
    for method in ("progbuf", "sysbus"):
        say("--- memory access : %s" % method)
        run("monitor riscv set_mem_access " + method)
        run("monitor mww 0x%08x 0xdeadbeef %d" % (RESET_ADDR, words))
        flush()
        out = run("compare-sections")
        s.check("MIS-MATCHED" in out, "%s : image overwritten (compare-sections mismatch)" % method)
        out = run("load")
        s.check("Transfer rate" in out or "Start address" in out, "%s : load" % method)
        out = run("compare-sections")
        s.check("MIS-MATCHED" not in out and "matched" in out, "%s : compare-sections matched" % method)
        flush()
        s.check(pc() == RESET_ADDR, "%s : pc = entry point 0x%08X : 0x%08X" % (method, RESET_ADDR, pc()))
    # Cross-check : written through the SBA, read through the program buffer
    run("monitor riscv set_mem_access progbuf")
    flush()
    out = run("compare-sections")
    s.check("MIS-MATCHED" not in out and "matched" in out, "sysbus load, progbuf compare-sections matched")
    run("monitor riscv set_mem_access progbuf sysbus abstract")


def step_registers(s):
    gprs = {"a0": 0x12345678, "t1": 0xCAFEF00D, "s1": 0x0BADC0DE, "a7": 0x80000001}
    for reg, value in gprs.items():
        run("set $%s = 0x%08x" % (reg, value))
    flush()
    for reg, value in gprs.items():
        got = u32("$" + reg)
        s.check(got == value, "GPR %s = 0x%08X : 0x%08X" % (reg, value, got))
    s.check(u32("$zero") == 0, "x0 = 0")
    # mhartid (read-only)
    s.check(u32("$mhartid") == HARTID, "mhartid = 0x%08X : 0x%08X" % (HARTID, u32("$mhartid")))
    # mscratch
    for value in (0xA5A5A5A5, 0x5A5A0001):
        run("set $mscratch = 0x%08x" % value)
        flush()
        got = u32("$mscratch")
        s.check(got == value, "mscratch = 0x%08X : 0x%08X" % (value, got))
    # dcsr : read, then write stepie (bit 11), cause / debugver / prv read-only
    dcsr = u32("$dcsr")
    s.check(dcsr >> 28 == 4, "dcsr.debugver = 4 (0x%08X)" % dcsr)
    s.check(dcsr & 3 == 3, "dcsr.prv = 3 (M)")
    run("set $dcsr = 0x%08x" % ((dcsr | (1 << 11)) & ~(7 << 6)))
    flush()
    got = u32("$dcsr")
    s.check((got >> 11) & 1 == 1, "dcsr.stepie written : 0x%08X" % got)
    s.check((got >> 6) & 7 == (dcsr >> 6) & 7, "dcsr.cause read-only")
    s.check(got >> 28 == 4 and got & 3 == 3, "dcsr.debugver / prv unchanged")
    run("set $dcsr = 0x%08x" % dcsr)
    flush()
    s.check(u32("$dcsr") == dcsr, "dcsr restored 0x%08X" % dcsr)
    # dpc = pc
    s.check(u32("$dpc") == pc(), "dpc = pc 0x%08X" % pc())
    target = u32("(unsigned int)&hb_func")
    run("set $dpc = 0x%08x" % target)
    flush()
    s.check(pc() == target, "dpc written : pc = 0x%08X : 0x%08X" % (target, pc()))
    run("set $pc = 0x%08x" % RESET_ADDR)
    flush()
    s.check(u32("$dpc") == RESET_ADDR, "pc written : dpc = 0x%08X : 0x%08X" % (RESET_ADDR, u32("$dpc")))


def step_break(s):
    bp = gdb.Breakpoint("bp_func")
    addr = bp_address(bp)
    for hit in range(2):
        cont()
        s.check(pc() == addr and func_name() == "bp_func",
                "break bp_func hit %d : pc = 0x%08X : 0x%08X (%s)" % (hit, addr, pc(), func_name()))
        s.check(bp.number in last_stop.get("breakpoints", []), "stop reported for breakpoint %d" % bp.number)
        s.check(dcsr_cause() == CAUSE_EBREAK, "dcsr.cause = ebreak (1) : %d" % dcsr_cause())
        s.check(u32("x") == hit and u32("counter") == hit, "argument x = counter = %d : %d / %d" % (hit, u32("x"), u32("counter")))
        s.check(u32("bp_hits") == hit, "bp_hits = %d : %d" % (hit, u32("bp_hits")))
    bp.delete()


def step_triggers(s):
    # Watchpoint : store trigger on "watched" (written after bp_func returns)
    run("watch watched")
    wp = gdb.breakpoints()[-1]
    s.check(wp.type == gdb.BP_HARDWARE_WATCHPOINT, "hardware watchpoint %d" % wp.number)
    for counter in (1, 2):
        expected = (counter + 1) * (counter + 2) // 2
        cont()
        s.check(wp.number in last_stop.get("breakpoints", []), "stop reported for watchpoint %d" % wp.number)
        s.check(func_name() == "main", "stopped in main (%s), pc 0x%08X" % (func_name(), pc()))
        s.check(u32("watched") == expected and u32("counter") == counter,
                "watched = %d (counter %d) : %d (counter %d)" % (expected, counter, u32("watched"), u32("counter")))
    wp.delete()
    # Hardware breakpoint : execute trigger
    run("hbreak hb_func")
    hb = gdb.breakpoints()[-1]
    s.check(hb.type == gdb.BP_HARDWARE_BREAKPOINT, "hardware breakpoint %d" % hb.number)
    addr = bp_address(hb)
    cont()
    s.check(pc() == addr and func_name() == "hb_func", "hbreak hb_func : pc = 0x%08X : 0x%08X" % (addr, pc()))
    s.check(hb.number in last_stop.get("breakpoints", []), "stop reported for breakpoint %d" % hb.number)
    s.check(dcsr_cause() == CAUSE_TRIGGER, "dcsr.cause = trigger (2) : %d" % dcsr_cause())
    total = LOOP_COUNT * (LOOP_COUNT + 1) // 2
    s.check(u32("counter") == LOOP_COUNT and u32("watched") == total and u32("bp_hits") == LOOP_COUNT,
            "loop done : counter %d, watched %d, bp_hits %d" % (u32("counter"), u32("watched"), u32("bp_hits")))
    s.check(u32("hb_hits") == 0, "hb_func not executed yet")
    hb.delete()


def step_stepping(s):
    base = u32("(unsigned int)&step_test")
    gdb.Breakpoint("*0x%08x" % base, temporary=True)
    cont()
    s.require(pc() == base, "tbreak *step_test : pc = 0x%08X : 0x%08X" % (base, pc()))
    s.check(u32("hb_hits") == 1, "hb_func executed")
    # (offset after the step, t0 after the step)
    for offset, t0 in ((4, 1), (8, 3), (12, 6), (20, 6), (24, 10)):
        run("stepi")
        s.check(pc() == base + offset and u32("$t0") == t0,
                "stepi : pc = step_test+%d, t0 = %d : pc = step_test+%d, t0 = %d"
                % (offset, t0, pc() - base, u32("$t0")))
    # GDB 15 + OpenOCD 0.12 : OpenOCD does not report vContSupported, so GDB
    # single-steps RISC-V with a breakpoint on the next instruction (Z0 +
    # vCont;c, software single step : dcsr.cause = ebreak). Accept also the
    # step of the target (dcsr.cause = step) for other versions.
    cause = dcsr_cause()
    s.check(cause in (CAUSE_EBREAK, CAUSE_STEP), "stepi : dcsr.cause = %d (%s)"
            % (cause, {CAUSE_EBREAK: "GDB software single step", CAUSE_STEP: "hardware step"}.get(cause, "?")))
    # Hardware single step of the hart (dcsr.step), requested to OpenOCD
    ra = u32("$ra")
    run("monitor step")
    flush()
    s.check(pc() == base + 28 and u32("$a0") == 10,
            "monitor step : pc = step_test+28, a0 = 10 : pc = step_test+%d, a0 = %d" % (pc() - base, u32("$a0")))
    s.check(dcsr_cause() == CAUSE_STEP, "monitor step : dcsr.cause = step (4) : %d" % dcsr_cause())
    run("monitor step")
    flush()
    s.check(pc() == ra and func_name() == "main", "monitor step (ret) : pc = ra 0x%08X : 0x%08X (%s)" % (ra, pc(), func_name()))
    s.check(dcsr_cause() == CAUSE_STEP, "monitor step : dcsr.cause = step (4) : %d" % dcsr_cause())
    # next : three consecutive lines in next_test
    gdb.Breakpoint("next_test", temporary=True)
    cont()
    s.require(func_name() == "next_test", "tbreak next_test (%s)" % func_name())
    line0 = gdb.selected_frame().find_sal().line
    s.check(u32("next_calls") == 0, "next_calls = 0")
    for n in (1, 2):
        run("next")
        sal = gdb.selected_frame().find_sal()
        s.check(func_name() == "next_test" and sal.line == line0 + n and pc() == (sal.pc & 0xFFFFFFFF),
                "next : line %d, pc 0x%08X at the start of the line : line %d, pc 0x%08X (%s)"
                % (line0 + n, sal.pc & 0xFFFFFFFF, sal.line, pc(), func_name()))
        s.check(u32("next_calls") == n, "next_calls = %d : %d" % (n, u32("next_calls")))


def step_memory(s):
    words = read_words("&pattern", 4)
    s.check(words == PATTERN, "x/4wx &pattern : %s" % " ".join("0x%08X" % w for w in words))
    s.check(u32("limit") == 4, "limit = 4")
    run("set var limit = 3")
    run("set var pattern[1] = 0x01020304")
    run("set var *(unsigned char *)&pattern[0] = 0x99")
    run("set var *(unsigned short *)&pattern[2] = 0xbeef")
    run("set {unsigned int}&pattern[3] = 0x5a5a5a5a")
    flush()
    expected = [0x11111199, 0x01020304, 0x3333BEEF, 0x5A5A5A5A]
    words = read_words("&pattern", 4)
    s.check(words == expected, "x/4wx &pattern after set var : %s" % " ".join("0x%08X" % w for w in words))
    out = run("x/4bx &pattern")
    s.check(re.search(r"0x99\s+0x11\s+0x11\s+0x11", out) is not None, "x/4bx &pattern : 0x99 0x11 0x11 0x11")
    s.check(u32("limit") == 3, "limit = 3")
    # Same memory read through the System Bus Access
    run("monitor riscv set_mem_access sysbus")
    flush()
    words = read_words("&pattern", 4)
    s.check(words == expected, "sysbus x/4wx &pattern : %s" % " ".join("0x%08X" % w for w in words))
    run("monitor riscv set_mem_access progbuf sysbus abstract")
    flush()


def step_end(s):
    expected = (0x11111199 + 0x01020304 + 0x3333BEEF) & 0xFFFFFFFF
    bp = gdb.Breakpoint("end_marker")
    addr = bp_address(bp)
    cont()
    s.check(pc() == addr and func_name() == "end_marker", "break end_marker : pc = 0x%08X : 0x%08X" % (addr, pc()))
    s.check(u32("r") == expected and u32("result") == expected,
            "result = pattern[0] + pattern[1] + pattern[2] (limit 3) = 0x%08X : r 0x%08X, result 0x%08X"
            % (expected, u32("r"), u32("result")))
    s.check(u32("next_calls") == 12, "next_calls = 12 : %d" % u32("next_calls"))
    bp.delete()
    run("next")
    s.check(u32("finished") == expected, "finished = 0x%08X : 0x%08X" % (expected, u32("finished")))


STEPS = [
    ("1 connection", step_connect),
    ("2 reset halt", step_reset_halt),
    ("3 load", step_load),
    ("4 registers", step_registers),
    ("5 break", step_break),
    ("6 watch / hbreak", step_triggers),
    ("7 stepi / next", step_stepping),
    ("8 memory", step_memory),
    ("9 end marker", step_end),
]


def main():
    connected = False
    for name, func in STEPS:
        with Step(name) as s:
            if name.startswith("1") or connected:
                func(s)
            else:
                s.check(False, "not connected")
        if name.startswith("1"):
            connected = results[-1][1] == "PASS"

    passed = all(v == "PASS" for _, v in results) and len(results) == len(STEPS)
    say("")
    say("=" * 72)
    for name, verdict in results:
        say("  %-20s %s" % (name, verdict))
    say("SCENARIO: %s (%.1f s)" % ("PASS" if passed else "FAIL", time.time() - T0))
    with open(VERDICT, "w") as f:
        for name, verdict in results:
            f.write("%s : %s\n" % (name, verdict))
        f.write("SCENARIO: %s\n" % ("PASS" if passed else "FAIL"))
    try:
        gdb.execute("delete", to_string=True)
        run("detach")
    except gdb.error as e:
        say("detach : %s" % e)


main()
