#!/usr/bin/env python3
#-----------------------------------------------------------------------------
# Title      : gen_rom.py
# Project    : WardRV
#-----------------------------------------------------------------------------
# File       : gen_rom.py
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Generate the debug ROM package hdl/debug/WardRV_dbg_rom_pkg.vhd
#              from debug_rom.S, without RISC-V toolchain : a tiny two-pass
#              RV32I / Zicsr assembler supporting the subset used by the ROM
#              (.equ, labels, j, nop, csrr, csrw, sw, lw, andi, bnez, jalr,
#              dret, ebreak). Also writes debug_rom.hex and debug_rom.lst.
#
#              Usage : python3 gen_rom.py        (in esw/debug_rom)
#                      make                      (same)
#                      make check                (compare with GNU as)
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

ROM_BASE  = 0xFFFFF800
ROM_WORDS = 64           # 0x000 - 0x0FF of the debug memory
MASK      = 0xFFFFFFFF

REGS = {f"x{i}": i for i in range(32)}
REGS.update({"zero": 0, "ra": 1, "sp": 2, "gp": 3, "tp": 4, "t0": 5, "t1": 6, "t2": 7,
             "s0": 8, "fp": 8, "s1": 9, "a0": 10, "a1": 11, "a2": 12, "a3": 13, "a4": 14,
             "a5": 15, "a6": 16, "a7": 17, "s2": 18, "s3": 19, "s4": 20, "s5": 21,
             "s6": 22, "s7": 23, "s8": 24, "s9": 25, "s10": 26, "s11": 27, "t3": 28,
             "t4": 29, "t5": 30, "t6": 31})

#-----------------------------------------------------------------------------
# Encoders
#-----------------------------------------------------------------------------
def i_type(imm, rs1, f3, rd, op):
    assert -2048 <= imm < 4096, f"I immediate out of range : {imm}"
    return ((imm & 0xFFF) << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | op

def s_type(imm, rs2, rs1, f3, op):
    assert -2048 <= imm < 2048, f"S immediate out of range : {imm}"
    imm &= 0xFFF
    return ((imm >> 5) << 25) | (rs2 << 20) | (rs1 << 15) | (f3 << 12) | ((imm & 0x1F) << 7) | op

def b_type(imm, rs2, rs1, f3, op=0x63):
    assert imm % 2 == 0 and -4096 <= imm < 4096, f"B offset out of range : {imm}"
    imm &= 0x1FFF
    return (((imm >> 12) & 1) << 31) | (((imm >> 5) & 0x3F) << 25) | (rs2 << 20) | (rs1 << 15) | \
           (f3 << 12) | (((imm >> 1) & 0xF) << 8) | (((imm >> 11) & 1) << 7) | op

def j_type(imm, rd, op=0x6F):
    assert imm % 2 == 0 and -(1 << 20) <= imm < (1 << 20), f"J offset out of range : {imm}"
    imm &= 0x1FFFFF
    return (((imm >> 20) & 1) << 31) | (((imm >> 1) & 0x3FF) << 21) | (((imm >> 11) & 1) << 20) | \
           (((imm >> 12) & 0xFF) << 12) | (rd << 7) | op

#-----------------------------------------------------------------------------
# Assembler
#-----------------------------------------------------------------------------
class Asm:
    def __init__(self, text):
        self.equ    = {}
        self.labels = {}
        self.insts  = []   # (offset, mnemonic, operands, source line)
        self.parse(text)

    def value(self, expr):
        expr = expr.strip()
        names = dict(self.equ)
        names.update(self.labels)
        if not re.fullmatch(r"[0-9A-Za-z_+\-\s()xX]+", expr):
            raise ValueError(f"bad expression '{expr}'")
        return int(eval(expr, {"__builtins__": {}}, names))

    def reg(self, name):
        name = name.strip()
        if name not in REGS:
            raise ValueError(f"bad register '{name}'")
        return REGS[name]

    def mem(self, operand):
        m = re.fullmatch(r"\s*(.+)\(\s*(\w+)\s*\)\s*", operand)
        if not m:
            raise ValueError(f"bad memory operand '{operand}'")
        return self.value(m.group(1)), self.reg(m.group(2))

    def parse(self, text):
        offset = 0
        for line in text.splitlines():
            src  = line.rstrip()
            line = line.split("#")[0].strip()
            while True:
                m = re.match(r"^(\w+):\s*(.*)$", line)
                if not m:
                    break
                self.labels[m.group(1)] = offset
                line = m.group(2).strip()
            if not line:
                continue
            if line.startswith("."):
                m = re.match(r"^\.equ\s+(\w+)\s*,\s*(.+)$", line)
                if m:
                    self.equ[m.group(1)] = self.value(m.group(2))
                continue     # other directives (.section, .globl) ignored
            parts = line.split(None, 1)
            mnem  = parts[0]
            ops   = [o.strip() for o in parts[1].split(",")] if len(parts) > 1 else []
            self.insts.append((offset, mnem, ops, src.strip()))
            offset += 4

    def encode(self, offset, mnem, ops):
        if mnem == "nop":
            return i_type(0, 0, 0, 0, 0x13)
        if mnem == "ebreak":
            return 0x00100073
        if mnem == "dret":
            return 0x7B200073
        if mnem == "j":
            return j_type(self.value(ops[0]) - offset, 0)
        if mnem == "csrr":
            return i_type(self.value(ops[1]), 0, 2, self.reg(ops[0]), 0x73)          # csrrs rd, csr, x0
        if mnem == "csrw":
            return i_type(self.value(ops[0]), self.reg(ops[1]), 1, 0, 0x73)          # csrrw x0, csr, rs
        if mnem == "sw":
            imm, rs1 = self.mem(ops[1])
            return s_type(imm, self.reg(ops[0]), rs1, 2, 0x23)
        if mnem == "lw":
            imm, rs1 = self.mem(ops[1])
            return i_type(imm, rs1, 2, self.reg(ops[0]), 0x03)
        if mnem == "andi":
            return i_type(self.value(ops[2]), self.reg(ops[1]), 7, self.reg(ops[0]), 0x13)
        if mnem == "bnez":
            return b_type(self.value(ops[1]) - offset, 0, self.reg(ops[0]), 1)       # bne rs, x0
        if mnem == "jalr":
            imm, rs1 = self.mem(ops[1])
            return i_type(imm, rs1, 0, self.reg(ops[0]), 0x67)
        raise ValueError(f"unsupported instruction '{mnem}'")

    def words(self):
        return [(off, self.encode(off, mnem, ops) & MASK, src) for off, mnem, ops, src in self.insts]

#-----------------------------------------------------------------------------
# Outputs
#-----------------------------------------------------------------------------
HEADER = """-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_rom_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Debug ROM of the WardRV Debug Module (debug memory 0x000 - 0x0FF, mapped
-- at 0xFFFF_F800). GENERATED by esw/debug_rom/gen_rom.py from
-- esw/debug_rom/debug_rom.S : do not edit.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.WardRV_debug_pkg.all;

package WardRV_dbg_rom_pkg is

"""

def main():
    here   = os.path.dirname(os.path.abspath(__file__))
    src    = os.path.join(here, "debug_rom.S")
    pkg    = os.path.join(here, "..", "..", "hdl", "debug", "WardRV_dbg_rom_pkg.vhd")
    asm    = Asm(open(src).read())
    words  = asm.words()
    if len(words) > ROM_WORDS:
        sys.exit(f"debug ROM too large : {len(words)} words (max {ROM_WORDS})")

    with open(os.path.join(here, "debug_rom.hex"), "w") as f:
        for _, w, _ in words:
            f.write(f"{w:08x}\n")

    with open(os.path.join(here, "debug_rom.lst"), "w") as f:
        f.write("# Generated by gen_rom.py - do not edit\n")
        for off, w, s in words:
            f.write(f"{ROM_BASE + off:08x}:\t{w:08x}\t{s}\n")

    with open(pkg, "w") as f:
        f.write(HEADER)
        f.write(f"  constant DBG_ROM_LENGTH : natural := {len(words)};\n\n")
        f.write("  constant DBG_ROM : dbg_words_t(0 to DBG_ROM_LENGTH-1) := (\n")
        for i, (off, w, s) in enumerate(words):
            sep = "," if i < len(words) - 1 else " "
            f.write(f"    {i:2d} => x\"{w:08X}\"{sep} -- 0x{off:03X} : {s}\n")
        f.write("  );\n\n")
        f.write("end package WardRV_dbg_rom_pkg;\n")

    print(f"debug ROM : {len(words)} words, labels : " +
          ", ".join(f"{k}=0x{v:03x}" for k, v in asm.labels.items()))

if __name__ == "__main__":
    main()
