#!/usr/bin/env python3
#-----------------------------------------------------------------------------
# Title      : gen_irq_fw.py
# Project    : WardRV
#-----------------------------------------------------------------------------
# File       : gen_irq_fw.py
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Generate the firmware of the interrupt test (sim_irq,
#              sim/tb_WardRV.vhd) without RISC-V toolchain : writes
#              irq_fw.hex (one 32-bit word per line, loaded at 0x80000000)
#              and irq_fw.lst.
#
#              The testbench drives meip_i : a store at MEIP_ADDR sets
#              meip_i to wdata(0) at the next instruction fetch, so the
#              interrupt becomes pending during the instruction that follows
#              the store. The program checks that :
#                2 : csrci mstatus, MIE with the interrupt pending : no trap
#                3 : csrsi mstatus, MIE with the interrupt pending : trap
#                    right after it (mepc, mcause, mstatus.MPIE = 1)
#                4 : csrc  mie, MEIE with the interrupt pending : no trap
#                5 : csrs  mie, MEIE with the interrupt pending : trap
#                    right after it
#              It writes tohost = 1 when all the checks pass, else the
#              number of the failing check.
#
#              Usage : python3 gen_irq_fw.py   (in esw/irq)
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-17  1.0      mrosiere Created
#-----------------------------------------------------------------------------

import os

RESET_ADDR   = 0x80000000
TOHOST_ADDR  = 0x80200000   # tb_WardRV : 1 = pass, other = fail
MEIP_ADDR    = 0x80300000   # tb_WardRV : meip_i <= wdata(0) at the next fetch
MASK         = 0xFFFFFFFF

CSR_MSTATUS  = 0x300
CSR_MIE      = 0x304
CSR_MTVEC    = 0x305
CSR_MEPC     = 0x341
CSR_MCAUSE   = 0x342

#-----------------------------------------------------------------------------
# Encoding
#-----------------------------------------------------------------------------
def r_type(f7, rs2, rs1, f3, rd, op=0x33):
    return (f7 << 25) | (rs2 << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | op

def i_type(imm, rs1, f3, rd, op):
    assert -2048 <= imm < 2048
    return ((imm & 0xFFF) << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | op

def s_type(imm, rs2, rs1, f3, op=0x23):
    imm &= 0xFFF
    return ((imm >> 5) << 25) | (rs2 << 20) | (rs1 << 15) | (f3 << 12) | ((imm & 0x1F) << 7) | op

def b_type(imm, rs2, rs1, f3, op=0x63):
    assert -4096 <= imm < 4096 and imm % 2 == 0
    imm &= 0x1FFF
    return (((imm >> 12) & 1) << 31) | (((imm >> 5) & 0x3F) << 25) | (rs2 << 20) | (rs1 << 15) | \
           (f3 << 12) | (((imm >> 1) & 0xF) << 8) | (((imm >> 11) & 1) << 7) | op

def u_type(imm20, rd, op):
    return ((imm20 & 0xFFFFF) << 12) | (rd << 7) | op

def j_type(imm, rd, op=0x6F):
    imm &= 0x1FFFFF
    return (((imm >> 20) & 1) << 31) | (((imm >> 1) & 0x3FF) << 21) | (((imm >> 11) & 1) << 20) | \
           (((imm >> 12) & 0xFF) << 12) | (rd << 7) | op

def csr_type(csr, rs1, f3, rd):
    return (csr << 20) | (rs1 << 15) | (f3 << 12) | (rd << 7) | 0x73

#-----------------------------------------------------------------------------
# Program : list of (label, encoder(pc, labels), text), two passes
#-----------------------------------------------------------------------------
PROG   = []

def label(name):
    PROG.append((name, None, None))

def ins(enc, text):
    PROG.append((None, enc, text))

def hi_lo(value):
    lo = value & 0xFFF
    if lo >= 0x800:
        lo -= 0x1000
    return ((value - lo) >> 12) & 0xFFFFF, lo

def li(rd, value, text):
    hi, lo = hi_lo(value & MASK)
    ins(lambda pc, l: u_type(hi, rd, 0x37),             f"lui     x{rd}, 0x{hi:05x}   # {text}")
    ins(lambda pc, l: i_type(lo, rd, 0, rd, 0x13),      f"addi    x{rd}, x{rd}, {lo}")

def la(rd, name, text):
    ins(lambda pc, l: u_type(hi_lo(l[name])[0], rd, 0x37), f"lui     x{rd}, %hi({name}) # {text}")
    ins(lambda pc, l: i_type(hi_lo(l[name])[1], rd, 0, rd, 0x13), f"addi    x{rd}, x{rd}, %lo({name})")

def addi(rd, rs1, imm, text=""):
    ins(lambda pc, l: i_type(imm, rs1, 0, rd, 0x13), f"addi    x{rd}, x{rs1}, {imm}" + (f"   # {text}" if text else ""))

def sw(rs2, imm, rs1, text=""):
    ins(lambda pc, l: s_type(imm, rs2, rs1, 2), f"sw      x{rs2}, {imm}(x{rs1})" + (f"   # {text}" if text else ""))

def bne(rs1, rs2, name):
    ins(lambda pc, l: b_type(l[name] - pc, rs2, rs1, 1), f"bne     x{rs1}, x{rs2}, {name}")

def j(name):
    ins(lambda pc, l: j_type(l[name] - pc, 0), f"jal     x0, {name}")

def csrrw(rd, csr, rs1, text=""):
    ins(lambda pc, l: csr_type(csr, rs1, 1, rd), f"csrrw   x{rd}, 0x{csr:03x}, x{rs1}" + (f"   # {text}" if text else ""))

def csrrs(rd, csr, rs1, text=""):
    ins(lambda pc, l: csr_type(csr, rs1, 2, rd), f"csrrs   x{rd}, 0x{csr:03x}, x{rs1}" + (f"   # {text}" if text else ""))

def csrrc(rd, csr, rs1, text=""):
    ins(lambda pc, l: csr_type(csr, rs1, 3, rd), f"csrrc   x{rd}, 0x{csr:03x}, x{rs1}" + (f"   # {text}" if text else ""))

def csrrsi(rd, csr, uimm, text=""):
    ins(lambda pc, l: csr_type(csr, uimm, 6, rd), f"csrrsi  x{rd}, 0x{csr:03x}, {uimm}" + (f"   # {text}" if text else ""))

def csrrci(rd, csr, uimm, text=""):
    ins(lambda pc, l: csr_type(csr, uimm, 7, rd), f"csrrci  x{rd}, 0x{csr:03x}, {uimm}" + (f"   # {text}" if text else ""))

def nop():
    ins(lambda pc, l: 0x00000013, "nop")

def mret():
    ins(lambda pc, l: 0x30200073, "mret")

# Registers :
#   x10 MEIP_ADDR, x11 TOHOST_ADDR, x13 = 1, x14 = MEIE (0x800)
#   x20 interrupt count, x21 marker (incremented after each trap return)
#   handler : x22 mepc, x23 mstatus, x24 mcause, x25 marker
#   x31 number of the current check (tohost value on failure)

li(10, MEIP_ADDR,   "testbench meip_i register")
li(11, TOHOST_ADDR, "tohost")
addi(13, 0, 1)
li(14, 0x800,       "MEIE")
li(17, 0x00001880,  "mstatus in the handler : MPP = 3, MPIE = 1, MIE = 0")
li(18, 0x8000000B,  "mcause : machine external interrupt")
addi(20, 0, 0,      "interrupt count")
addi(21, 0, 0,      "marker")
la(12, "handler",   "trap handler")
csrrw(0, CSR_MTVEC, 12)
csrrw(0, CSR_MIE, 14, "MEIE = 1")
csrrsi(0, CSR_MSTATUS, 8, "MIE = 1, no interrupt pending")

# 2 : interrupt pending during the instruction that clears mstatus.MIE
addi(31, 0, 2)
sw(13, 0, 10,        "meip_i = 1 during the next instruction")
csrrci(0, CSR_MSTATUS, 8, "MIE = 0 : no trap")
nop()
nop()
bne(20, 0, "fail")

# 3 : mstatus.MIE set with the interrupt pending : trap right after
addi(31, 0, 3)
csrrsi(0, CSR_MSTATUS, 8, "MIE = 1 : trap, mepc = ret3")
label("ret3")
addi(21, 21, 1,      "marker")
addi(15, 0, 1)
bne(20, 15, "fail")
bne(25, 0, "fail")
la(16, "ret3", "mepc")
bne(22, 16, "fail")
bne(23, 17, "fail")
bne(24, 18, "fail")
csrrs(19, CSR_MSTATUS, 0)
li(16, 0x00001888,  "mstatus after mret : MPIE = 1, MIE = 1")
bne(19, 16, "fail")

# 4 : interrupt pending during the instruction that clears mie.MEIE
addi(31, 0, 4)
sw(13, 0, 10,        "meip_i = 1 during the next instruction")
csrrc(0, CSR_MIE, 14, "MEIE = 0 : no trap")
nop()
nop()
addi(15, 0, 1)
bne(20, 15, "fail")

# 5 : mie.MEIE set with the interrupt pending : trap right after
addi(31, 0, 5)
csrrs(0, CSR_MIE, 14, "MEIE = 1 : trap, mepc = ret5")
label("ret5")
addi(21, 21, 1,      "marker")
addi(15, 0, 2)
bne(20, 15, "fail")
addi(15, 0, 1)
bne(25, 15, "fail")
la(16, "ret5", "mepc")
bne(22, 16, "fail")
bne(23, 17, "fail")
bne(24, 18, "fail")

# Pass
sw(13, 0, 11,        "tohost = 1 : pass")
label("pass")
j("pass")

label("fail")
sw(31, 0, 11,        "tohost = number of the failing check")
label("fail_loop")
j("fail_loop")

# Trap handler
label("handler")
addi(20, 20, 1,      "interrupt count")
csrrs(22, CSR_MEPC, 0)
csrrs(23, CSR_MSTATUS, 0)
csrrs(24, CSR_MCAUSE, 0)
addi(25, 21, 0,      "marker")
sw(0, 0, 10,         "meip_i = 0")
mret()

#-----------------------------------------------------------------------------
# Main
#-----------------------------------------------------------------------------
def assemble():
    labels = {}
    pc     = RESET_ADDR
    for name, enc, _ in PROG:
        if enc is None:
            labels[name] = pc
        else:
            pc += 4
    words = []
    pc    = RESET_ADDR
    for name, enc, text in PROG:
        if enc is None:
            words.append((pc, None, name + ":"))
        else:
            words.append((pc, enc(pc, labels) & MASK, text))
            pc += 4
    return words

def main():
    here  = os.path.dirname(os.path.abspath(__file__))
    words = assemble()
    with open(os.path.join(here, "irq_fw.hex"), "w") as f:
        for pc, w, _ in words:
            if w is not None:
                f.write(f"{w:08x}\n")
    with open(os.path.join(here, "irq_fw.lst"), "w") as f:
        f.write("# Generated by gen_irq_fw.py - do not edit\n")
        for pc, w, t in words:
            if w is None:
                f.write(f"{pc:08x} <{t[:-1]}>:\n")
            else:
                f.write(f"{pc:08x}:\t{w:08x}\t{t}\n")
    print(f"{sum(w is not None for _, w, _ in words)} instructions")

if __name__ == "__main__":
    main()
