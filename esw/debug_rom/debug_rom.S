#-----------------------------------------------------------------------------
# Title      : debug_rom.S
# Project    : WardRV
#-----------------------------------------------------------------------------
# File       : debug_rom.S
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Debug ROM of the WardRV Debug Module (execution-based,
#              RISC-V Debug Spec 1.0). Mapped at 0xFFFF_F800 (debug memory,
#              visible by the hart in debug mode only). Every access uses a
#              x0-relative address (12-bit immediate between -2048 and -1),
#              so no register is needed to reach the debug memory.
#
#              0x000 : halt entry (debug mode entry, ebreak in debug mode)
#              0x008 : exception entry (exception in debug mode)
#
#              Parking loop : save s0 in dscratch0, write the hartid to
#              HALTED, poll FLAGS.
#              - go     (FLAGS bit 0) : write GOING, restore s0, jump to
#                                       WHERETO (abstract command / progbuf)
#              - resume (FLAGS bit 1) : write RESUMING, restore s0, dret
#              - exception            : write EXCEPTION, then ebreak (back
#                                       to the halt entry)
#
#              Assembled by gen_rom.py (self-contained, no toolchain needed)
#              into hdl/debug/WardRV_dbg_rom_pkg.vhd. "make check" also
#              assembles this file with the GNU toolchain and compares.
#              Supported syntax (gen_rom.py) : .equ, labels, j, nop, csrr,
#              csrw, sw, lw, andi, bnez, jalr, dret, ebreak.
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-09  1.0      mrosiere Created
#-----------------------------------------------------------------------------

        # Debug memory map (offsets from 0xFFFF_F800, x0-relative immediates)
        .equ DBG_BASE,     0x800
        .equ HALTED,       0x100 - DBG_BASE
        .equ GOING,        0x104 - DBG_BASE
        .equ RESUMING,     0x108 - DBG_BASE
        .equ EXCEPTION,    0x10C - DBG_BASE
        .equ WHERETO,      0x200 - DBG_BASE
        .equ FLAGS,        0x400 - DBG_BASE

        .equ FLAG_GO,      0x1
        .equ FLAG_RESUME,  0x2

        # CSRs
        .equ CSR_MHARTID,  0xF14
        .equ CSR_DSCRATCH0,0x7B2

        .section .text
        .globl  _start
_start:
_halt:                                  # 0x000 : halt entry
        j       _entry
        nop
_exception:                             # 0x008 : exception entry
        j       _exc

_entry:
        csrw    CSR_DSCRATCH0, s0       # save s0
        csrr    s0, CSR_MHARTID
        sw      s0, HALTED(zero)        # hart halted

_poll:
        lw      s0, FLAGS(zero)
        andi    s0, s0, FLAG_GO
        bnez    s0, _go
        lw      s0, FLAGS(zero)
        andi    s0, s0, FLAG_RESUME
        bnez    s0, _resume
        j       _poll

_go:
        sw      zero, GOING(zero)       # command acknowledged
        csrr    s0, CSR_DSCRATCH0       # restore s0
        jalr    zero, WHERETO(zero)     # abstract command / program buffer

_resume:
        sw      zero, RESUMING(zero)    # resume acknowledged
        csrr    s0, CSR_DSCRATCH0       # restore s0
        dret

_exc:
        sw      zero, EXCEPTION(zero)   # exception in debug mode
        ebreak                          # back to the halt entry
