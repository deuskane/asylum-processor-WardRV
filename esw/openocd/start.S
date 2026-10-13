//-----------------------------------------------------------------------------
// Title      : WardRV OpenOCD / GDB co-simulation firmware : startup
// Project    : Asylum
//-----------------------------------------------------------------------------
// File       : start.S
// Author     : Mathieu Rosiere
//-----------------------------------------------------------------------------
// Description: Reset vector (stack, .bss cleared : GDB load does not write
//              it), call main. step_test : straight-line code and a jump,
//              single-stepped by sim/openocd/scenario.py (stepi) : the PC
//              and t0 are checked after each instruction.
//-----------------------------------------------------------------------------
// Copyright (c) 2026
//-----------------------------------------------------------------------------
// Revisions  :
// Date        Version  Author   Description
// 2026-10-09  1.0      mrosiere Created
//-----------------------------------------------------------------------------

    .section .text.init
    .global _start
    .type   _start, @function
_start:
    la    sp, _stack_top
    la    t0, _bss_start
    la    t1, _bss_end
1:  bgeu  t0, t1, 2f
    sw    zero, 0(t0)
    addi  t0, t0, 4
    j     1b
2:  call  main
3:  j     3b
    .size _start, . - _start

    .section .text
    .global step_test
    .type   step_test, @function
step_test:
    addi  t0, zero, 1           // step_test + 0  : t0 = 1
    addi  t0, t0, 2             // step_test + 4  : t0 = 3
    addi  t0, t0, 3             // step_test + 8  : t0 = 6
    j     step_test_target      // step_test + 12
    addi  t0, t0, 100           // step_test + 16 : never executed
    .global step_test_target
step_test_target:
    addi  t0, t0, 4             // step_test + 20 : t0 = 10
    mv    a0, t0                // step_test + 24 : a0 = 10
    ret                         // step_test + 28
    .size step_test, . - step_test
