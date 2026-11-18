// ----------------------------------------------------------------------------
// Firmware of the debug system test (sim/tb_WardRV_debug.vhd)
//
// A loop increments a counter (t0) and stores it in memory. When the flag
// word is not 0 (written by the debugger), it executes an ebreak (software
// breakpoint), counts it in s1 and clears the flag. The testbench uses the
// addresses of debug_fw.lst (loop, store, load, ebreak, handler).
//
// Trap handler (machine external interrupt enabled) :
//   breakpoint exception (mcause 3, EBREAK_EXCEPTION) : count in s3, return
//                                                        after the ebreak
//   external interrupt                               : count in s2, meip_i
//                                                        cleared
//
// Build : make (RISC-V toolchain), debug_fw.hex / debug_fw.lst committed
// ----------------------------------------------------------------------------

.equ DATA,          0x80001000    // counter (+0), flag (+4)
.equ MEIP_ADDR,     0x80300000    // tb_WardRV_debug : meip_i <= wdata(0)
.equ MIE_MEIE,      0x800         // mie.MEIE
.equ MSTATUS_MIE,   0x8           // mstatus.MIE
.equ MCAUSE_BKPT,   3

.section .text
.global _start
_start:
    la    t1, handler
    csrw  mtvec, t1
    li    t1, MIE_MEIE
    csrw  mie, t1
    csrsi mstatus, MSTATUS_MIE
    li    a0, DATA
    li    a1, MEIP_ADDR
    li    t0, 0                   // counter
    li    s1, 0                   // ebreak executed
    li    s2, 0                   // interrupts
    li    s3, 0                   // breakpoint exceptions
loop:
    addi  t0, t0, 1
    sw    t0, 0(a0)               // counter
    lw    s0, 4(a0)               // flag
    beqz  s0, loop
    ebreak                        // software breakpoint
    addi  s1, s1, 1
    sw    zero, 4(a0)             // flag cleared
    j     loop

handler:
    csrr  t2, mcause
    li    t3, MCAUSE_BKPT
    bne   t2, t3, irq
    csrr  t2, mepc                // breakpoint : return after the ebreak
    addi  t2, t2, 4
    csrw  mepc, t2
    addi  s3, s3, 1
    mret
irq:
    addi  s2, s2, 1
    sw    zero, 0(a1)             // meip_i = 0
    mret
