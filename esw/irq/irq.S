// ----------------------------------------------------------------------------
// Firmware of the interrupt test (target sim_irq, sim/tb_WardRV.vhd)
//
// The testbench drives meip_i : a store at MEIP_ADDR sets meip_i to the bit 0
// of the stored data at the next instruction fetch, so the interrupt is
// pending during the instruction that follows the store.
//
// Checks (tohost = 1 : pass, else the number of the failing check) :
//   2 : csrci mstatus, MIE with the interrupt pending : no trap
//   3 : csrsi mstatus, MIE with the interrupt pending : trap right after it
//       (mepc, mcause, mstatus.MPIE = 1)
//   4 : csrc  mie, MEIE with the interrupt pending : no trap
//   5 : csrs  mie, MEIE with the interrupt pending : trap right after it
//
// Build : make (needs a RISC-V toolchain), irq_fw.hex / irq_fw.lst committed
// ----------------------------------------------------------------------------

.equ MEIP_ADDR,     0x80300000    // tb_WardRV : meip_i <= wdata(0) at the next fetch
.equ TOHOST_ADDR,   0x80200000    // tb_WardRV : 1 = pass, other = fail
.equ MSTATUS_MIE,   0x8           // mstatus.MIE  (bit 3)
.equ MIE_MEIE,      0x800         // mie.MEIE     (bit 11)
.equ MSTATUS_TRAP,  0x1880        // mstatus in the handler    : MPP = 3, MPIE = 1, MIE = 0
.equ MSTATUS_MRET,  0x1888        // mstatus after mret        : MPP = 3, MPIE = 1, MIE = 1
.equ MCAUSE_MEI,    0x8000000B    // mcause : machine external interrupt

// Registers
//   a0 : MEIP_ADDR             a1 : TOHOST_ADDR         a3 : 1
//   a4 : MIE_MEIE              a7 : MSTATUS_TRAP        s2 : MCAUSE_MEI
//   s4 : number of traps       s5 : marker, incremented after each trap return
//   s6, s7, s8, s9 : mepc, mstatus, mcause and marker saved by the handler
//   t6 : number of the current check (written in tohost on a failure)

.section .text
.global _start
_start:
    li    a0, MEIP_ADDR
    li    a1, TOHOST_ADDR
    li    a3, 1
    li    a4, MIE_MEIE
    li    a7, MSTATUS_TRAP
    li    s2, MCAUSE_MEI
    li    s4, 0
    li    s5, 0
    la    a2, handler
    csrw  mtvec, a2
    csrw  mie, a4                 // MEIE = 1
    csrsi mstatus, MSTATUS_MIE    // MIE  = 1, no interrupt pending

    // 2 : interrupt pending during the instruction that clears mstatus.MIE
    li    t6, 2
    sw    a3, 0(a0)               // meip_i = 1 during the next instruction
    csrci mstatus, MSTATUS_MIE    // MIE = 0 : no trap
    nop
    nop
    bnez  s4, fail

    // 3 : mstatus.MIE set with the interrupt pending : trap right after
    li    t6, 3
    csrsi mstatus, MSTATUS_MIE    // MIE = 1 : trap, mepc = ret3
ret3:
    addi  s5, s5, 1               // marker
    li    a5, 1
    bne   s4, a5, fail            // one trap
    bnez  s9, fail                // taken before the marker
    la    a6, ret3
    bne   s6, a6, fail            // mepc
    bne   s7, a7, fail            // mstatus in the handler
    bne   s8, s2, fail            // mcause
    csrr  s3, mstatus
    li    a6, MSTATUS_MRET
    bne   s3, a6, fail            // mstatus after mret

    // 4 : interrupt pending during the instruction that clears mie.MEIE
    li    t6, 4
    sw    a3, 0(a0)               // meip_i = 1 during the next instruction
    csrc  mie, a4                 // MEIE = 0 : no trap
    nop
    nop
    li    a5, 1
    bne   s4, a5, fail

    // 5 : mie.MEIE set with the interrupt pending : trap right after
    li    t6, 5
    csrs  mie, a4                 // MEIE = 1 : trap, mepc = ret5
ret5:
    addi  s5, s5, 1               // marker
    li    a5, 2
    bne   s4, a5, fail            // second trap
    li    a5, 1
    bne   s9, a5, fail            // taken before the marker
    la    a6, ret5
    bne   s6, a6, fail            // mepc
    bne   s7, a7, fail            // mstatus in the handler
    bne   s8, s2, fail            // mcause

pass:
    sw    a3, 0(a1)               // tohost = 1 : pass
1:  j     1b

fail:
    sw    t6, 0(a1)               // tohost = number of the failing check
1:  j     1b

// Trap handler : count the trap, save mepc, mstatus, mcause and the marker,
// clear meip_i
handler:
    addi  s4, s4, 1
    csrr  s6, mepc
    csrr  s7, mstatus
    csrr  s8, mcause
    mv    s9, s5
    sw    zero, 0(a0)             // meip_i = 0
    mret
