// ----------------------------------------------------------------------------
// Debug ROM of the WardRV Debug Module (Debug Spec 1.0, execution-based debug)
//
// Offset 0x000 of the debug memory (0xFFFF_F800), executed by the hart in
// debug mode : at the debug mode entry and after each ebreak in debug mode.
// The debug memory is reached with x0-relative accesses (immediate = offset
// - 0x800), so no register is needed for the addresses. The offsets are the
// ones of hdl/debug/WardRV_debug_pkg.vhd.
//
//   save s0 in dscratch0, tell the Debug Module the hart is halted (HALTED)
//   wait for the Debug Module :
//     GO     : restore s0, run the abstract command program (it ends with an
//              ebreak, or runs the program buffer, which ends with an ebreak :
//              back to the entry)
//     RESUME : acknowledge (RESUMING), restore s0, dret
//
// Build : make (RISC-V toolchain) -> debug_rom.hex, debug_rom.lst and
// hdl/debug/WardRV_dbg_rom_pkg.vhd, all committed
// ----------------------------------------------------------------------------

.equ HALTED,      0x100 - 0x800   // W : the hart is halted
.equ RESUMING,    0x104 - 0x800   // W : the hart resumes
.equ GO,          0x108 - 0x800   // R : 1 = execute the abstract command
.equ RESUME,      0x10C - 0x800   // R : 1 = resume
.equ ABSTRACT,    0x200 - 0x800   //     abstract command program

.section .text
.global _start
_start:
    csrw  dscratch0, s0
    sw    zero, HALTED(zero)
poll:
    lw    s0, GO(zero)
    bnez  s0, go
    lw    s0, RESUME(zero)
    bnez  s0, resume
    j     poll
go:
    csrr  s0, dscratch0
    jalr  zero, ABSTRACT(zero)
resume:
    sw    zero, RESUMING(zero)
    csrr  s0, dscratch0
    dret
