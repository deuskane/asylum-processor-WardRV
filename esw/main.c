//-------------------------------------------------------------------------------
// Title      : WardRV
// Project    : 
//-----------------------------------------------------------------------------
// File       : main.c
// Author     : Mathieu Rosiere
//-----------------------------------------------------------------------------
// Description: 
//-----------------------------------------------------------------------------
// Copyright (c) 2026
//-----------------------------------------------------------------------------
// Revisions  :
// Date        Version  Author   Description
// 2026-02-01  1.0      mrosiere Created
//-----------------------------------------------------------------------------

#include <stdint.h>  
#define TOHOST_ADDR   0x80001000
#define FROMHOST_ADDR 0x80001008

static inline void  test_pass() {
    *(volatile uint32_t*)TOHOST_ADDR = 1; // Signal success to the simulation host
}

static inline void test_fail() {
    *(volatile uint32_t*)TOHOST_ADDR = 0; // Signal failure to the simulation host
}

void check(uint32_t result, uint32_t expected) {
    if (result != expected) {
        test_fail();
        while(1);
    }
}

uint32_t main() {
    // Test RV32I Arithmetic and Logical instructions using registers
    volatile uint32_t a = 0x12345678;
    volatile uint32_t b = 0x00000001;
    volatile uint32_t c = 0x12345678;
    
    check(a + b, 0x12345679); // Verify ADD (Addition)
    check(a - b, 0x12345677); // Verify SUB (Subtraction)
    check(a ^ b, 0x12345679); // Verify XOR (Exclusive OR)
    check(a | b, 0x12345679); // Verify OR (Logical OR)
    check(a & b, 0x00000000); // Verify AND (Logical AND)
    check(a & c, 0x12345678); // Verify AND with identical values

    // Test RV32I Shift instructions
    volatile uint32_t s = 0x80000000;
    check(s >> 1,  0x40000000); // Verify SRL (Shift Right Logical)
    check((int32_t)s >> 1, 0xC0000000); // Verify SRA (Shift Right Arithmetic - sign extension)
    check(b << 4,  0x00000010); // Verify SLL (Shift Left Logical)

    // Test RV32I Comparison instructions
    volatile int32_t neg = -10;
    volatile int32_t pos = 10;
    check(neg < pos, 1); // Verify SLT (Set Less Than - signed)
    check((uint32_t)neg < (uint32_t)pos, 0); // Verify SLTU (Set Less Than Unsigned)

    // Test RV32I instructions with Immediate values
    check(a + 0x123, 0x1234579B); // Verify ADDI (Add Immediate)
    check(b << 2, 0x4);           // Verify SLLI (Shift Left Logical Immediate)

    // Test RV32I Memory Access (Load and Store) instructions
    volatile uint32_t mem_val;
    mem_val = 0xDEADBEEF;
    check(mem_val, 0xDEADBEEF);   // Verify LW (Load Word) and SW (Store Word)

    volatile uint16_t mem_half = 0xABCD;
    check(mem_half, 0xABCD);      // Verify LHU (Load Halfword Unsigned) and SH (Store Halfword)

    volatile uint8_t mem_byte = 0x7F;
    check(mem_byte, 0x7F);        // Verify LBU (Load Byte Unsigned) and SB (Store Byte)

    // Test RV32I Conditional Branching instructions
    uint32_t branch_res = 0;
    if (a != b) branch_res = 1;   // Verify BNE (Branch Not Equal)
    else        branch_res = 0;
    check(branch_res, 1);

    if (a == c) branch_res = 2;   // Verify BEQ (Branch Equal)
    check(branch_res, 2);

    if ((int32_t)neg < (int32_t)pos) branch_res = 3; // Verify BLT (Branch Less Than - signed)
    check(branch_res, 3);

    if ((uint32_t)a >= (uint32_t)b) branch_res = 4;  // Verify BGEU (Branch Greater or Equal Unsigned)
    check(branch_res, 4);

    // Test RV32I Unconditional Jump instructions
    int jump_check = 0;
    goto jump_target;
    jump_check = 1;
jump_target:                      // Target for the jump
    check(jump_check, 0);         // Verify JAL (Jump and Link) via C goto

    // Test RV32I Upper Immediate instructions
    uint32_t lui_val = 0x12345000;
    check(lui_val, 0x12345000);   // Verify LUI (Load Upper Immediate)

    // Test AUIPC (Add Upper Immediate to PC)
    uint32_t auipc_val;
    asm volatile ("auipc %0, 0" : "=r"(auipc_val));

    // Test BLT (Branch Less Than) using assembly
    uint32_t blt_asm_res = 0;
    asm volatile (
        "li t0, -10\n"
        "li t1, 10\n"
        "li %0, 0\n"
        "blt t0, t1, 1f\n"
        "li %0, 2\n"
        "1:\n"
        "addi %0, %0, 1\n"
        : "=r"(blt_asm_res) :: "t0", "t1"
    );
    check(blt_asm_res, 1);

    // Test BGEU (Branch Greater Equal Unsigned) using assembly
    uint32_t bgeu_asm_res = 1;
    asm volatile (
        "li t0, 20\n"
        "li t1, 10\n"
        "bgeu t0, t1, 1f\n"
        "li %0, 2\n"
        "1:\n"
        "addi %0, %0, -1\n"
        : "=r"(bgeu_asm_res) :: "t0", "t1"
    );
    check(bgeu_asm_res, 0);

    // Test LB (Load Byte Signed) with sign extension
    volatile int8_t s_byte = -10;
    volatile int8_t *p_s_byte = &s_byte;
    check(*p_s_byte, 0xFFFFFFF6);

    // Test LH (Load Halfword Signed) with sign extension
    volatile int16_t s_half = -2000;
    volatile int16_t *p_s_half = &s_half;
    check(*p_s_half, 0xFFFFF830);

    // Test SLTI (Set Less Than Immediate - signed)
    volatile int32_t slti_src = -20;
    int32_t slti_res;
    asm volatile ("slti %0, %1, 10" : "=r"(slti_res) : "r"(slti_src));
    check(slti_res, 1);

    // Test SLTIU (Set Less Than Immediate Unsigned)
    volatile uint32_t sltiu_src = 20;
    int32_t sltiu_res;
    asm volatile ("sltiu %0, %1, 30" : "=r"(sltiu_res) : "r"(sltiu_src));
    check(sltiu_res, 1);

    // Test XORI and ORI (Logical operations with immediates)
    check(a ^ 0x123, 0x1234575B); // Verify XORI (Exclusive OR Immediate)
    check(a | 0x123, 0x1234577B); // Verify ORI (Logical OR Immediate)

    // SLL, SRL, and SRA (Shift operations with register-based shift amount)
    volatile uint32_t shamt = 3;
    check(b << shamt, 8); // Verify SLL (Shift Left Logical)
    check(s >> shamt, 0x10000000); // Verify SRL (Shift Right Logical)
    volatile int32_t s_signed = 0x80000000;
    check(s_signed >> shamt, 0xF0000000); // Verify SRA (Shift Right Arithmetic)

    test_pass();

    return 0;
}
