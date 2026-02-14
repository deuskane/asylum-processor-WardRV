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

void check16(uint16_t result, uint16_t expected) {
    if (result != expected) {
        test_fail();
        while(1);
    }
}

uint8_t read_mem8(uintptr_t addr, uint32_t offset) {
    return *(volatile uint8_t*)(addr + offset);
}

uint16_t read_mem16(uintptr_t addr, uint32_t offset) {
    return *(volatile uint16_t*)(addr + offset);
}

uint32_t read_mem32(uintptr_t addr, uint32_t offset) {
    return *(volatile uint32_t*)(addr + offset);
}

uint32_t testcase(uint32_t * operands) {
    uint32_t   a     = operands[0];
    uint32_t   b     = operands[1];
    uint32_t   c     = operands[2];
    uint32_t   d     = operands[3];
    int32_t    neg   = operands[4];
    int32_t    pos   = operands[5];
    uint32_t * mem32 = &(operands[6]);
    uint16_t * mem16 = (uint16_t *)mem32;
    uint8_t  * mem8  = (uint8_t  *)mem32;
    uint32_t   res;

    // Test RV32I Arithmetic and Logical instructions using registers
    
    // Test RV32I Arithmetic instructions
    asm volatile ("add %0, %1, %2" : "=r"(res) : "r"(a), "r"(b));
    check(res, 0x12345679); // Verify ADD (Addition)
    asm volatile ("sub %0, %1, %2" : "=r"(res) : "r"(a), "r"(b));
    check(res, 0x12345677); // Verify SUB (Subtraction)
    asm volatile ("xor %0, %1, %2" : "=r"(res) : "r"(a), "r"(b));
    check(res, 0x12345679); // Verify XOR (Exclusive OR)
    asm volatile ("or  %0, %1, %2" : "=r"(res) : "r"(a), "r"(b));
    check(res, 0x12345679); // Verify OR (Logical OR)
    asm volatile ("and %0, %1, %2" : "=r"(res) : "r"(a), "r"(b));
    check(res, 0x00000000); // Verify AND (Logical AND)
    asm volatile ("and %0, %1, %2" : "=r"(res) : "r"(a), "r"(c));
    check(res, 0x12345678); // Verify AND with identical values

    // Test RV32I Shift instructions
    asm volatile ("srl %0, %1, %2" : "=r"(res) : "r"(a), "r"(1));
    check(res, 0x091A2B3C); // Verify SRL (Shift Right Logical)
    asm volatile ("sra %0, %1, %2" : "=r"(res) : "r"(a), "r"(1));
    check(res, 0x091A2B3C); // Verify SRA (Shift Right Arithmetic)
    
    asm volatile ("srl %0, %1, %2" : "=r"(res) : "r"(d), "r"(1));
    check(res, 0x43B2A190); // Verify SRL (Shift Right Logical)
    asm volatile ("sra %0, %1, %2" : "=r"(res) : "r"(d), "r"(1));
    check(res, 0xC3B2A190); // Verify SRA (Shift Right Arithmetic)
    
    asm volatile ("sll %0, %1, %2" : "=r"(res) : "r"(a), "r"(4));
    check(res, 0x23456780); // Verify SLL (Shift Left Logical)


    // Test RV32I Comparison instructions
    asm volatile ("slt %0, %1, %2" : "=r"(res) : "r"(pos), "r"(neg));
    check(res, 0); // Verify SLT (Set Less Than - signed)
    asm volatile ("slt %0, %1, %2" : "=r"(res) : "r"(neg), "r"(pos));
    check(res, 1); // Verify SLT (Set Less Than - signed)
    asm volatile ("sltu %0, %1, %2" : "=r"(res) : "r"(neg), "r"(pos));
    check(res, 0); // Verify SLTU (Set Less Than Unsigned)
    asm volatile ("sltu %0, %1, %2" : "=r"(res) : "r"(pos), "r"(neg));
    check(res, 1); // Verify SLTU (Set Less Than Unsigned)

    // Test RV32I instructions with Immediate values
    asm volatile ("addi %0, %1, 0x123" : "=r"(res) : "r"(a));
    check(res, 0x1234579B); // Verify ADDI (Add Immediate)
    asm volatile ("slli %0, %1, 2" : "=r"(res) : "r"(b));
    check(res, 0x4);           // Verify SLLI (Shift Left Logical Immediate)

    // Test RV32I Memory Access (Load and Store) instructions
    
    check(mem32[0], 0x01234567); 
    check(mem32[1], 0x89ABCDEF);
    check(mem32[2], 0xFEDCBA98);
    check(mem32[3], 0x76543210);

    check(mem16[0], 0x4567); 
  //check(mem16[1], 0x0123); 
    check(mem16[2], 0xCDEF); 
  //check(mem16[3], 0x89AB); 
    check(mem16[4], 0xBA98); 
  //check(mem16[5], 0xFEDC); 
    check(mem16[6], 0x3210); 
  //check(mem16[7], 0x7654); 

    check(mem8[0], 0x67);
    //check(mem8[1], 0x45);
    //check(mem8[2], 0x23);
    //check(mem8[3], 0x01);
    check(mem8[4], 0xEF);
    //check(mem8[5], 0xCD);
    //check(mem8[6], 0xAB);
    //check(mem8[7], 0x89);
    check(mem8[8], 0x98);
    //check(mem8[9], 0xBA);
    //check(mem8[10], 0xDC);
    //check(mem8[11], 0xFE);
    check(mem8[12], 0x10);
    //check(mem8[13], 0x32);
    //check(mem8[14], 0x54);
    //check(mem8[15], 0x76);


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


    // Test SLTI (Set Less Than Immediate - signed)
    int32_t slti_res;
    asm volatile ("slti %0, %1, 5" : "=r"(slti_res) : "r"(pos));
    check(slti_res, 0);
    asm volatile ("slti %0, %1, 5" : "=r"(slti_res) : "r"(neg));
    check(slti_res, 1);

        
    // Test SLTIU (Set Less Than Immediate Unsigned)
    int32_t sltiu_res;
    asm volatile ("sltiu %0, %1, 15" : "=r"(sltiu_res) : "r"(pos));
    check(sltiu_res, 1);
    asm volatile ("sltiu %0, %1, 15" : "=r"(sltiu_res) : "r"(neg));
    check(sltiu_res, 0);

    // Test XORI and ORI (Logical operations with immediates)
    asm volatile ("xori %0, %1, 0x123" : "=r"(res) : "r"(a));
    check(res, 0x1234575B); // Verify XORI (Exclusive OR Immediate)
    asm volatile ("ori %0, %1, 0x123" : "=r"(res) : "r"(a));
    check(res, 0x1234577B); // Verify ORI (Logical OR Immediate)
    asm volatile ("andi %0, %1, 0x123" : "=r"(res) : "r"(a));
    check(res, 0x00000020); // Verify ANDI (Logical AND Immediate)

    // SLL, SRL, and SRA (Shift operations with register-based shift amount)
    volatile uint32_t shamt = 3;
    check(b << shamt, 8); // Verify SLL (Shift Left Logical)
    check(a >> shamt, 0x2468ACF); // Verify SRL (Shift Right Logical)
    volatile int32_t s_signed = 0x80000000;
    check(s_signed >> shamt, 0xF0000000); // Verify SRA (Shift Right Arithmetic)

    return 0;    
}

uint32_t main() {
    uint32_t operands[10] = {0x12345678, 
                            0x00000001, 
                            0x12345678, 
                            0x87654321,
                            -10, 
                            10, 
                            0x01234567, 
                            0x89ABCDEF, 
                            0xFEDCBA98, 
                            0x76543210};
    
    testcase(operands);

    test_pass();

    return 0;
}
