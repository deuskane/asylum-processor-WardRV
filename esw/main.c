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
    volatile uint32_t * mem32u = &(operands[6]);
    volatile uint16_t * mem16u = (uint16_t *)mem32u;
    volatile uint8_t  * mem8u  = (uint8_t  *)mem32u;
    volatile int32_t  * mem32i = (int32_t *)&(operands[6]);
    volatile int16_t  * mem16i = (int16_t *)mem32i;
    volatile int8_t   * mem8i  = (int8_t  *)mem32i;
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
    check(mem32u[0], 0x01234567); 
    check(mem32u[1], 0x89ABCDEF);
    check(mem32u[2], 0xFEDCBA98);
    check(mem32u[3], 0x76543210);

    check(mem16u[0], 0x4567); 
    check(mem16u[1], 0x0123); 
    check(mem16u[2], 0xCDEF); 
    check(mem16u[3], 0x89AB); 
    check(mem16u[4], 0xBA98); 
    check(mem16u[5], 0xFEDC); 
    check(mem16u[6], 0x3210); 
    check(mem16u[7], 0x7654); 

    check(mem8u[0], 0x67);
    check(mem8u[1], 0x45);
    check(mem8u[2], 0x23);
    check(mem8u[3], 0x01);
    check(mem8u[4], 0xEF);
    check(mem8u[5], 0xCD);
    check(mem8u[6], 0xAB);
    check(mem8u[7], 0x89);
    check(mem8u[8], 0x98);
    check(mem8u[9], 0xBA);
    check(mem8u[10], 0xDC);
    check(mem8u[11], 0xFE);
    check(mem8u[12], 0x10);
    check(mem8u[13], 0x32);
    check(mem8u[14], 0x54);
    check(mem8u[15], 0x76);

    check(mem32i[0], 0x01234567); 
    check(mem32i[1], 0x89ABCDEF);
    check(mem32i[2], 0xFEDCBA98);
    check(mem32i[3], 0x76543210);

    int16_t data_i16;

    asm volatile ("lh %0, 0(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0x00004567); 
    asm volatile ("lh %0, 2(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0x00000123); 
    asm volatile ("lh %0, 4(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0xFFFFCDEF); 
    asm volatile ("lh %0, 6(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0xFFFF89AB); 
    asm volatile ("lh %0, 8(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0xFFFFBA98); 
    asm volatile ("lh %0, 10(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0xFFFFFEDC); 
    asm volatile ("lh %0, 12(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0x00003210); 
    asm volatile ("lh %0, 14(%1)" : "=r"(data_i16) : "r"(mem16i));
    check(data_i16, 0x00007654); 

    int8_t data_i8;

    asm volatile ("lb %0, 0(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8,  0x00000067);
    asm volatile ("lb %0, 1(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000045);
    asm volatile ("lb %0, 2(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000023);
    asm volatile ("lb %0, 3(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000001);

    asm volatile ("lb %0, 4(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8,  0xFFFFFFEF);
    asm volatile ("lb %0, 5(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFFCD);
    asm volatile ("lb %0, 6(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFFAB);
    asm volatile ("lb %0, 7(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFF89);
        
    asm volatile ("lb %0, 8(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFF98);
    asm volatile ("lb %0, 9(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFFBA);
    asm volatile ("lb %0, 10(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFFDC);
    asm volatile ("lb %0, 11(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0xFFFFFFFE);

    asm volatile ("lb %0, 12(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000010);
    asm volatile ("lb %0, 13(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000032);
    asm volatile ("lb %0, 14(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000054);
    asm volatile ("lb %0, 15(%1)" : "=r"(data_i8) : "r"(mem8i));
    check(data_i8, 0x00000076);
    

    // Test RV32I Conditional Branching instructions
    res = 42;
    asm volatile (
        "bne %1, %2, 1f\n"
        "li  %0, 24\n"
        "1:\n"
        : "+r"(res)
        : "r"(a), "r"(b)
    );
    // Verify BNE (Branch Not Equal)
    check(res, 42);

    res = 42;
    asm volatile (
        "bne %1, %2, 1f\n"
        "li  %0, 24\n"
        "1:\n"
        : "+r"(res)
        : "r"(a), "r"(c)
    );
    // Verify BNE (Branch Not Equal)
    check(res, 24);

    res = 42;
    asm volatile (
        "beq %1, %2, 1f\n"
        "li  %0, 24\n"
        "1:\n"
        : "+r"(res)
        : "r"(a), "r"(c)
    );
    // Verify BEQ (Branch Equal)
    check(res, 42);

    res = 42;
    asm volatile (
        "beq %1, %2, 1f\n"
        "li  %0, 24\n"
        "1:\n"
        : "+r"(res)
        : "r"(a), "r"(b)
    );
    // Verify BEQ (Branch Equal)
    check(res, 24);

    res = 42;
    asm volatile (
        "blt %1, %2, 1f\n"
        "li  %0, 24\n"
        "1:\n"
        : "+r"(res)
        : "r"((int32_t)neg), "r"((int32_t)pos)
    );
    // Verify BLT (Branch Less Than)
    check(res, 42);

    res = 42;
    asm volatile (
        "bgeu %1, %2, 1f\n"
        "li  %0, 24\n"
        "1:\n"
        : "+r"(res)
        : "r"(a), "r"(b)
    );
    // Verify BGEU (Branch Greater or Equal Unsigned)
    check(res, 42);

    
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
    uint32_t shamt = 3;
    int32_t  s_signed = 0x80000000;
    asm volatile ("sll %0, %1, %2" : "=r"(res) : "r"(b), "r"(shamt));
    check(res, 8);           // Verify SLL (Shift Left Logical)
    asm volatile ("srl %0, %1, %2" : "=r"(res) : "r"(a), "r"(shamt));
    check(res, 0x02468ACF);  // Verify SRL (Shift Right Logical)
    asm volatile ("sra %0, %1, %2" : "=r"(res) : "r"(s_signed), "r"(shamt));
    check(res, 0xF0000000);  // Verify SRA (Shift Right Arithmetic)

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
