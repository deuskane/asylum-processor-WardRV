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

#define BRANCH_TAKE 42
#define BRANCH_NOT_TAKE 24
#define CHECK_BRANCH(inst, op1, op2, expected) \
    res = BRANCH_TAKE ;                         \
    asm volatile (                             \
        inst " %1, %2, 1f\n"                   \
        "li  %0, %3\n"                         \
        "1:\n"                                 \
        : "+r"(res)                            \
        : "r"(op1), "r"(op2), "i"(BRANCH_NOT_TAKE) \
    );                                         \
    check(res, expected);

#define SW(offset, value) \
    data_u32 = value; \
    asm volatile ("sw %0, " #offset "(%1)" :: "r"(data_u32), "r"(mem32u) : "memory");

#define SH(offset, value) \
    data_u16 = value; \
    asm volatile ("sh %0, " #offset "(%1)" :: "r"(data_u16), "r"(mem16u) : "memory");

#define SB(offset, value) \
    data_u8 = value; \
    asm volatile ("sb %0, " #offset "(%1)" :: "r"(data_u8), "r"(mem8u) : "memory");

#define CHECK_LW(offset, expected) \
    asm volatile ("lw %0, " #offset "(%1)" : "=r"(data_u32) : "r"(mem32u)); \
    check(data_u32, expected);

#define CHECK_LHU(offset, expected) \
    asm volatile ("lhu %0, " #offset "(%1)" : "=r"(data_u16) : "r"(mem16u)); \
    check(data_u16, expected);

#define CHECK_LH(offset, expected) \
    asm volatile ("lh %0, " #offset "(%1)" : "=r"(data_i16) : "r"(mem16i)); \
    check(data_i16, expected);

#define CHECK_LB(offset, expected) \
    asm volatile ("lb %0, " #offset "(%1)" : "=r"(data_i8) : "r"(mem8i)); \
    check(data_i8, expected);

#define CHECK_LBU(offset, expected) \
    asm volatile ("lbu %0, " #offset "(%1)" : "=r"(data_u8) : "r"(mem8u)); \
    check(data_u8, expected);

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
    uint32_t data_u32;
    uint16_t data_u16;
    uint8_t  data_u8;
    int16_t  data_i16;
    int8_t   data_i8;

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
    asm volatile ("slli %0, %1, 4" : "=r"(res) : "r"(a));
    check(res, 0x23456780); // Verify SLLI (Shift Left Logical Immediate)
    asm volatile ("srli %0, %1, 4" : "=r"(res) : "r"(a));
    check(res, 0x01234567);
    
    // Test RV32I Memory Access (Load and Store) instructions
    CHECK_LW(0,  0x01234567);
    CHECK_LW(4,  0x89ABCDEF);
    CHECK_LW(8,  0xFEDCBA98);
    CHECK_LW(12, 0x76543210);

    CHECK_LHU(0,  0x4567);
    CHECK_LHU(2,  0x0123);
    CHECK_LHU(4,  0xCDEF);
    CHECK_LHU(6,  0x89AB);
    CHECK_LHU(8,  0xBA98);
    CHECK_LHU(10, 0xFEDC);
    CHECK_LHU(12, 0x3210);
    CHECK_LHU(14, 0x7654);

    CHECK_LBU(0,  0x67);
    CHECK_LBU(1,  0x45);
    CHECK_LBU(2,  0x23);
    CHECK_LBU(3,  0x01);
    CHECK_LBU(4,  0xEF);
    CHECK_LBU(5,  0xCD);
    CHECK_LBU(6,  0xAB);
    CHECK_LBU(7,  0x89);
    CHECK_LBU(8,  0x98);
    CHECK_LBU(9,  0xBA);
    CHECK_LBU(10, 0xDC);
    CHECK_LBU(11, 0xFE);
    CHECK_LBU(12, 0x10);
    CHECK_LBU(13, 0x32);
    CHECK_LBU(14, 0x54);
    CHECK_LBU(15, 0x76);

    CHECK_LH(0,  0x00004567);
    CHECK_LH(2,  0x00000123);
    CHECK_LH(4,  0xFFFFCDEF);
    CHECK_LH(6,  0xFFFF89AB);
    CHECK_LH(8,  0xFFFFBA98);
    CHECK_LH(10, 0xFFFFFEDC);
    CHECK_LH(12, 0x00003210);
    CHECK_LH(14, 0x00007654);

    CHECK_LB(0,  0x00000067);
    CHECK_LB(1,  0x00000045);
    CHECK_LB(2,  0x00000023);
    CHECK_LB(3,  0x00000001);
    CHECK_LB(4,  0xFFFFFFEF);
    CHECK_LB(5,  0xFFFFFFCD);
    CHECK_LB(6,  0xFFFFFFAB);
    CHECK_LB(7,  0xFFFFFF89);
    CHECK_LB(8,  0xFFFFFF98);
    CHECK_LB(9,  0xFFFFFFBA);
    CHECK_LB(10, 0xFFFFFFDC);
    CHECK_LB(11, 0xFFFFFFFE);
    CHECK_LB(12, 0x00000010);
    CHECK_LB(13, 0x00000032);
    CHECK_LB(14, 0x00000054);
    CHECK_LB(15, 0x00000076);

    SW(      0,  0x0badbeef);
    CHECK_LH(0,  0xffffbeef);
    CHECK_LH(2,  0x00000bad);
    CHECK_LW(0,  0x0badbeef);
    
    SH(      0,  0xcafe);
    CHECK_LW(0,  0x0badcafe);
    SH(      2,  0x900d);
    CHECK_LW(0,  0x900dcafe);
 
    SB(      0,  0x1e);
    CHECK_LW(0,  0x900dca1e);
    SB(      1,  0x2d);
    CHECK_LW(0,  0x900d2d1e);
    SB(      2,  0x3c);
    CHECK_LW(0,  0x903c2d1e);
    SB(      3,  0x4b);
    CHECK_LW(0,  0x4b3c2d1e);
 
    // Test RV32I Conditional Branching instructions
    CHECK_BRANCH("bne",  a, b, BRANCH_TAKE);
    CHECK_BRANCH("bne",  a, c, BRANCH_NOT_TAKE);

    CHECK_BRANCH("beq",  a, c, BRANCH_TAKE);
    CHECK_BRANCH("beq",  a, b, BRANCH_NOT_TAKE);

    CHECK_BRANCH("blt",   (int32_t)neg,  (int32_t)pos, BRANCH_TAKE);
    CHECK_BRANCH("blt",   (int32_t)pos,  (int32_t)neg, BRANCH_NOT_TAKE);
    CHECK_BRANCH("blt",   (int32_t)pos,  (int32_t)pos, BRANCH_NOT_TAKE);
    CHECK_BRANCH("blt",   (int32_t)neg,  (int32_t)neg, BRANCH_NOT_TAKE);

    CHECK_BRANCH("bltu", (uint32_t)neg, (uint32_t)pos, BRANCH_NOT_TAKE);
    CHECK_BRANCH("bltu", (uint32_t)pos, (uint32_t)neg, BRANCH_TAKE);
    CHECK_BRANCH("bltu", (uint32_t)pos, (uint32_t)pos, BRANCH_NOT_TAKE);
    CHECK_BRANCH("bltu", (uint32_t)neg, (uint32_t)neg, BRANCH_NOT_TAKE);

    CHECK_BRANCH("bgeu", (uint32_t)neg, (uint32_t)pos, BRANCH_TAKE);
    CHECK_BRANCH("bgeu", (uint32_t)pos, (uint32_t)neg, BRANCH_NOT_TAKE);
    CHECK_BRANCH("bgeu", (uint32_t)pos, (uint32_t)pos, BRANCH_TAKE);
    CHECK_BRANCH("bgeu", (uint32_t)neg, (uint32_t)neg, BRANCH_TAKE);

    CHECK_BRANCH("bge",   (int32_t)neg,  (int32_t)pos, BRANCH_NOT_TAKE);
    CHECK_BRANCH("bge",   (int32_t)pos,  (int32_t)neg, BRANCH_TAKE);
    CHECK_BRANCH("bge",   (int32_t)pos,  (int32_t)pos, BRANCH_TAKE);
    CHECK_BRANCH("bge",   (int32_t)neg,  (int32_t)neg, BRANCH_TAKE);

    // Test RV32I Unconditional Jump instructions
    int jump_check = 0;
    goto jump_target;
    jump_check = 1;
jump_target:                      // Target for the jump
    check(jump_check, 0);         // Verify JAL (Jump and Link) via C goto

    // Test RV32I Upper Immediate instructions
    uint32_t lui_val;
    asm volatile ("lui %0, 0x12345" : "=r"(lui_val));
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
