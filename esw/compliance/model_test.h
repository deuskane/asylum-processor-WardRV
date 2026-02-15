#ifndef _MODEL_TEST_H
#define _MODEL_TEST_H

#define RV_COMPLIANCE_HALT \
    /* Dump Signature */ \
    li t0, 0x80001004;          /* C_SIGNATURE_ADDR */ \
    la t1, begin_signature; \
    la t2, end_signature; \
1: \
    bge t1, t2, 2f;             /* End of signature? */ \
    lw t3, 0(t1);               /* Load signature value */ \
    sw t3, 0(t0);               /* Write to MMIO (triggers TB write) */ \
    addi t1, t1, 4; \
    j 1b; \
2: \
    /* Halt Simulation */ \
    li t0, 0x80001000;          /* C_TOHOST_ADDR */ \
    li t1, 1; \
    sw t1, 0(t0);               /* Write 1 to TOHOST */ \
    wfi;

#define RV_COMPLIANCE_RV32M
#define RV_COMPLIANCE_CODE_BEGIN .section .text.init; .globl _start; _start:
#define RV_COMPLIANCE_CODE_END
#define RV_COMPLIANCE_DATA_BEGIN .section .data; .align 4; .globl begin_signature; begin_signature:
#define RV_COMPLIANCE_DATA_END .align 4; .globl end_signature; end_signature:

#endif