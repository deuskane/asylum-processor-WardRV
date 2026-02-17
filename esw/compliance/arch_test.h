#ifndef _ARCH_TEST_H
#define _ARCH_TEST_H

#include "model_test.h"

// -----------------------------------------------------------------------------
// Parameters
// -----------------------------------------------------------------------------
#define XLEN 32

// -----------------------------------------------------------------------------
// Architecture Macros (Must be defined before including test_macros.h)
// -----------------------------------------------------------------------------
#define LI(_reg, _val)    li _reg, _val
#define SREG              sw
#define LREG              lw

#include "test_macros.h"

// -----------------------------------------------------------------------------
// Fallback Macros (Fix for missing definitions in test_macros.h)
// -----------------------------------------------------------------------------
#ifndef MASK_XLEN
#define MASK_XLEN(x) x
#endif
#ifndef SEXT_IMM
#define SEXT_IMM(x) x
#endif

// -----------------------------------------------------------------------------
// RVTEST Macros (Required by Test Source)
// -----------------------------------------------------------------------------
#define RVTEST_ISA(_STR)      .section .text
#define RVTEST_CODE_BEGIN     .section .text
#define RVTEST_CODE_END       nop
#define RVTEST_DATA_BEGIN     .section .data
#define RVTEST_DATA_END       .section .text
#define RVTEST_CASE(_P, _C, _N) .global test_case_##_P; test_case_##_P:

// Undefine to override the default from test_macros.h and initialize offset
#undef RVTEST_SIGBASE
#define RVTEST_SIGBASE(_R, _L) la _R, _L

// Override SIGUPD to use the explicit offset passed by the test, avoiding assembler loops
#undef RVTEST_SIGUPD
#define RVTEST_SIGUPD(_R, _V, _O) SREG _V, _O(_R)

// Canary for signature boundaries
#define CANARY .word 0xCAFECAFE

#endif