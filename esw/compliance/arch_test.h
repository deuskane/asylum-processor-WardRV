#ifndef _ARCH_TEST_H
#define _ARCH_TEST_H

#include "model_test.h"

// -----------------------------------------------------------------------------
// Parameters
// -----------------------------------------------------------------------------
#define XLEN 32

// -----------------------------------------------------------------------------
// Map RISC-V Arch Test Framework macros to our implementation
// -----------------------------------------------------------------------------
#define RVMODEL_BOOT          RV_COMPLIANCE_CODE_BEGIN
#define RVMODEL_HALT          RV_COMPLIANCE_HALT
#define RVMODEL_DATA_BEGIN    RV_COMPLIANCE_DATA_BEGIN
#define RVMODEL_DATA_END      RV_COMPLIANCE_DATA_END

// -----------------------------------------------------------------------------
// RVTEST Macros (Required by Test Source)
// -----------------------------------------------------------------------------
#define RVTEST_ISA(_STR)
#define RVTEST_CODE_BEGIN
#define RVTEST_CODE_END
#define RVTEST_DATA_BEGIN     .section .data
#define RVTEST_DATA_END
#define RVTEST_CASE(_P, _C, _N)
#define RVTEST_SIGBASE(_R, _L) la _R, _L

// Canary for signature boundaries
#define CANARY .word 0xCAFECAFE

// -----------------------------------------------------------------------------
// IO macros (Unused for signature-based compliance)
// -----------------------------------------------------------------------------
#define RVMODEL_IO_INIT
#define RVMODEL_IO_WRITE_STR(_SP, _STR)
#define RVMODEL_IO_CHECK()
#define RVMODEL_IO_ASSERT_GPR_EQ(_SP, _R, _I)
#define RVMODEL_IO_ASSERT_SFPR_EQ(_F, _R, _I)
#define RVMODEL_IO_ASSERT_DFPR_EQ(_D, _R, _I)

#define RVMODEL_SET_MSW_INT
#define RVMODEL_CLEAR_MSW_INT
#define RVMODEL_CLEAR_MTIMER_INT
#define RVMODEL_CLEAR_MEXT_INT

#endif