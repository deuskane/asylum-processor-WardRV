#ifndef _COMPLIANCE_MODEL_H
#define _COMPLIANCE_MODEL_H

/* Definition of the tohost section for communication with the simulator (optional but recommended) */
#define RVMODEL_DATA_SECTION \
        .section .tohost ;                                \
        .align 12; .global tohost; tohost: .dword 0;                    \
        .align 8;  .global fromhost; fromhost: .dword 0;                \
        .align 8; .global begin_regstate; begin_regstate:               \
        .word 128;                                                      \
        .align 8; .global end_regstate; end_regstate:                   \
        .word 4;

/* Boot code: .text.init section and _start label */
#define RVMODEL_BOOT \
        .section .text.init; \
        .globl _start; \
        _start:

/* Halt code: writes 1 to tohost and enters an infinite loop */
#define RVMODEL_HALT                                \
        li x1, 1;                                   \
        la x5, tohost;                              \
        sw x1, 0(x5);                               \
    halt_loop:                                      \
        j halt_loop;

/* Start of the signature section (test results) */
#define RVMODEL_DATA_BEGIN \
        .section .data.begin; \
        .globl rvtest_data_begin; \
        rvtest_data_begin:

/* End of the signature section */
#define RVMODEL_DATA_END \
        .section .data.end; \
        .globl rvtest_data_end; \
        rvtest_data_end: \
        RVMODEL_DATA_SECTION

/* IO Macros (left empty for simple baremetal simulation) */
#define RVMODEL_IO_INIT
#define RVMODEL_IO_WRITE_STR(_R, _STR)
#define RVMODEL_IO_CHECK()
#define RVMODEL_IO_ASSERT_GPR_EQ(_S, _R, _I)
#define RVMODEL_IO_ASSERT_SFPR_EQ(_F, _R, _I)
#define RVMODEL_IO_ASSERT_DFPR_EQ(_D, _R, _I)

#define RVMODEL_SET_MSW_INT
#define RVMODEL_CLEAR_MSW_INT
#define RVMODEL_CLEAR_MTIMER_INT
#define RVMODEL_CLEAR_MEXT_INT

#endif // _COMPLIANCE_MODEL_H