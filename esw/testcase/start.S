.section .text
.global _start

_start:
    # Initialize the Stack Pointer (SP) using the symbol defined in the linker script
    la   sp, _stack_top
    addi sp,sp,-8

    # Invoke the main function
    call main

    # Infinite loop to prevent execution from proceeding beyond the intended scope
loop:
    j loop
