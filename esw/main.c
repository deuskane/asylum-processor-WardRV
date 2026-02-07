-------------------------------------------------------------------------------
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
    *(volatile uint32_t*)TOHOST_ADDR = 1; // PASS
}

static inline void test_fail() {
    *(volatile uint32_t*)TOHOST_ADDR = 0; // FAIL
}

uint8_t add_8b (uint8_t a, uint8_t b) 
{
    return a + b;
}

uint8_t sub_8b (uint8_t a, uint8_t b) 
{
    return a - b;
}

uint8_t or_8b (uint8_t a, uint8_t b) 
{
    return a | b;
}

uint8_t and_8b (uint8_t a, uint8_t b) 
{
    return a & b;
}

uint8_t xor_8b (uint8_t a, uint8_t b) 
{
    return a ^ b;
}

// uint8_t mul_8b (uint8_t a, uint8_t b) 
// {
//     return a * b;
// }
// 
// uint8_t div_8b (uint8_t a, uint8_t b) 
// {
//     return (b != 0) ? (a / b) : 0;
// }
// 
void check(uint8_t result, uint8_t expected) {
    if (result != expected) {
        
        test_fail();
    }
    
}

uint8_t main() {
    uint8_t a = 0x10;
    uint8_t b = 0x32;
    
    check(add_8b(a,b), 0x42);
    //check(sub_8b(b,a), 0x22);
    //check(or_8b( a,b), 0x32);
    //check(and_8b(a,b), 0x10);
    //check(xor_8b(a,b), 0x22);

    test_pass();

    return 0;
}
