#include <stdint.h>  
#define TOHOST_ADDR 0x80001000

// Function to write a 32-bit word to memory
void write_word(uint32_t addr, uint32_t data) {
    *(volatile uint32_t*)addr = data;
}

uint32_t add (uint32_t a, uint32_t b) {
    return a + b;
}


uint32_t main() {
    uint32_t a = 10;
    uint32_t b = 32;
    uint32_t c = add(a, b); // Simple addition test

    // If the calculation is correct (42), write 1 to TOHOST to stop the simulation successfully
    if (c == 42) {
        write_word(TOHOST_ADDR, 1); // PASS
    } else {
        write_word(TOHOST_ADDR, 0); // FAIL
    }

    return 0;
}
