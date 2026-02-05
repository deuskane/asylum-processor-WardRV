#include <stdint.h>  
#define TOHOST_ADDR 0x80001000

// Function to write a 32-bit word to memory
void write_word(uint32_t addr, uint8_t data) {
    *(volatile uint32_t*)addr = data;
}

uint8_t add (uint8_t a, uint8_t b) {
    return a + b;
}


uint8_t main() {
    uint8_t a = 10;
    uint8_t b = 32;
    uint8_t c = add(a, b); // Simple addition test

    // If the calculation is correct (42), write 1 to TOHOST to stop the simulation successfully
    if (c == 42) {
        write_word(TOHOST_ADDR, 1); // PASS
    } else {
        write_word(TOHOST_ADDR, 0); // FAIL
    }

    return 0;
}
