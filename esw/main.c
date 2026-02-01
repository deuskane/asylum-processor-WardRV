#define TOHOST_ADDR 0x80001000

// Function to write a 32-bit word to memory
void write_word(int addr, int data) {
    *(volatile int*)addr = data;
}

int main() {
    int a = 10;
    int b = 32;
    int c = a + b; // Simple addition test

    // If the calculation is correct (42), write 1 to TOHOST to stop the simulation successfully
    if (c == 42) {
        write_word(TOHOST_ADDR, 1); // PASS
    } else {
        write_word(TOHOST_ADDR, 0); // FAIL
    }

    return 0;
}
