#define TOHOST_ADDR 0x80001000

// Fonction pour écrire un mot 32 bits en mémoire
void write_word(int addr, int data) {
    *(volatile int*)addr = data;
}

int main() {
    int a = 10;
    int b = 32;
    int c = a + b; // Test simple d'addition

    // Si le calcul est bon (42), on écrit 1 dans TOHOST pour arrêter la simu avec succès
    if (c == 42) {
        write_word(TOHOST_ADDR, 1); // PASS
    } else {
        write_word(TOHOST_ADDR, 0); // FAIL
    }

    return 0;
}
