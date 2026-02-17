# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

# Compilation options for RV32I (Base architecture without hardware multiplication/division)

# -----------------------------------------------------------------------------
# Files
# -----------------------------------------------------------------------------
SRC          = start.S main.c
BUILD_DIR    = build
ELF          = $(BUILD_DIR)/firmware.elf
BIN          = $(BUILD_DIR)/firmware.bin
HEX          = $(BUILD_DIR)/firmware.hex
LST          = $(BUILD_DIR)/firmware.lst
ASM          = $(BUILD_DIR)/main.s

# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------
CFLAGS       = -march=rv32i        # Architecture de base RISC-V 32-bit entier
CFLAGS      += -mabi=ilp32         # ABI standard 32-bit (int, long, pointeurs sont 32-bit)
CFLAGS      += -O2                 # Optimisation de niveau 2 pour la performance
CFLAGS      += -ffreestanding      # Environnement sans OS (pas de bibliothèque standard complète)
CFLAGS      += -nostartfiles       # Ne pas utiliser les fichiers de démarrage standards (crt0...)
CFLAGS      += -nostdlib           # Ne pas lier les bibliothèques standards
CFLAGS      += -g                  # Inclure les informations de débogage
CFLAGS      += -fverbose-asm       # Ajouter des commentaires explicatifs dans l'assembleur généré
CFLAGS      += -Wall               # Activer tous les avertissements de base
CFLAGS      += -Wextra             # Activer des avertissements supplémentaires
CFLAGS      += -Werror             # Traiter les avertissements comme des erreurs
CFLAGS      += -static             # Lier statiquement les bibliothèques
CFLAGS      += -mcmodel=medany     # Modèle de code pour adresser n'importe où dans une plage de 2 Go
CFLAGS      += $(INCLUDES)


# Include paths: Current dir (for model_test.h) and Suite includes
#INCLUDES = -I. -I$(TEST_DIR)/../../../../include
#
#CFLAGS   = -march=rv32i -mabi=ilp32 -static -mcmodel=medany -g \
#           -fvisibility=hidden -nostdlib -nostartfiles $(INCLUDES)
#
#LDFLAGS  = -T link.ld -Wl,--no-warn-rwx-segments

all: $(HEX) $(LST) $(ASM)

# 1. Compilation and Linking
$(ELF): $(SRC) link.ld | $(BUILD_DIR)
	$(CC) $(CFLAGS) -T link.ld -o $@ $(SRC)

# 2. Pure binary extraction
$(BIN): $(ELF) | $(BUILD_DIR)
	$(OBJCOPY) -O binary $< $@

# 3. Hexadecimal conversion (32-bit text format per line for VHDL)
# hexdump reads 4 bytes and displays them in hexadecimal on one line
$(HEX): $(BIN) | $(BUILD_DIR)
	hexdump -v -e '1/4 "%08x\n"' $< > $@

# 4. Disassembly with source (Listing)
$(LST): $(ELF) | $(BUILD_DIR)
	$(OBJDUMP) -d -S $< > $@

# 5. Commented assembly file (GCC output)
$(ASM): main.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -S $< -o $@

$(BUILD_DIR) :
	mkdir -p $@

clean:
	rm -fr $(BUILD_DIR)
