# Makefile for WardRV Compliance Tests

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

# Path to the tests source code
TEST_DIR     = riscv-compliance-ws/src/riscv-arch-test/riscv-test-suite/rv32i_m/I/src
BUILD_DIR    = build

# -----------------------------------------------------------------------------
# Files
# -----------------------------------------------------------------------------
#SRCS         = $(wildcard $(TEST_DIR)/*.S)
SRCS         = $(TEST_DIR)/add-01.S
OBJS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.o, $(SRCS))
ELFS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.elf, $(SRCS))
BINS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.bin, $(SRCS))
HEXS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.hex, $(SRCS))
LSTS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.lst, $(SRCS))

# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------
# Include paths: Current dir (for model_test.h) and Suite includes
INCLUDES     = -I.
INCLUDES    += -I$(TEST_DIR)/../../../env

CFLAGS       = -march=rv32i        # Architecture de base RISC-V 32-bit entier
CFLAGS      += -mabi=ilp32         # ABI standard 32-bit (int, long, pointeurs sont 32-bit)
CFLAGS      += -DXLEN=32           # Définir XLEN pour les macros de test
CFLAGS      += -O2                 # Optimisation de niveau 2 pour la performance
CFLAGS      += -ffreestanding      # Environnement sans OS (pas de bibliothèque standard complète)
CFLAGS      += -nostartfiles       # Ne pas utiliser les fichiers de démarrage standards (crt0...)
CFLAGS      += -nostdlib           # Ne pas lier les bibliothèques standards
CFLAGS      += -g                  # Inclure les informations de débogage
CFLAGS      += -fverbose-asm       # Ajouter des commentaires explicatifs dans l'assembleur généré
CFLAGS      += -Wall               # Activer tous les avertissements de base
CFLAGS      += -Wextra             # Activer des avertissements supplémentaires
#CFLAGS      += -Werror             # Traiter les avertissements comme des erreurs
CFLAGS      += -static             # Lier statiquement les bibliothèques
CFLAGS      += -mcmodel=medany     # Modèle de code pour adresser n'importe où dans une plage de 2 Go
CFLAGS	    += -DTEST_CASE_1
CFLAGS      += $(INCLUDES)
    
LDFLAGS      = -T link.ld
LDFLAGS     += -Wl,--no-warn-rwx-segments

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------
.PHONY: all clean

all: $(HEXS) $(LSTS)

$(BUILD_DIR)/%.o: $(TEST_DIR)/%.S
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.elf: $(BUILD_DIR)/%.o
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

$(BUILD_DIR)/%.bin: $(BUILD_DIR)/%.elf
	$(OBJCOPY) -O binary $< $@

$(BUILD_DIR)/%.hex: $(BUILD_DIR)/%.bin
	hexdump -v -e '1/4 "%08x\n"' $< > $@

$(BUILD_DIR)/%.lst: $(BUILD_DIR)/%.elf
	$(OBJDUMP) -D -S $< > $@

clean:
	rm -rf $(BUILD_DIR)
