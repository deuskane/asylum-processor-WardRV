# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

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
CFLAGS       = -march=rv32i_zicsr  # RISC-V 32-bit integer base architecture
CFLAGS      += -mabi=ilp32         # Standard 32-bit ABI (int, long, pointers are 32-bit)
CFLAGS      += -O2                 # Level 2 optimization for performance
CFLAGS      += -ffreestanding      # OS-less environment (no full standard library)
CFLAGS      += -nostartfiles       # Do not use standard startup files (crt0...)
CFLAGS      += -nostdlib           # Do not link standard libraries
CFLAGS      += -g                  # Include debugging information
CFLAGS      += -fverbose-asm       # Add explanatory comments in generated assembly
CFLAGS      += -Wall               # Enable all basic warnings
CFLAGS      += -Wextra             # Enable extra warnings
CFLAGS      += -Werror             # Treat warnings as errors
CFLAGS      += -static             # Statically link libraries
CFLAGS      += -mcmodel=medany     # Code model to address anywhere within a 2 GB range
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
