# Tools
CC        = riscv64-unknown-elf-gcc
OBJCOPY   = riscv64-unknown-elf-objcopy
OBJDUMP   = riscv64-unknown-elf-objdump

# Compilation options for RV32I (Base architecture without hardware multiplication/division)
CFLAGS    = -march=rv32i -mabi=ilp32 -O2 -ffreestanding -nostartfiles -nostdlib -g -fverbose-asm -Wall -Wextra -Werror

# Files
SRC       = start.S main.c
DIR_BUILD = build
ELF       = $(DIR_BUILD)/firmware.elf
BIN       = $(DIR_BUILD)/firmware.bin
HEX       = $(DIR_BUILD)/firmware.hex
LST       = $(DIR_BUILD)/firmware.lst
ASM       = $(DIR_BUILD)/main.s

all: $(HEX) $(LST) $(ASM)

# 1. Compilation and Linking
$(ELF): $(SRC) link.ld | $(DIR_BUILD)
	$(CC) $(CFLAGS) -T link.ld -o $@ $(SRC)

# 2. Pure binary extraction
$(BIN): $(ELF) | $(DIR_BUILD)
	$(OBJCOPY) -O binary $< $@

# 3. Hexadecimal conversion (32-bit text format per line for VHDL)
# hexdump reads 4 bytes and displays them in hexadecimal on one line
$(HEX): $(BIN) | $(DIR_BUILD)
	hexdump -v -e '1/4 "%08x\n"' $< > $@

# 4. Disassembly with source (Listing)
$(LST): $(ELF) | $(DIR_BUILD)
	$(OBJDUMP) -d -S $< > $@

# 5. Commented assembly file (GCC output)
$(ASM): main.c | $(DIR_BUILD)
	$(CC) $(CFLAGS) -S $< -o $@

$(DIR_BUILD) :
	mkdir -p $@

clean:
	rm -fr $(DIR_BUILD)
