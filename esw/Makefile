# Tools
CC      = riscv64-unknown-elf-gcc
OBJCOPY = riscv64-unknown-elf-objcopy
OBJDUMP = riscv64-unknown-elf-objdump

# Compilation options for RV32I (Base architecture without hardware multiplication/division)
CFLAGS  = -march=rv32i -mabi=ilp32 -O2 -ffreestanding -nostartfiles -nostdlib -g -fverbose-asm

# Files
SRC     = start.S main.c
ELF     = firmware.elf
BIN     = firmware.bin
HEX     = firmware.hex
LST     = firmware.lst
ASM     = main.s

all: $(HEX) $(LST) $(ASM)

# 1. Compilation and Linking
$(ELF): $(SRC) link.ld
	$(CC) $(CFLAGS) -T link.ld -o $@ $(SRC)

# 2. Pure binary extraction
$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

# 3. Hexadecimal conversion (32-bit text format per line for VHDL)
# hexdump reads 4 bytes and displays them in hexadecimal on one line
$(HEX): $(BIN)
	hexdump -v -e '1/4 "%08x\n"' $< > $@

# 4. Disassembly with source (Listing)
$(LST): $(ELF)
	$(OBJDUMP) -d -S $< > $@

# 5. Commented assembly file (GCC output)
$(ASM): main.c
	$(CC) $(CFLAGS) -S $< -o $@

# Utilities
dump: $(ELF)
	$(OBJDUMP) -d $(ELF)

clean:
	rm -f $(ELF) $(BIN) $(HEX) $(LST) $(ASM)
