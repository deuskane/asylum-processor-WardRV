# -----------------------------------------------------------------------------
# Debug ROM of the Debug Module : debug_rom.S -> debug_rom.hex, debug_rom.lst
# and the VHDL package hdl/debug/WardRV_dbg_rom_pkg.vhd (rom_pkg.awk)
# The outputs are committed : the simulation does not need the toolchain
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

BUILD_DIR    = build
ELF          = $(BUILD_DIR)/debug_rom.elf
BIN          = $(BUILD_DIR)/debug_rom.bin
PKG          = ../../hdl/debug/WardRV_dbg_rom_pkg.vhd

CFLAGS       = -march=rv32i_zicsr        # RISC-V 32-bit integer base + CSR instructions
CFLAGS      += -mabi=ilp32               # 32-bit ABI
CFLAGS      += -nostdlib -nostartfiles   # No library, no startup file
CFLAGS      += -Wl,-Ttext=0xFFFFF800     # Base of the debug memory

all: debug_rom.hex debug_rom.lst $(PKG)

$(ELF): debug_rom.S | $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $@ $<

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

# One 32-bit word per line
debug_rom.hex: $(BIN)
	hexdump -v -e '1/4 "%08x\n"' $< > $@

debug_rom.lst: $(ELF)
	$(OBJDUMP) -d $< > $@

$(PKG): debug_rom.lst debug_rom.hex rom_pkg.awk
	awk -f rom_pkg.awk debug_rom.lst debug_rom.hex > $@

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -fr $(BUILD_DIR)
