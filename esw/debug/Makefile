# -----------------------------------------------------------------------------
# Firmware of the debug system test (tb_WardRV_debug) : debug_fw.S -> debug_fw.hex, debug_fw.lst
# Both outputs are committed : the simulation does not need the toolchain
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

BUILD_DIR    = build
ELF          = $(BUILD_DIR)/debug_fw.elf
BIN          = $(BUILD_DIR)/debug_fw.bin

CFLAGS       = -march=rv32i_zicsr        # RISC-V 32-bit integer base + CSR instructions
CFLAGS      += -mabi=ilp32               # 32-bit ABI
CFLAGS      += -nostdlib -nostartfiles   # No library, no startup file
CFLAGS      += -Wl,-Ttext=0x80000000     # Reset address of tb_WardRV_debug

all: debug_fw.hex debug_fw.lst

$(ELF): debug_fw.S | $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $@ $<

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

# One 32-bit word per line (format read by tb_WardRV_debug)
debug_fw.hex: $(BIN)
	hexdump -v -e '1/4 "%08x\n"' $< > $@

debug_fw.lst: $(ELF)
	$(OBJDUMP) -d $< > $@

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -fr $(BUILD_DIR)
