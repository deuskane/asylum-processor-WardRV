# -----------------------------------------------------------------------------
# Firmware of the interrupt test (sim_irq) : irq.S -> irq_fw.hex, irq_fw.lst
# Both outputs are committed : the simulation does not need the toolchain
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

BUILD_DIR    = build
ELF          = $(BUILD_DIR)/irq.elf
BIN          = $(BUILD_DIR)/irq.bin

CFLAGS       = -march=rv32i_zicsr        # RISC-V 32-bit integer base + CSR instructions
CFLAGS      += -mabi=ilp32               # 32-bit ABI
CFLAGS      += -nostdlib -nostartfiles   # No library, no startup file
CFLAGS      += -Wl,-Ttext=0x80000000     # Reset address of tb_WardRV

all: irq_fw.hex irq_fw.lst

$(ELF): irq.S | $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $@ $<

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $< $@

# One 32-bit word per line (format read by tb_WardRV)
irq_fw.hex: $(BIN)
	hexdump -v -e '1/4 "%08x\n"' $< > $@

irq_fw.lst: $(ELF)
	$(OBJDUMP) -d $< > $@

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -fr $(BUILD_DIR)
