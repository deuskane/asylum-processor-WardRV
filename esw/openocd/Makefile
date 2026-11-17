# -----------------------------------------------------------------------------
# Firmware of the OpenOCD + GDB co-simulation (sim_dbg_openocd_*) :
# start.S + main.c -> fw.elf (loaded by GDB), fw.hex (initial content of the
# RAM of tb_WardRV_openocd), fw.lst. Built by the pre_build hook of the
# targets (sim/openocd/cosim.sh) with the toolchain of the asylum-env image :
# nothing is committed.
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

CFLAGS       = -march=rv32i_zicsr -mabi=ilp32  # RV32I + CSR instructions, 32-bit ABI
CFLAGS      += -O0 -g                          # Every statement keeps its own code (GDB break / next)
CFLAGS      += -ffreestanding -fno-builtin     # No library
CFLAGS      += -nostartfiles -nostdlib -static
CFLAGS      += -mcmodel=medany
CFLAGS      += -Wall -Wextra -Werror
LDFLAGS      = -T link.ld -Wl,--no-warn-rwx-segments

all: fw.elf fw.hex fw.lst

fw.elf: start.S main.c link.ld
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ start.S main.c

# One 32-bit word per line (format read by tb_WardRV_openocd)
fw.hex: fw.elf
	$(OBJCOPY) -O binary $< fw.bin
	hexdump -v -e '1/4 "%08x\n"' fw.bin > $@
	rm -f fw.bin

fw.lst: fw.elf
	$(OBJDUMP) -d -S $< > $@

clean:
	rm -f fw.elf fw.hex fw.lst fw.bin
