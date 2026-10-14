#-----------------------------------------------------------------------------
# Title      : Makefile
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : esw/openocd/Makefile
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Firmware of the OpenOCD / GDB co-simulation (cosim_openocd)
#              fw.elf : loaded by GDB (debug information)
#              fw.hex : initial content of the RAM of tb_WardRV_openocd
#                       (one little-endian 32-bit word per line, init_ram)
#              fw.lst : disassembly with source
#              The three files are committed. To rebuild them (RISC-V
#              toolchain, RISCV_PREFIX) :
#                make -C esw/openocd
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-09  1.0      mrosiere Created
#-----------------------------------------------------------------------------

RISCV_PREFIX ?= riscv64-unknown-elf-
CC            = $(RISCV_PREFIX)gcc
OBJCOPY       = $(RISCV_PREFIX)objcopy
OBJDUMP       = $(RISCV_PREFIX)objdump

SRC           = start.S main.c
ELF           = fw.elf
HEX           = fw.hex
LST           = fw.lst
BIN           = fw.bin

CFLAGS        = -march=rv32i_zicsr -mabi=ilp32
# -O0 : every statement keeps its own code (GDB break / next)
CFLAGS       += -O0 -g
CFLAGS       += -ffreestanding -fno-builtin
CFLAGS       += -nostartfiles -nostdlib -static
CFLAGS       += -mcmodel=medany
CFLAGS       += -Wall -Wextra -Werror
LDFLAGS       = -T link.ld -Wl,--no-warn-rwx-segments

.PHONY : all clean

all : $(ELF) $(HEX) $(LST)

# Objects with fixed names : the linker records their names in fw.elf
# (gcc temporary names would make the ELF different at each build)
$(ELF) : $(SRC) link.ld
	$(CC) $(CFLAGS) -c -o start.o start.S
	$(CC) $(CFLAGS) -c -o main.o main.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ start.o main.o
	rm -f start.o main.o

# One 32-bit little-endian word per line (same format as esw/testcase)
$(HEX) : $(ELF)
	$(OBJCOPY) -O binary $< $(BIN)
	od -An -v -tx4 -w4 --endian=little $(BIN) | tr -d ' ' > $@
	rm -f $(BIN)

$(LST) : $(ELF)
	$(OBJDUMP) -d -S $< > $@

clean :
	rm -f $(ELF) $(HEX) $(LST) $(BIN) start.o main.o
