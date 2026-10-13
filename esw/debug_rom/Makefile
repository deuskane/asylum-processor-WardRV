#-----------------------------------------------------------------------------
# Title      : Makefile
# Project    : WardRV
#-----------------------------------------------------------------------------
# File       : Makefile
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: Debug ROM generation
#              make        : debug_rom.S -> ../../hdl/debug/WardRV_dbg_rom_pkg.vhd
#                            (gen_rom.py, no toolchain needed)
#              make check  : assemble debug_rom.S with the GNU toolchain and
#                            compare with debug_rom.hex
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-09  1.0      mrosiere Created
#-----------------------------------------------------------------------------

PYTHON       ?= python3
RISCV_PREFIX ?= riscv64-unknown-elf-
CC            = $(RISCV_PREFIX)gcc
OBJCOPY       = $(RISCV_PREFIX)objcopy

BUILD_DIR     = build
PKG           = ../../hdl/debug/WardRV_dbg_rom_pkg.vhd

all : $(PKG)

$(PKG) debug_rom.hex debug_rom.lst : debug_rom.S gen_rom.py
	$(PYTHON) gen_rom.py

check : debug_rom.hex
	@mkdir -p $(BUILD_DIR)
	$(CC) -march=rv32i_zicsr -mabi=ilp32 -nostdlib -nostartfiles -Wl,-Ttext=0xFFFFF800 \
	      -o $(BUILD_DIR)/debug_rom.elf debug_rom.S
	$(OBJCOPY) -O binary -j .text $(BUILD_DIR)/debug_rom.elf $(BUILD_DIR)/debug_rom.bin
	od -An -v -tx4 -w4 $(BUILD_DIR)/debug_rom.bin | tr -d ' ' > $(BUILD_DIR)/debug_rom.hex
	diff $(BUILD_DIR)/debug_rom.hex debug_rom.hex && echo "debug ROM : gen_rom.py and GNU as match"

clean :
	rm -rf $(BUILD_DIR)

.PHONY : all check clean
