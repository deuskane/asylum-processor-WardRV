# Makefile for WardRV Compliance Tests (ACT3 flow)
#
# Full flow :
#   make toolchain   : download the xPack RISC-V toolchain (binutils 2.42)
#   make sail        : download the Sail RISC-V reference model
#   make fetch       : clone riscv-arch-test (tag $(ARCH_TEST_VERSION))
#   make all         : build .hex, .lst and .signature (Sail) in $(BUILD_DIR)
#   make check       : compare $(BUILD_DIR) with $(BENCH_DIR)
#   make update      : copy $(BUILD_DIR) hex/lst/signature into $(BENCH_DIR)
#   make full        : toolchain + sail + fetch + all + check

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
WS_DIR             = riscv-compliance-ws

# Directory of the tools (toolchain, Sail, riscv-arch-test), downloaded if missing.
# The docker image provides them in /opt (see docker/Dockerfile)
TOOLS_DIR         ?= $(WS_DIR)

# riscv-arch-test : last tag of the old framework (ACT3)
ARCH_TEST_URL      = https://github.com/riscv/riscv-arch-test.git
ARCH_TEST_VERSION ?= 3.10.0
ARCH_TEST_DIR      = $(TOOLS_DIR)/src/riscv-arch-test

# Toolchain : binutils 2.42 is required to reproduce the benchs
# (binutils >= 2.43 keeps c.nop in the alignment padding)
TOOLCHAIN_VERSION ?= 13.3.0-1
TOOLCHAIN_URL      = https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack/releases/download/v$(TOOLCHAIN_VERSION)/xpack-riscv-none-elf-gcc-$(TOOLCHAIN_VERSION)-linux-x64.tar.gz
TOOLCHAIN_DIR      = $(TOOLS_DIR)/toolchain/xpack-riscv-none-elf-gcc-$(TOOLCHAIN_VERSION)

# Sail RISC-V reference model : generate the reference signatures
SAIL_VERSION      ?= 0.13.1
SAIL_URL           = https://github.com/riscv/sail-riscv/releases/download/$(SAIL_VERSION)/sail-riscv-Linux-x86_64.tar.gz
SAIL_DIR           = $(TOOLS_DIR)/sail-$(SAIL_VERSION)
SAIL               = $(SAIL_DIR)/bin/sail_riscv_sim

RISCV_PREFIX      ?= $(abspath $(TOOLCHAIN_DIR))/bin/riscv-none-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

# Path to the tests source code
TEST_DIR     = $(ARCH_TEST_DIR)/riscv-test-suite/rv32i_m/I/src
BUILD_DIR    = build
BENCH_DIR    = benchs

# DUT configuration (link.ld, model_test.h and Sail configuration)
CONFIG_DIR   = config/wardrv

# Directory where the benchs were originally built.
# It is written in the debug informations, so it is mandatory to have identical .lst
# Set it empty to keep the real build directory
REF_BUILD_DIR ?= /home/kane/Work/Repo/ip/asylum-processor-WardRV/esw/compliance

# -----------------------------------------------------------------------------
# Files
# -----------------------------------------------------------------------------
SRCS         = $(wildcard $(TEST_DIR)/*.S)
OBJS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.o, $(SRCS))
ELFS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.elf, $(SRCS))
BINS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.bin, $(SRCS))
HEXS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.hex, $(SRCS))
LSTS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.lst, $(SRCS))
SIGS         = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.signature, $(SRCS))

# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------
# Include paths: DUT config (for model_test.h) and Suite includes
INCLUDES     = -I$(CONFIG_DIR)
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

LDFLAGS      = -T $(CONFIG_DIR)/link.ld
LDFLAGS     += -Wl,--no-warn-rwx-segments

OBJDUMP_FLAGS = -D -S

SAIL_FLAGS    = --rv32 --config-override $(CONFIG_DIR)/sail_rv32i.json
SAIL_FLAGS   += --signature-granularity 4

# The assembler takes the build directory from $PWD (logical path) :
# force it to the physical path to have the same path than make
export PWD  := $(CURDIR)

ifneq ($(strip $(REF_BUILD_DIR)),)
# Write REF_BUILD_DIR in the debug informations instead of the current directory,
# and tell objdump where the sources really are.
CFLAGS        += -fdebug-prefix-map=$(CURDIR)=$(REF_BUILD_DIR)
ifneq ($(TOOLS_DIR),$(WS_DIR))
# The sources of the tests were in $(WS_DIR), relative to the build directory
CFLAGS        += -fdebug-prefix-map=$(abspath $(TOOLS_DIR))=$(WS_DIR)
endif
OBJDUMP_FLAGS += --prefix=$(abspath $(TOOLS_DIR)) --prefix-strip=$(words $(subst /, ,$(REF_BUILD_DIR)) $(WS_DIR))
endif

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------
.PHONY: all full toolchain sail fetch check update clean distclean

# Keep intermediate files (.o, .elf, .bin)
.SECONDARY:

all: $(HEXS) $(LSTS) $(SIGS)

full: toolchain sail fetch
	$(MAKE) all
	$(MAKE) check

toolchain: $(TOOLCHAIN_DIR)/bin/riscv-none-elf-gcc

$(TOOLCHAIN_DIR)/bin/riscv-none-elf-gcc:
	@mkdir -p $(TOOLS_DIR)/toolchain
	curl -sSL $(TOOLCHAIN_URL) | tar xz -C $(TOOLS_DIR)/toolchain
	@touch $@

sail: $(SAIL)

$(SAIL):
	@mkdir -p $(SAIL_DIR)
	curl -sSL $(SAIL_URL) | tar xz -C $(SAIL_DIR) --strip-components=1
	@touch $@

fetch: $(ARCH_TEST_DIR)/.git

$(ARCH_TEST_DIR)/.git:
	@mkdir -p $(dir $(ARCH_TEST_DIR))
	git clone --depth 1 -b $(ARCH_TEST_VERSION) $(ARCH_TEST_URL) $(ARCH_TEST_DIR)

$(BUILD_DIR)/%.o: $(TEST_DIR)/%.S
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.elf: $(BUILD_DIR)/%.o
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

$(BUILD_DIR)/%.bin: $(BUILD_DIR)/%.elf
	$(OBJCOPY) -O binary $< $@

$(BUILD_DIR)/%.hex: $(BUILD_DIR)/%.bin $(BUILD_DIR)/%.lst
	hexdump -v -e '1/4 "%08x\n"' $< > $@

$(BUILD_DIR)/%.lst: $(BUILD_DIR)/%.elf
	$(OBJDUMP) $(OBJDUMP_FLAGS) $< > $@

# Run the test on Sail to dump the reference signature
$(BUILD_DIR)/%.signature: $(BUILD_DIR)/%.elf $(CONFIG_DIR)/sail_rv32i.json
	$(SAIL) $(SAIL_FLAGS) --test-signature $@ $< > $(BUILD_DIR)/$*.sail.log

# Compare generated files with the benchs
check:
	@status=0;                                                          \
	for ref in $(BENCH_DIR)/*.hex $(BENCH_DIR)/*.lst $(BENCH_DIR)/*.signature; do \
	  f=$(BUILD_DIR)/$$(basename $$ref);                                \
	  if   [ ! -f $$f ];        then echo "[MISSING] $$f"; status=1;    \
	  elif cmp -s $$ref $$f;    then echo "[OK]      $$f";              \
	  else                           echo "[DIFF]    $$f"; status=1;    \
	  fi;                                                               \
	done;                                                               \
	exit $$status

update: all
	cp $(HEXS) $(LSTS) $(SIGS) $(BENCH_DIR)/

clean:
	rm -rf $(BUILD_DIR)

distclean: clean
	rm -rf $(WS_DIR)
