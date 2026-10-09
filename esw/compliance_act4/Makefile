# Makefile for WardRV Compliance Tests (ACT4 flow)
#
# Full flow :
#   make tools       : download toolchain, Sail reference model and mise (uv, ruby)
#   make fetch       : clone riscv-arch-test (tag $(ARCH_TEST_VERSION))
#   make elfs        : generate the self-checking ELFs with the ACT4 framework
#   make all         : convert the ELFs into .hex and .lst in $(BUILD_DIR)
#   make check       : compare $(BUILD_DIR) with $(BENCH_DIR)
#   make update      : copy $(BUILD_DIR) hex/lst into $(BENCH_DIR)
#   make full        : tools + fetch + elfs + all + check

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
WS_DIR             = $(CURDIR)/riscv-compliance-ws

# Directory of the tools (toolchain, Sail, mise, uv, riscv-arch-test), downloaded if missing.
# The docker image provides them in /opt (see docker/Dockerfile)
TOOLS_DIR         ?= $(WS_DIR)
BIN_DIR            = $(TOOLS_DIR)/bin

# riscv-arch-test : ACT4 framework
ARCH_TEST_URL      = https://github.com/riscv/riscv-arch-test.git
ARCH_TEST_VERSION ?= 4.1.0
ARCH_TEST_DIR      = $(TOOLS_DIR)/src/riscv-arch-test

# Toolchain : ACT4 requires binutils >= 2.44
TOOLCHAIN_VERSION ?= 15.2.0-1
TOOLCHAIN_URL      = https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack/releases/download/v$(TOOLCHAIN_VERSION)/xpack-riscv-none-elf-gcc-$(TOOLCHAIN_VERSION)-linux-x64.tar.gz
TOOLCHAIN_DIR      = $(TOOLS_DIR)/toolchain/xpack-riscv-none-elf-gcc-$(TOOLCHAIN_VERSION)

# Sail RISC-V reference model (compute the expected results)
SAIL_VERSION      ?= 0.13.1
SAIL_URL           = https://github.com/riscv/sail-riscv/releases/download/$(SAIL_VERSION)/sail-riscv-Linux-x86_64.tar.gz
SAIL_DIR           = $(TOOLS_DIR)/sail-$(SAIL_VERSION)

# mise : tool manager used by ACT4 (install uv and ruby)
MISE_VERSION      ?= v2026.10.5
MISE_URL           = https://github.com/jdx/mise/releases/download/$(MISE_VERSION)/mise-$(MISE_VERSION)-linux-x64.tar.gz
MISE_DIR           = $(TOOLS_DIR)/mise

# DUT configuration (test_config.yaml, UDB config, sail.json, rvmodel_macros.h, link.ld)
CONFIG_DIR         = $(abspath config/wardrv)
CONFIG_NAME        = $(shell sed -n 's/^name: *\([^ #]*\).*/\1/p' $(CONFIG_DIR)/test_config.yaml)

# Extensions to test (empty : all extensions of the UDB config)
EXTENSIONS        ?= I,Zicsr

WORK_DIR           = $(WS_DIR)/work
ELF_DIR            = $(WORK_DIR)/$(CONFIG_NAME)/elfs
BUILD_DIR          = build
BENCH_DIR          = benchs

# All tools are installed in TOOLS_DIR
export PATH            := $(BIN_DIR):$(MISE_DIR)/bin:$(PATH)
export MISE_DATA_DIR   := $(MISE_DIR)/data
export MISE_CACHE_DIR  := $(MISE_DIR)/cache
export MISE_STATE_DIR  := $(MISE_DIR)/state
export MISE_CONFIG_DIR := $(MISE_DIR)/config
export MISE_YES        := 1
export UV_CACHE_DIR    := $(TOOLS_DIR)/uv/cache
export UV_PYTHON_INSTALL_DIR := $(TOOLS_DIR)/uv/python
# UDB downloads z3 in $XDG_CACHE_HOME/udb at the first run : keep it with the tools
export XDG_CACHE_HOME  := $(TOOLS_DIR)/cache

# UDB writes its generated files in $XDG_DATA_HOME/udb and uses flock on them :
# it must be a local disk (flock fails on NFS)
UDB_DATA_DIR      ?= /tmp/asylum-compliance-act4-$(shell id -u)
export XDG_DATA_HOME   := $(UDB_DATA_DIR)

OBJCOPY      = $(TOOLCHAIN_DIR)/bin/riscv-none-elf-objcopy
OBJDUMP      = $(TOOLCHAIN_DIR)/bin/riscv-none-elf-objdump

# -----------------------------------------------------------------------------
# Files
# -----------------------------------------------------------------------------
ELFS         = $(shell find $(ELF_DIR) -name '*.elf' 2>/dev/null)
HEXS         = $(patsubst %.elf, $(BUILD_DIR)/%.hex, $(notdir $(ELFS)))
LSTS         = $(patsubst %.elf, $(BUILD_DIR)/%.lst, $(notdir $(ELFS)))

vpath %.elf $(sort $(dir $(ELFS)))

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------
.PHONY: all full tools toolchain sail mise fetch elfs check update clean distclean

.SECONDARY:

all: $(HEXS) $(LSTS)

full: tools fetch
	$(MAKE) elfs
	$(MAKE) all
	$(MAKE) check

tools: toolchain sail mise

# The DUT configuration uses the riscv32-unknown-elf- prefix
toolchain: $(TOOLCHAIN_DIR)/bin/riscv-none-elf-gcc
	@mkdir -p $(BIN_DIR)
	@for tool in gcc objdump objcopy; do                                 \
	  ln -sf $(TOOLCHAIN_DIR)/bin/riscv-none-elf-$$tool $(BIN_DIR)/riscv32-unknown-elf-$$tool; \
	done

$(TOOLCHAIN_DIR)/bin/riscv-none-elf-gcc:
	@mkdir -p $(TOOLS_DIR)/toolchain
	curl -sSL $(TOOLCHAIN_URL) | tar xz -C $(TOOLS_DIR)/toolchain
	@touch $@

sail: $(SAIL_DIR)/bin/sail_riscv_sim
	@mkdir -p $(BIN_DIR)
	ln -sf $(SAIL_DIR)/bin/sail_riscv_sim $(BIN_DIR)/sail_riscv_sim

$(SAIL_DIR)/bin/sail_riscv_sim:
	@mkdir -p $(SAIL_DIR)
	curl -sSL $(SAIL_URL) | tar xz -C $(SAIL_DIR) --strip-components=1
	@touch $@

mise: $(MISE_DIR)/bin/mise

$(MISE_DIR)/bin/mise:
	@mkdir -p $(MISE_DIR)
	curl -sSL $(MISE_URL) | tar xz -C $(MISE_DIR) --strip-components=1
	@touch $@

fetch: $(ARCH_TEST_DIR)/.git

$(ARCH_TEST_DIR)/.git:
	@mkdir -p $(dir $(ARCH_TEST_DIR))
	git clone --depth 1 -b $(ARCH_TEST_VERSION) $(ARCH_TEST_URL) $(ARCH_TEST_DIR)
	cd $(ARCH_TEST_DIR) && mise trust .mise.toml && mise install

# Generate the tests, run them on Sail and build the self-checking ELFs
elfs:
	$(MAKE) -C $(ARCH_TEST_DIR)                                          \
	  CONFIG_FILES=$(CONFIG_DIR)/test_config.yaml                        \
	  WORKDIR=$(WORK_DIR)                                                \
	  EXTENSIONS=$(EXTENSIONS)

# The debug informations contain the absolute path of the workspace :
# remove them to have reproducible files
$(BUILD_DIR)/%.elf: %.elf
	@mkdir -p $(dir $@)
	$(OBJCOPY) --strip-debug $< $@

$(BUILD_DIR)/%.bin: $(BUILD_DIR)/%.elf
	$(OBJCOPY) -O binary $< $@

$(BUILD_DIR)/%.hex: $(BUILD_DIR)/%.bin
	hexdump -v -e '1/4 "%08x\n"' $< > $@

$(BUILD_DIR)/%.lst: $(BUILD_DIR)/%.elf
	$(OBJDUMP) -D $< > $@

# Compare generated files with the benchs
check:
	@status=0;                                                          \
	for ref in $(wildcard $(BENCH_DIR)/*.hex $(BENCH_DIR)/*.lst); do    \
	  f=$(BUILD_DIR)/$$(basename $$ref);                                \
	  if   [ ! -f $$f ];        then echo "[MISSING] $$f"; status=1;    \
	  elif cmp -s $$ref $$f;    then echo "[OK]      $$f";              \
	  else                           echo "[DIFF]    $$f"; status=1;    \
	  fi;                                                               \
	done;                                                               \
	exit $$status

update: all
	@mkdir -p $(BENCH_DIR)
	cp $(HEXS) $(LSTS) $(BENCH_DIR)/

clean:
	rm -rf $(BUILD_DIR)

distclean: clean
	rm -rf $(WS_DIR)
