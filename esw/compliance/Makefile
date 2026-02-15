# Makefile for WardRV Compliance Tests

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
RISCV_PREFIX ?= riscv64-unknown-elf-
CC           = $(RISCV_PREFIX)gcc
OBJCOPY      = $(RISCV_PREFIX)objcopy
OBJDUMP      = $(RISCV_PREFIX)objdump

# Path to the tests source code
TEST_DIR     = riscv-non-isa/riscv-test-suite/rv32i_m/I/src
BUILD_DIR    = build_compliance

# -----------------------------------------------------------------------------
# Files
# -----------------------------------------------------------------------------
SRCS = $(wildcard $(TEST_DIR)/*.S)
OBJS = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.o, $(SRCS))
ELFS = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.elf, $(SRCS))
HEXS = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.hex, $(SRCS))
LSTS = $(patsubst $(TEST_DIR)/%.S, $(BUILD_DIR)/%.lst, $(SRCS))

# -----------------------------------------------------------------------------
# Flags
# -----------------------------------------------------------------------------
# Include paths: Current dir (for model_test.h) and Suite includes
INCLUDES = -I. -I$(TEST_DIR)/../../../../include

CFLAGS   = -march=rv32i -mabi=ilp32 -static -mcmodel=medany -g \
           -fvisibility=hidden -nostdlib -nostartfiles $(INCLUDES)

LDFLAGS  = -T link.ld -Wl,--no-warn-rwx-segments

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------
.PHONY: all clean

all: $(HEXS) $(LSTS)

$(BUILD_DIR)/%.o: $(TEST_DIR)/%.S
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.elf: $(BUILD_DIR)/%.o
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

$(BUILD_DIR)/%.hex: $(BUILD_DIR)/%.elf
	$(OBJCOPY) -O verilog $< $@

$(BUILD_DIR)/%.lst: $(BUILD_DIR)/%.elf
	$(OBJDUMP) -D -S $< > $@

clean:
	rm -rf $(BUILD_DIR)
