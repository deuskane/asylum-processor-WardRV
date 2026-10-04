#-----------------------------------------------------------------------------
# Title      : Makefile
# Project    : PicoSoC
#-----------------------------------------------------------------------------
# File       : PicoSoC.vhd
# Author     : Mathieu Rosiere
#-----------------------------------------------------------------------------
# Description: Makefile to execute fusesoc
#-----------------------------------------------------------------------------
# Copyright (c) 2024
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2024-12-31  1.0      mrosiere	Created
# 2025-01-22  1.1      mrosiere Delete impulse target
# 2026-09-29  1.2      mrosiere Add all nonreg variant depends of type and step
# 2026-10-01  1.3      mrosiere Add log directory and log file for each target
# 2026-10-04  1.4      mrosiere Add ci_generate target to generate GitHub Actions jobs
#-----------------------------------------------------------------------------

#=============================================================================
# Variables
#=============================================================================
SHELL                = /bin/bash

include mk/defs.mk

# Fusesoc Options
PATH_BUILD          ?= $(CURDIR)/build
PATH_LOG            ?= $(CURDIR)/log

FUSESOC_CACHE        = ~/.cache/fusesoc
FUSESOC_OPT          = --cores-root .
FUSESOC_RUN_OPT     += --build-root $(PATH_BUILD)
FUSESOC_RUN_OPT     += --no-export

GHDL_ANALYZE_OPTION :=
GHDL_RUN_OPTION     :=

ifneq ($(CI),true)
# If not CI, add option to generate waveform file for GHDL simulation
GHDL_RUN_OPTION     += --fst=dut.fst
endif

export GHDL_ANALYZE_OPTION
export GHDL_RUN_OPTION

# IP parameters
CORE_NAME           := $(shell grep ^name $(FILE_CORE) | head -n1 | tr -d ' ')

IP_VENDOR            = $(shell echo $(CORE_NAME) | cut -d':' -f2)
IP_LIBRARY           = $(shell echo $(CORE_NAME) | cut -d':' -f3)
IP_NAME              = $(shell echo $(CORE_NAME) | cut -d':' -f4)
IP_VERSION           = $(shell echo $(CORE_NAME) | cut -d':' -f5)
VLNV                 = $(IP_VENDOR):$(IP_LIBRARY):$(IP_NAME):$(IP_VERSION)

space               := $(empty) $(empty)
LOG_NAME             = $@-$(if $(strip $(STEP)),$(subst $(space),_,$(strip $(STEP))),$(empty)).log

CI_WORKFLOW          = .github/workflows/ci.yml

# Targets generations
FILE_TARGETS         = mk/targets.txt
TARGETS_FILTER      ?= .

TARGETS_ALL         := $(shell cat $(FILE_TARGETS) | cut -d ':' -f1 | tr -d ' ')
TARGETS_SIM         := $(shell grep -e ^sim_  $(FILE_TARGETS) | grep -E -e '$(TARGETS_FILTER)' | cut -d ':' -f1 | tr -d ' ')
TARGETS_EMU         := $(shell grep -e ^emu_  $(FILE_TARGETS) | grep -E -e '$(TARGETS_FILTER)' | cut -d ':' -f1 | tr -d ' ')
TARGETS_LINT        := $(shell grep -e ^lint_ $(FILE_TARGETS) | grep -E -e '$(TARGETS_FILTER)' | cut -d ':' -f1 | tr -d ' ')

# Non regression
NONREG_TYPES        := sim emu lint
NONREG_STEPS        := setup build run
NONREG_RULES        := $(foreach type,$(NONREG_TYPES),nonreg_$(type)) 
NONREG_RULES        += $(foreach type,$(NONREG_TYPES),$(foreach step,$(NONREG_STEPS),nonreg_$(type)_$(step)))

NONREG              ?= sim
STEP                ?= $(NONREG_STEPS)

#=============================================================================
# Rules
#=============================================================================

#--------------------------------------------------------
# Display list of target
help : $(FILE_TARGETS)
#--------------------------------------------------------
	@echo ""
	@echo "=====================| Variables"
	@echo "VLNV                 : Vendor/Library/Name/Version"
	@echo "                       $(VLNV)"
	@echo "TOOL                 : Specific Tool for Fusesoc"
	@echo "                       $(TOOL)"
	@echo "TARGETS_FILTER       : Filter for targets (regex)"
	@echo "                       $(TARGETS_FILTER)"
	@echo "                       example: ^sim_m or ^sim_m.*"
	@echo "TARGETS_SIM          : All simulation targets (after filter)"
	@for target in $(TARGETS_SIM); do \
	 echo "                       * $${target}"; \
	 done
	@echo "TARGETS_EMU          : All emulation targets (after filter)"
	@for target in $(TARGETS_EMU); do \
	 echo "                       * $${target}"; \
	 done
	@echo "TARGETS_LINT         : All lint & static checks targets (after filter)"
	@for target in $(TARGETS_LINT); do \
	 echo "                       * $${target}"; \
	 done
	@echo "TARGET               : Define specfic Target for Fusesoc"
	@echo "                       $(TARGET)"
	@echo "NONREG               : Define specific Non Regression Type ($(NONREG_TYPES))"
	@echo "                       $(NONREG)"
	@echo "STEP                 : Define specific Step for Fusesoc ($(NONREG_STEPS))"
	@echo "                       $(STEP)"
	@echo "PATH_BUILD           : Path to build directory"
	@echo "                       $(PATH_BUILD)"
	@echo ""
	@echo "=====================| Rules"
	@echo "help                 : Print this message"
	@echo "info                 : Display library list and cores list"
	@echo "update               : Update the FuseSoC core libraries"
	@echo "ci_generate          : Regenerate the GitHub Actions job block between markers"
	@echo "clean                : Delete build directory"
	@echo "nonreg               : Run all targets for type set in NONREG"
	@echo "                       Aka nonreg_$(NONREG)"
	@echo "nonreg_<type>        : Run all steps for all <type> targets"
	@echo "                       Type is $(NONREG_TYPES)"
	@echo "nonreg_<type>_<step> : Run <step> for all <type> targets"
	@echo "                       Type is $(NONREG_TYPES)"
	@echo "                       Step is $(NONREG_STEPS)"
	@echo ""
	@echo "target               : Execute all   stages of fusesoc flow for specific target and tool"
	@echo "setup                : Execute Setup stage  of fusesoc flow for specific target and tool"
	@echo "build                : Execute Build stage  of fusesoc flow for specific target and tool"
	@echo "run                  : Execute Run   stage  of fusesoc flow for specific target and tool"
	@echo "*                    : Run target with default tool"
	@echo ""
	@echo "=====================| Targets"
	@echo ""
	@cat $(FILE_TARGETS)

.PHONY  : help

#--------------------------------------------------------
# Generate the Information file
$(FILE_TARGETS) : $(FILE_CORE)
#--------------------------------------------------------
	@fusesoc $(FUSESOC_OPT) core show $(VLNV) | awk '/Targets:/{flag=1; next} flag' > $(FILE_TARGETS)

#--------------------------------------------------------
# Display library list and cores list
info :
#--------------------------------------------------------
	@fusesoc $(FUSESOC_OPT) library list
	@fusesoc $(FUSESOC_OPT) gen     list
	@fusesoc $(FUSESOC_OPT) core    list

.PHONY : info

#--------------------------------------------------------
# Update the FuseSoC core libraries
update :
#--------------------------------------------------------
	@fusesoc $(FUSESOC_OPT) library update

.PHONY : update

#--------------------------------------------------------
target :
#--------------------------------------------------------
	fusesoc $(FUSESOC_OPT) run $(FUSESOC_RUN_OPT) --target $(TARGET) --tool $(TOOL) $(VLNV)

.PHONY : target

#--------------------------------------------------------
setup build run :
#--------------------------------------------------------
	fusesoc $(FUSESOC_OPT) run $(FUSESOC_RUN_OPT) --$@ --target $(TARGET) --tool $(TOOL) $(VLNV)

.PHONY : setup build run

#--------------------------------------------------------
$(TARGETS_ALL) : | $(PATH_LOG)
#--------------------------------------------------------
	@set -o pipefail; fusesoc $(FUSESOC_OPT) run $(FUSESOC_RUN_OPT) $(addprefix --,$(STEP)) --target $@ $(VLNV) | tee $(PATH_LOG)/$(LOG_NAME)

.PHONY : $(TARGETS_ALL)

#--------------------------------------------------------
$(PATH_LOG) :
#--------------------------------------------------------
	@mkdir -p $@

#--------------------------------------------------------
# Create the CI workflow file and add generation markers if missing
$(CI_WORKFLOW) : $(FILE_TARGETS)
#--------------------------------------------------------
	@set -e; \
	file="$@"; \
	mkdir -p "$$(dirname "$$file")"; \
	if [ ! -f "$$file" ]; then \
		printf '%s\n' 'name: CI' '' 'on:' '  push:' '    branches: [ main, develop ]' '  pull_request:' '    branches: [ main, develop ]' '#<GENERATE_BEGIN>' '#<GENERATE_END>' > "$$file"; \
	fi; \
	if ! grep -q '#<GENERATE_BEGIN>' "$$file" || ! grep -q '#<GENERATE_END>' "$$file"; then \
		printf '\n#<GENERATE_BEGIN>\n#<GENERATE_END>\n' >> "$$file"; \
	fi

#--------------------------------------------------------
# Generate CI jobs between markers only
ci_generate : $(CI_WORKFLOW) $(FILE_TARGETS)
#--------------------------------------------------------
	@python3 -c "import sys; from pathlib import Path; targets = [t for t in sys.argv[1].split() if t]; path = Path(sys.argv[2]); text = path.read_text(); start = '#<GENERATE_BEGIN>'; end = '#<GENERATE_END>'; assert start in text and end in text, f'Markers {start!r} and {end!r} not found in {path}'; start_idx = text.index(start); end_idx = text.index(end, start_idx); jobs = ''.join(f'  {t}:\n    uses: deuskane/asylum-ci/.github/workflows/vhdl-ci.yml@main\n    with:\n      name: {t}\n\n' for t in targets); jobs_header = 'jobs:' if targets else '# jobs:'; new_text = text[:start_idx] + start + '\n' + jobs_header + ('\n' + jobs if targets else '') + '\n' + end + text[end_idx + len(end):]; path.write_text(new_text)" "$(TARGETS_SIM)" "$<"

#--------------------------------------------------------
# Generate nonreg_<type> and nonreg_<type>_<stage> rules
NONREG_UPPER = $(shell printf '%s' '$(1)' | tr '[:lower:]' '[:upper:]')

define NONREG_GROUP_TEMPLATE
ifneq ($(strip $(STEP)),)
nonreg_$(1) :
	+$(MAKE) --no-print-directory $(addprefix nonreg_$(1)_,$(STEP));
else
nonreg_$(1) :
	@:
endif
endef

define NONREG_STEP_TEMPLATE
ifneq ($(strip $$(TARGETS_$(call NONREG_UPPER,$(1)))),)
nonreg_$(1)_$(2) :
	+$(MAKE) --no-print-directory $$(TARGETS_$(call NONREG_UPPER,$(1))) STEP=$(2);
else
nonreg_$(1)_$(2) :
	@:
endif
endef

$(foreach type,$(NONREG_TYPES),$(eval $(call NONREG_GROUP_TEMPLATE,$(type))))
$(foreach type,$(NONREG_TYPES),$(foreach step,$(NONREG_STEPS),$(eval $(call NONREG_STEP_TEMPLATE,$(type),$(step)))))

.PHONY : $(NONREG_RULES)

#--------------------------------------------------------
ifneq ($(strip $(NONREG)),)
nonreg : nonreg_$(NONREG)
else
nonreg :
	@:
endif
#--------------------------------------------------------
# nothing

.PHONY : nonreg

#--------------------------------------------------------
clean :
#--------------------------------------------------------
	rm -fr $(FUSESOC_CACHE)/generator_cache
	rm -fr $(PATH_BUILD)
	rm -fr $(PATH_LOG)

.PHONY : clean
