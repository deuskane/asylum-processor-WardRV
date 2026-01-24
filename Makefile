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
#-----------------------------------------------------------------------------

#=============================================================================
# Variables
#=============================================================================
SHELL            = /bin/bash

include mk/defs.mk

FILE_TARGETS     = mk/targets.txt

CORE_NAME       := $(shell grep ^name $(FILE_CORE) | head -n1 | tr -d ' ')

IP_VENDOR        = $(shell echo $(CORE_NAME) | cut -d':' -f2)
IP_LIBRARY       = $(shell echo $(CORE_NAME) | cut -d':' -f3)
IP_NAME          = $(shell echo $(CORE_NAME) | cut -d':' -f4)
IP_VERSION       = $(shell echo $(CORE_NAME) | cut -d':' -f5)
VLNV             = $(IP_VENDOR):$(IP_LIBRARY):$(IP_NAME):$(IP_VERSION)

TARGETS_SIM     := $(shell cat $(FILE_TARGETS) | grep sim_   | cut -d ':' -f1 | tr -d ' ')
TARGETS_EMU     := $(shell cat $(FILE_TARGETS) | grep emu_   | cut -d ':' -f1 | tr -d ' ')
TARGETS_LINT    := $(shell cat $(FILE_TARGETS) | grep lint_  | cut -d ':' -f1 | tr -d ' ')

PATH_BUILD      ?= $(CURDIR)/build

FUSESOC_OPT     += --build-root $(PATH_BUILD)
FUSESOC_OPT     += --no-export

NONREG          ?= SIM

#=============================================================================
# Rules
#=============================================================================

#--------------------------------------------------------
# Display list of target
help : $(FILE_TARGETS)
#--------------------------------------------------------
	@echo ""
	@echo ">>>>>>>  Makefile Help"
	@echo ""
	@echo "==============| Variables"
	@echo "VLNV          : Vendor/Library/Name/Version"
	@echo "                $(VLNV)"
	@echo "TARGET        : Specific Target for Fusesoc"
	@echo "                $(TARGET)"
	@echo "TOOL          : Specific Tool for Fusesoc"
	@echo "                $(TOOL)"
	@echo "TARGETS_SIM   : All simulation targets"
	@for target in $(TARGETS_SIM); do \
	 echo "                * $${target}"; \
	 done
	@echo "TARGETS_EMU   : All emulation targets"
	@for target in $(TARGETS_EMU); do \
	 echo "                * $${target}"; \
	 done
	@echo "TARGETS_LINT  : All lint & static checks targets"
	@for target in $(TARGETS_LINT); do \
	 echo "                * $${target}"; \
	 done
	@echo "NONREG        : Non Regression Type (SIM/EMU/LINT)"
	@echo "                $(NONREG)"
	@echo "PATH_BUILD    : Path to build directory"
	@echo "                $(PATH_BUILD)"
	@echo ""
	@echo "==============| Rules"
	@echo "help          : Print this message"
	@echo "info          : Display library list and cores list"
	@echo "clean         : delete build directory"
	@echo "nonreg        : Run all simulation targets"
	@echo ""
	@echo "target        : Execute all   stages of fusesoc flow for specific target and tool"
	@echo "setup         : Execute Setup stage  of fusesoc flow for specific target and tool"
	@echo "build         : Execute Build stage  of fusesoc flow for specific target and tool"
	@echo "run           : Execute Run   stage  of fusesoc flow for specific target and tool"
	@echo "*             : Run target with default tool"
	@echo ""
	@echo "==============| Targets"
	@echo ""
	@cat $(FILE_TARGETS)

.PHONY  : help

#--------------------------------------------------------
# Generate the Information file
$(FILE_TARGETS) : $(FILE_CORE)
#--------------------------------------------------------
	@fusesoc core show $(VLNV) | awk '/Targets:/{flag=1; next} flag' > $(FILE_TARGETS)

#--------------------------------------------------------
# Display library list and cores list
info :
#--------------------------------------------------------
	@fusesoc library list
	@fusesoc gen     list
	@fusesoc core    list

.PHONY : info

#--------------------------------------------------------
target :
#--------------------------------------------------------
	fusesoc run $(FUSESOC_OPT) --target $(TARGET) --tool $(TOOL) $(VLNV)

.PHONY : target

#--------------------------------------------------------
setup build run :
#--------------------------------------------------------
	fusesoc run $(FUSESOC_OPT) --$@ --target $(TARGET) --tool $(TOOL) $(VLNV)

.PHONY : setup build run

#--------------------------------------------------------
% :
#--------------------------------------------------------
	@fusesoc run $(FUSESOC_OPT) --target $* $(VLNV)

#--------------------------------------------------------
nonreg : $(TARGETS_$(NONREG))
#--------------------------------------------------------
# nothing

.PHONY : nonreg

#--------------------------------------------------------
clean :
#--------------------------------------------------------
	rm -fr $(PATH_BUILD)

.PHONY : clean
