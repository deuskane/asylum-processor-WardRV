#!/bin/bash
#-----------------------------------------------------------------------------
# Title      : Synthesis check of WardRV with ghdl --synth -Wall (post_run hook)
# Project    : Asylum
#-----------------------------------------------------------------------------
# File       : ghdl_synth.sh
# Author     : mrosiere
#-----------------------------------------------------------------------------
# Description: post_run hook of the lint_ghdl_synth target (cwd = fusesoc
#              work_root, after the run step : the ghdl -m of edalize has
#              analyzed (-Wall) WardRV_synth_top (library work) and library
#              asylum). Synthesizes WardRV_synth_top (core + debug unit) for
#              each configuration below. Writes synth_<config>.vhd (netlist)
#              and synth_<config>.log, prints the warnings of each
#              configuration, fails if a synthesis fails or infers a latch.
#-----------------------------------------------------------------------------
# Copyright (c) 2026
#-----------------------------------------------------------------------------
# Revisions  :
# Date        Version  Author   Description
# 2026-10-10  1.0      mrosiere Created
# 2026-10-20  1.1      mrosiere -Wall, print the warnings of each configuration
#-----------------------------------------------------------------------------

TOP=WardRV_synth_top
GHDL_OPT="--std=08 -Wall -fsynopsys -frelaxed -P./asylum"

# name : generics
CONFIGS=(
    "nodebug       : -gDEBUG_ENABLE=false"
    "debug         : -gDEBUG_ENABLE=true -gNB_TRIGGERS=0"
    "debug_trig2   : -gDEBUG_ENABLE=true -gNB_TRIGGERS=2"
    "debug_sba     : -gDEBUG_ENABLE=true -gNB_TRIGGERS=0 -gSBA_ENABLE=true"
    "debug_full    : -gDEBUG_ENABLE=true -gNB_TRIGGERS=2 -gSBA_ENABLE=true -gEBREAK_EXCEPTION=true -gRESETHALTREQ_ENABLE=true"
    "debug_large   : -gDEBUG_ENABLE=true -gNB_TRIGGERS=4 -gPROGBUF_SIZE=8 -gDATA_COUNT=12"
)

nb_fail=0
for config in "${CONFIGS[@]}"; do
    name=$(echo "${config%%:*}" | tr -d ' ')
    generics=${config#*:}

    # shellcheck disable=SC2086
    ghdl --synth $GHDL_OPT $generics $TOP > "synth_${name}.vhd" 2> "synth_${name}.log"
    rc=$?

    if [ $rc -ne 0 ]; then
        status="FAIL (ghdl rc=$rc)"
    elif grep -qi "latch" "synth_${name}.log"; then
        status="FAIL (latch)"
    else
        status="PASS"
    fi
    [ "$status" = "PASS" ] || nb_fail=$((nb_fail + 1))
    nb_warn=$(grep -ci "warning" "synth_${name}.log")

    printf "ghdl --synth %-14s : %s (%d warnings)\n" "$name" "$status" "$nb_warn"
    if [ "$status" != "PASS" ]; then
        sed 's/^/    /' "synth_${name}.log"
    elif [ "$nb_warn" -ne 0 ]; then
        grep -i "warning" "synth_${name}.log" | sed 's/^/    /'
    fi
done

if [ $nb_fail -ne 0 ]; then
    echo "lint_ghdl_synth : FAIL ($nb_fail / ${#CONFIGS[@]} configurations)"
    exit 1
fi
echo "lint_ghdl_synth : PASS (${#CONFIGS[@]} configurations)"
