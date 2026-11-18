//-----------------------------------------------------------------------------
// Title      : WardRV OpenOCD + GDB co-simulation firmware
// Project    : Asylum
//-----------------------------------------------------------------------------
// File       : main.c
// Author     : Mathieu Rosiere
//-----------------------------------------------------------------------------
// Description: Firmware debugged by GDB through OpenOCD in the
//              sim_dbg_openocd_* targets (sim/openocd/scenario.py relies on
//              the names, the values and the order below) :
//              - loop of LOOP_COUNT iterations : bp_func (software
//                breakpoint), watched = running sum (watchpoint)
//              - hb_func (hardware breakpoint)
//              - step_test (start.S, stepi)
//              - next_test : three consecutive source lines (next)
//              - sum of pattern[0 .. limit-1] (x/, set var), end_marker,
//                then ticks incremented forever (memory read while running)
//              Built with -O0 -g : every statement keeps its own code.
//-----------------------------------------------------------------------------
// Copyright (c) 2026
//-----------------------------------------------------------------------------
// Revisions  :
// Date        Version  Author   Description
// 2026-11-17  1.0      mrosiere Created
//-----------------------------------------------------------------------------

//--------------------------------------
// Constant
//--------------------------------------
#define LOOP_COUNT   8
#define PATTERN_SIZE 4

volatile unsigned int counter;                         // Loop index
volatile unsigned int watched;                         // Running sum (watch)
volatile unsigned int bp_hits;                         // Calls of bp_func
volatile unsigned int hb_hits;                         // Calls of hb_func
volatile unsigned int next_calls;                      // Calls of helper
volatile unsigned int limit = PATTERN_SIZE;            // Changed by GDB (set var)
volatile unsigned int pattern[PATTERN_SIZE] =          // Read (x/) and changed by GDB
  {0x11111111, 0x22222222, 0x33333333, 0x44444444};
volatile unsigned int result;                          // Sum of pattern[0 .. limit-1]
volatile unsigned int finished;                        // Written by end_marker
volatile unsigned int ticks;                           // Incremented after the end

extern unsigned int step_test (void);

//--------------------------------------
// Functions
//--------------------------------------
// Software breakpoint (break)
unsigned int __attribute__((noinline)) bp_func (unsigned int x)
{
  bp_hits++;
  return x + 1;
}

// Hardware breakpoint (hbreak)
void __attribute__((noinline)) hb_func (void)
{
  hb_hits++;
}

void __attribute__((noinline)) helper (void)
{
  next_calls++;
}

// next : three consecutive lines
void __attribute__((noinline)) next_test (void)
{
  helper();
  helper();
  next_calls += 10;
}

// End : runs forever
void __attribute__((noinline)) end_marker (unsigned int r)
{
  finished = r;
  while (1)
    ticks++;
}

//--------------------------------------
// Main
//--------------------------------------
int main (void)
{
  unsigned int sum = 0;
  unsigned int i;

  for (counter = 0; counter < LOOP_COUNT; counter++)
    {
      sum     += bp_func(counter);
      watched  = sum;
    }

  hb_func  ();
  step_test();
  next_test();

  result = 0;
  for (i = 0; i < limit; i++)
    result += pattern[i];

  end_marker(result);
  return 0;
}
