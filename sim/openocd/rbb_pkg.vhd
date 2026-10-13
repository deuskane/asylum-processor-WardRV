-------------------------------------------------------------------------------
-- Title      : OpenOCD remote_bitbang server - VHPIDIRECT declaration
-- Project    : Asylum
-------------------------------------------------------------------------------
-- File       : rbb_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: GHDL VHPIDIRECT binding of rbb_tick (sim/openocd/
--              remote_bitbang.c, built as librbb.so in the simulation
--              directory by the pre_build hook of the cosim_openocd target).
--              The library path of the foreign attribute is relative to the
--              working directory of the simulation (fusesoc work_root) :
--              GHDL (GCC and LLVM backends) links ./librbb.so when the design
--              is elaborated, mcode loads it when the simulation starts.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-------------------------------------------------------------------------------

package rbb_pkg is

  -- Bits of the value returned by rbb_tick
  constant RBB_TCK    : natural := 0;
  constant RBB_TMS    : natural := 1;
  constant RBB_TDI    : natural := 2;
  constant RBB_TRST_N : natural := 3;
  constant RBB_SRST_N : natural := 4;
  constant RBB_VALID  : natural := 5;  -- A pin write was applied by this call
  constant RBB_QUIT   : natural := 6;  -- OpenOCD quit / connection closed
  constant RBB_WIDTH  : natural := 7;

  -- Process the commands of OpenOCD (at most one pin write), tdo = 0 / 1
  impure function rbb_tick (tdo : integer) return integer;
  attribute foreign of rbb_tick : function is "VHPIDIRECT ./librbb.so rbb_tick";

end package rbb_pkg;

package body rbb_pkg is

  impure function rbb_tick (tdo : integer) return integer is
  begin
    report "rbb_tick : VHPIDIRECT function not bound (./librbb.so)" severity failure;
    return 2**RBB_QUIT;
  end function rbb_tick;

end package body rbb_pkg;
