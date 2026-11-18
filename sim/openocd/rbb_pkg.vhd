-------------------------------------------------------------------------------
-- Title      : WardRV Debug - OpenOCD remote_bitbang binding
-- Project    : 
-------------------------------------------------------------------------------
-- File       : rbb_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: GHDL VHPIDIRECT binding of rbb_tick (remote_bitbang.c, built
--              as ./librbb.so in the simulation directory by the pre_build
--              hook of the sim_dbg_openocd_* targets).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-------------------------------------------------------------------------------

package rbb_pkg is

  -- Bits of the value returned by rbb_tick
  constant RBB_TCK  : natural := 0;
  constant RBB_TMS  : natural := 1;
  constant RBB_TDI  : natural := 2;
  constant RBB_QUIT : natural := 3;                               -- OpenOCD quit

  -- Commands of OpenOCD (at most one pin write), tdo : 0 / 1
  impure function rbb_tick(tdo : integer) return integer;
  attribute foreign of rbb_tick : function is "VHPIDIRECT ./librbb.so rbb_tick";

end package rbb_pkg;

package body rbb_pkg is

  impure function rbb_tick(tdo : integer) return integer is
  begin
    report "rbb_tick : VHPIDIRECT function not bound (./librbb.so)" severity failure;
    return 2**RBB_QUIT;
  end function rbb_tick;

end package body rbb_pkg;
