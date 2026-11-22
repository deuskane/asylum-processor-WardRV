-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_trigger_ge_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_trigger_ge_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario trigger_ge of tb_WardRV_debug : TRIGGER_RANGE_ENABLE, execute trigger, match >=
--              (fires on pc = tdata2)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_debug_pkg.vhd)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_trigger_ge_pkg is

  -- TRIGGER_RANGE_ENABLE, execute trigger, match >=
  -- (fires on pc = tdata2)
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_trigger_ge_pkg;

package body tb_WardRV_debug_trigger_ge_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);
    -- pc >= beqz : the first instruction of the loop that matches is beqz
    trigger_set(jtag_ini, jtag_tgt, 0, x"68001144", C_BEQZ);
    trigger_run(jtag_ini, jtag_tgt, C_LOOP, C_BEQZ, "trigger >=");
  end procedure;

end package body tb_WardRV_debug_trigger_ge_pkg;
