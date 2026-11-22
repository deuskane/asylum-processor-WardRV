-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_trigger_lt_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_trigger_lt_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario trigger_lt of tb_WardRV_debug : TRIGGER_RANGE_ENABLE, execute trigger, match <
--              (not on pc = tdata2)
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

library asylum_uvvm;
use     asylum_uvvm.jtag_bfm_pkg.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_trigger_lt_pkg is

  -- TRIGGER_RANGE_ENABLE, execute trigger, match <
  -- (not on pc = tdata2)
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_trigger_lt_pkg;

package body tb_WardRV_debug_trigger_lt_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
  begin
    connect    (jtag_if);
    halt       (jtag_if);
    -- pc < sw : resumed on the sw, the trigger fires on the addi of the next loop
    trigger_set(jtag_if, 0, x"680011C4", C_SW_COUNTER);
    trigger_run(jtag_if, C_SW_COUNTER, C_LOOP, "trigger <");
  end procedure;

end package body tb_WardRV_debug_trigger_lt_pkg;
