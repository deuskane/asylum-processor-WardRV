-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_trigger_range_store_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_trigger_range_store_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario trigger_range_store of tb_WardRV_debug : TRIGGER_RANGE_ENABLE, chain >= A, < B on
--              the store address
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
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_trigger_range_store_pkg is

  -- TRIGGER_RANGE_ENABLE, chain >= A, < B on
  -- the store address
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_trigger_range_store_pkg;

package body tb_WardRV_debug_trigger_range_store_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);
    -- Range [flag, flag + 4) of stores : the store of the counter matches
    -- the < trigger only, the store that clears the flag both
    trigger_set(jtag_ini, jtag_tgt, 1, x"680011C2", std_logic_vector(unsigned(C_FLAG) + 4));
    trigger_set(jtag_ini, jtag_tgt, 0, x"68001942", C_FLAG);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");          -- ebreak (NOP), then flag cleared
    trigger_run(jtag_ini, jtag_tgt, C_LOOP, C_SW_FLAG, "range trigger on the store address");
    mem_read   (jtag_ini, jtag_tgt, C_FLAG, v_data);
    check_value(v_data, x"00000001", ERROR, "store not executed (timing before)", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_trigger_range_store_pkg;
