-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_reset_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_reset_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario reset of tb_WardRV_debug : after reset, the core runs its firmware
--              (allrunning, counter in memory), Debug Module idle
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

package tb_WardRV_debug_reset_pkg is

  -- after reset, the core runs its firmware
  -- (allrunning, counter in memory), Debug Module idle
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; signal mem : in ram_t);

end package tb_WardRV_debug_reset_pkg;

package body tb_WardRV_debug_reset_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; signal mem : in ram_t) is
    variable v_data   : std_logic_vector(32-1 downto 0);
    variable v_count  : std_logic_vector(32-1 downto 0);
    variable v_count2 : std_logic_vector(32-1 downto 0);
  begin
    connect    (jtag_ini, jtag_tgt);
    dmi_read   (jtag_ini, jtag_tgt, DM_DMSTATUS  , v_data);
    check_value(v_data, x"00400CA3", ERROR, "dmstatus : impebreak, allrunning, authenticated, hasresethaltreq, version 3", C_TB_SCOPE);
    dmi_read   (jtag_ini, jtag_tgt, DM_ABSTRACTCS, v_data);
    check_value(v_data, x"02000002", ERROR, "abstractcs : progbufsize 2, not busy, cmderr 0, datacount 2", C_TB_SCOPE);
    dmi_read   (jtag_ini, jtag_tgt, DM_HALTSUM0  , v_data);
    check_value(v_data, x"00000000", ERROR, "haltsum0 : running", C_TB_SCOPE);
    -- The firmware loop stores its counter
    read_mem   (mem, C_COUNTER, v_count , false);
    wait for 5 us;
    read_mem   (mem, C_COUNTER, v_count2, false);
    check_value(unsigned(v_count2) > unsigned(v_count), ERROR,
                "the firmware runs : counter 0x" & to_hstring(v_count) & " then 0x" & to_hstring(v_count2), C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_reset_pkg;
