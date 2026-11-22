-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_jtag_reset_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_reset_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario reset of tb_WardRV_dbg_jtag : after TRST* : IDCODE selected, dtmcs fields
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_dbg_jtag_pkg.vhd)
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
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;

use     work.tb_WardRV_dbg_jtag_bfm_pkg.all;

package tb_WardRV_dbg_jtag_reset_pkg is

  -- after TRST* : IDCODE selected, dtmcs fields
  procedure run(
    signal   jtag_if    : inout t_jtag_if
  );

end package tb_WardRV_dbg_jtag_reset_pkg;

package body tb_WardRV_dbg_jtag_reset_pkg is

  procedure run(
    signal   jtag_if    : inout t_jtag_if
  ) is
    variable v_data     : std_logic_vector(32-1 downto 0);
  begin
    -- TRST* by vip_jtag_trst : IDCODE selected
    jtag_idle   (num_cycles => 1, msg => "Run-Test/Idle",
                 jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_scan_dr(data_value => x"00000000", data_out => v_data, msg => "IDCODE",
                 jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE after reset", C_TB_SCOPE);
    vip_jtag_dtmcs(jtag_if, x"00000000", v_data);
    check_value(v_data, x"00005071", ERROR, "dtmcs : idle 5, dmistat 0, abits 7, version 1", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_jtag_reset_pkg;
