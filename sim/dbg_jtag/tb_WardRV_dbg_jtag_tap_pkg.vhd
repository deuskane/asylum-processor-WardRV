-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_jtag_tap_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_tap_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario tap of tb_WardRV_dbg_jtag : Test-Logic-Reset (TRST*, 5 x TMS = 1), Capture-IR,
--              IDCODE, BYPASS and undecoded opcodes
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

package tb_WardRV_dbg_jtag_tap_pkg is

  -- Test-Logic-Reset (TRST*, 5 x TMS = 1), Capture-IR,
  -- IDCODE, BYPASS and undecoded opcodes
  procedure run(
    signal   jtag_if    : inout t_jtag_if
  );

end package tb_WardRV_dbg_jtag_tap_pkg;

package body tb_WardRV_dbg_jtag_tap_pkg is

  procedure run(
    signal   jtag_if    : inout t_jtag_if
  ) is
    constant C_PATTERN  : std_logic_vector(8-1 downto 0) := x"A7";
    variable v_data     : std_logic_vector(32-1 downto 0);
    variable v_bypass   : std_logic_vector(8-1 downto 0);
  begin
    log(ID_LOG_HDR, "IDCODE after TRST*", C_TB_SCOPE);
    jtag_reset   (msg        => "Test-Logic-Reset",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_scan_dr (data_value => x"00000000", data_out => v_data, msg => "IDCODE",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE selected after reset", C_TB_SCOPE);

    log(ID_LOG_HDR, "BYPASS : one bit delay, capture 0", C_TB_SCOPE);
    for i in 0 to 1 loop
      if i = 0
      then
        jtag_write_ir(ir_value   => JTAG_IR_BYPASS, msg => "BYPASS",
                      jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
      else
        jtag_write_ir(ir_value   => "00010", msg => "Not decoded",
                      jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
      end if;
      jtag_scan_dr (data_value => C_PATTERN, data_out => v_bypass, msg => "BYPASS",
                    jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
      check_value(v_bypass, C_PATTERN(6 downto 0) & '0', ERROR, "BYPASS " & integer'image(i), C_TB_SCOPE);
    end loop;

    log(ID_LOG_HDR, "Test-Logic-Reset after 5 TMS = 1 : IDCODE", C_TB_SCOPE);
    jtag_write_ir(ir_value   => JTAG_IR_BYPASS, msg => "BYPASS",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_reset   (msg        => "Test-Logic-Reset",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_scan_dr (data_value => x"00000000", data_out => v_data, msg => "IDCODE",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE after Test-Logic-Reset", C_TB_SCOPE);

    log(ID_LOG_HDR, "TRST* : IDCODE", C_TB_SCOPE);
    jtag_write_ir(ir_value   => JTAG_IR_BYPASS, msg => "BYPASS",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    vip_jtag_trst(jtag_if);
    jtag_idle    (num_cycles => 1, msg => "Run-Test/Idle",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_scan_dr (data_value => x"00000000", data_out => v_data, msg => "IDCODE",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE after TRST*", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_jtag_tap_pkg;
