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

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;

use     work.tb_WardRV_dbg_jtag_bfm_pkg.all;

package tb_WardRV_dbg_jtag_tap_pkg is

  -- Test-Logic-Reset (TRST*, 5 x TMS = 1), Capture-IR,
  -- IDCODE, BYPASS and undecoded opcodes
  procedure run(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t
  );

end package tb_WardRV_dbg_jtag_tap_pkg;

package body tb_WardRV_dbg_jtag_tap_pkg is

  procedure run(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t
  ) is
    constant C_PATTERN  : std_logic_vector(8-1 downto 0) := x"A7";
    variable v_data     : std_logic_vector(32-1 downto 0);
    variable v_bypass   : std_logic_vector(8-1 downto 0);
  begin
    log(ID_LOG_HDR, "IDCODE after TRST*", C_TB_SCOPE);
    vip_jtag_reset(jtag_ini);
    vip_jtag_scan (jtag_ini, jtag_tgt, false, x"00000000", v_data);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE selected after reset", C_TB_SCOPE);

    log(ID_LOG_HDR, "BYPASS : one bit delay, capture 0", C_TB_SCOPE);
    for i in 0 to 1 loop
      if i = 0
      then
        vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_BYPASS);
      else
        vip_jtag_write_ir(jtag_ini, jtag_tgt, "00010");           -- Not decoded
      end if;
      vip_jtag_scan(jtag_ini, jtag_tgt, false, C_PATTERN, v_bypass);
      check_value(v_bypass, C_PATTERN(6 downto 0) & '0', ERROR, "BYPASS " & integer'image(i), C_TB_SCOPE);
    end loop;

    log(ID_LOG_HDR, "Test-Logic-Reset after 5 TMS = 1 : IDCODE", C_TB_SCOPE);
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_BYPASS);
    vip_jtag_reset   (jtag_ini);
    vip_jtag_scan    (jtag_ini, jtag_tgt, false, x"00000000", v_data);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE after Test-Logic-Reset", C_TB_SCOPE);

    log(ID_LOG_HDR, "TRST* : IDCODE", C_TB_SCOPE);
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_BYPASS);
    vip_jtag_init    (jtag_ini);
    vip_jtag_clock   (jtag_ini, '0');                             -- Run-Test/Idle
    vip_jtag_scan    (jtag_ini, jtag_tgt, false, x"00000000", v_data);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE after TRST*", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_jtag_tap_pkg;
