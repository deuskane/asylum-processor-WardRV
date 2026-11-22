-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_jtag_busy_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_busy_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario busy of tb_WardRV_dbg_jtag : scans too close to each other : sticky busy
--              status, request ignored, dmireset and dmihardreset
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

package tb_WardRV_dbg_jtag_busy_pkg is

  -- scans too close to each other : sticky busy
  -- status, request ignored, dmireset and dmihardreset
  procedure run(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t;
    signal   dm_nb_req  : in  natural
  );

end package tb_WardRV_dbg_jtag_busy_pkg;

package body tb_WardRV_dbg_jtag_busy_pkg is

  procedure run(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t;
    signal   dm_nb_req  : in  natural
  ) is
    constant C_ADDR     : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"05";
    variable v_data     : std_logic_vector(32-1 downto 0);
    variable v_status   : std_logic_vector(1 downto 0);
    variable v_dtmcs    : std_logic_vector(32-1 downto 0);
  begin
    vip_jtag_reset   (jtag_ini);
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI);

    for i in 0 to 1 loop
      log(ID_LOG_HDR, "Second request without idle cycles : busy, ignored", C_TB_SCOPE);
      vip_jtag_dmi_scan(jtag_ini, jtag_tgt, DMI_OP_WRITE, C_ADDR, x"11111111", v_data, v_status, 0);
      check_value(v_status, "00", ERROR, "First request accepted", C_TB_SCOPE);
      vip_jtag_dmi_scan(jtag_ini, jtag_tgt, DMI_OP_WRITE, C_ADDR, x"22222222", v_data, v_status, 20);
      check_value(v_status, DMI_OP_BUSY, ERROR, "Second request : busy", C_TB_SCOPE);
      vip_jtag_dmi_scan(jtag_ini, jtag_tgt, DMI_OP_READ , C_ADDR, x"00000000", v_data, v_status, 20);
      check_value(v_status, DMI_OP_BUSY, ERROR, "Busy is sticky", C_TB_SCOPE);
      vip_jtag_dtmcs(jtag_ini, jtag_tgt, x"00000000", v_dtmcs);
      check_value(v_dtmcs(11 downto 10), "11", ERROR, "dtmcs.dmistat = 3", C_TB_SCOPE);

      if i = 0
      then
        log(ID_LOG_HDR, "dmireset", C_TB_SCOPE);
        vip_jtag_dtmcs(jtag_ini, jtag_tgt, x"00010000", v_dtmcs);
      else
        log(ID_LOG_HDR, "dmihardreset", C_TB_SCOPE);
        vip_jtag_dtmcs(jtag_ini, jtag_tgt, x"00020000", v_dtmcs);
      end if;
      vip_jtag_dtmcs(jtag_ini, jtag_tgt, x"00000000", v_dtmcs);
      check_value(v_dtmcs(11 downto 10), "00", ERROR, "dtmcs.dmistat cleared", C_TB_SCOPE);

      vip_jtag_dmi_read(jtag_ini, jtag_tgt, C_ADDR, v_data);
      check_value(v_data, x"11111111", ERROR, "Only the first request was executed", C_TB_SCOPE);
      vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI);
    end loop;
    check_value(dm_nb_req, 4, ERROR, "Requests executed : 2 writes, 2 reads", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_jtag_busy_pkg;
