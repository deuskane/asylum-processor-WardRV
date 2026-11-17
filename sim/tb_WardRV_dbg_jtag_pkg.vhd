-------------------------------------------------------------------------------
-- Title      : WardRV Debug - JTAG unit test scenarios
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_WardRV_dbg_jtag (one per run, TEST generic) :
--  tap   : Test-Logic-Reset (TRST*, 5 x TMS = 1), Capture-IR, IDCODE,
--          BYPASS and undecoded opcodes
--  dtmcs : dtmcs fields
--  dmi   : DMI writes and reads of the fake Debug Module, one DMI request
--          per access
--  busy  : scans too close to each other : sticky busy status, request
--          ignored, dmireset and dmihardreset
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;

package tb_WardRV_dbg_jtag_pkg is

  constant C_IDCODE : std_logic_vector(32-1 downto 0) := x"1DEB0001";

  procedure test_tap(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t
  );

  procedure test_dtmcs(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t
  );

  procedure test_dmi(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t;
    signal   dm_nb_req  : in  natural                             -- Requests seen by the fake DM
  );

  procedure test_busy(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t;
    signal   dm_nb_req  : in  natural
  );

end package tb_WardRV_dbg_jtag_pkg;

package body tb_WardRV_dbg_jtag_pkg is

  constant C_TB_SCOPE : string := "tb_WardRV_dbg_jtag";

  -----------------------------------------------------------------------------
  -- tap
  -----------------------------------------------------------------------------
  procedure test_tap(
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

  -----------------------------------------------------------------------------
  -- dtmcs
  -----------------------------------------------------------------------------
  procedure test_dtmcs(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t
  ) is
    variable v_dtmcs    : std_logic_vector(32-1 downto 0);
  begin
    vip_jtag_reset(jtag_ini);
    vip_jtag_dtmcs(jtag_ini, jtag_tgt, x"00000000", v_dtmcs);
    check_value(v_dtmcs, x"00005071", ERROR, "dtmcs : idle 5, dmistat 0, abits 7, version 1", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- dmi
  -----------------------------------------------------------------------------
  procedure test_dmi(
    signal   jtag_ini   : out jtag_ini_t;
    signal   jtag_tgt   : in  jtag_tgt_t;
    signal   dm_nb_req  : in  natural
  ) is
    variable v_data     : std_logic_vector(32-1 downto 0);
    variable v_addr     : std_logic_vector(DMI_ABITS-1 downto 0);
  begin
    vip_jtag_reset(jtag_ini);
    for i in 0 to 7 loop
      v_addr := std_logic_vector(to_unsigned(4*i+3, DMI_ABITS));
      vip_jtag_dmi_write(jtag_ini, jtag_tgt, v_addr, std_logic_vector(to_unsigned(i, 4)) & x"BADC0D" & std_logic_vector(to_unsigned(i, 4)));
    end loop;
    for i in 7 downto 0 loop
      v_addr := std_logic_vector(to_unsigned(4*i+3, DMI_ABITS));
      vip_jtag_dmi_read(jtag_ini, jtag_tgt, v_addr, v_data);
      check_value(v_data, std_logic_vector(to_unsigned(i, 4)) & x"BADC0D" & std_logic_vector(to_unsigned(i, 4)), ERROR, "DMI read 0x" & to_hstring(v_addr), C_TB_SCOPE);
    end loop;
    check_value(dm_nb_req, 16, ERROR, "One DMI request per access", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- busy
  -----------------------------------------------------------------------------
  procedure test_busy(
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

end package body tb_WardRV_dbg_jtag_pkg;
