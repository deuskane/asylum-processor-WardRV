-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_jtag.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- JTAG Debug Transport : TAP (IEEE 1149.1) + RISC-V DTM + DMI CDC.
-- JTAG pins (TCK domain) in, DMI request / response (clk domain) out.
-- arst_b_i is the power-on reset of the debug unit (never ndmreset) : it
-- resets the clk side of the CDC and, combined with TRST*, the TAP and the
-- DTM. The TCK side of the CDC is reset by arst_b_i only : a DMI transaction
-- in flight when TRST* is asserted always completes its handshake (the DTM
-- stays busy meanwhile and ignores the result).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere TCK side of the CDC not reset by TRST*
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_jtag is
  generic (
    IDCODE_VALUE    : std_logic_vector(31 downto 0) := x"10000001";
    TRST_ENABLE     : boolean                       := true;
    DTM_IDLE_CYCLES : natural                       := 5
  );
  port (
    -- JTAG (TCK domain)
    jtag_ini_i   : in  jtag_ini_t;
    jtag_tgt_o   : out jtag_tgt_t;

    -- DMI (clk domain)
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    dmi_req_o    : out dmi_req_t;
    dmi_rsp_i    : in  dmi_rsp_t
  );
end entity WardRV_dbg_jtag;

architecture rtl of WardRV_dbg_jtag is

  signal tck          : std_logic;
  signal trst_b       : std_logic;
  signal tlr          : std_logic;
  signal ir           : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  signal capture_dr   : std_logic;
  signal shift_dr     : std_logic;
  signal update_dr    : std_logic;
  signal tdi          : std_logic;
  signal user_tdo     : std_logic;

  signal tck_dmi_req  : dmi_req_t;
  signal tck_dmi_rsp  : dmi_rsp_t;
  signal tck_dmi_busy : std_logic;

begin

  ins_tap : entity work.WardRV_dbg_tap
    generic map (
      IDCODE_VALUE => IDCODE_VALUE,
      TRST_ENABLE  => TRST_ENABLE
    )
    port map (
      arst_b_i     => arst_b_i,
      jtag_ini_i   => jtag_ini_i,
      jtag_tgt_o   => jtag_tgt_o,
      tck_o        => tck,
      trst_b_o     => trst_b,
      tlr_o        => tlr,
      ir_o         => ir,
      capture_dr_o => capture_dr,
      shift_dr_o   => shift_dr,
      update_dr_o  => update_dr,
      tdi_o        => tdi,
      user_tdo_i   => user_tdo
    );

  ins_dtm : entity work.WardRV_dbg_dtm
    generic map (
      IDLE_CYCLES  => DTM_IDLE_CYCLES
    )
    port map (
      tck_i        => tck,
      trst_b_i     => trst_b,
      tlr_i        => tlr,
      ir_i         => ir,
      capture_dr_i => capture_dr,
      shift_dr_i   => shift_dr,
      update_dr_i  => update_dr,
      tdi_i        => tdi,
      tdo_o        => user_tdo,
      dmi_req_o    => tck_dmi_req,
      dmi_rsp_i    => tck_dmi_rsp,
      dmi_busy_i   => tck_dmi_busy
    );

  -- TCK side of the CDC : power-on reset only (not TRST*), so that the
  -- handshake of a transaction in flight is never broken
  ins_dmi_cdc : entity work.WardRV_dbg_dmi_cdc
    port map (
      tck_i        => tck,
      trst_b_i     => arst_b_i,
      tck_req_i    => tck_dmi_req,
      tck_rsp_o    => tck_dmi_rsp,
      tck_busy_o   => tck_dmi_busy,
      clk_i        => clk_i,
      arst_b_i     => arst_b_i,
      dmi_req_o    => dmi_req_o,
      dmi_rsp_i    => dmi_rsp_i
    );

end architecture rtl;
