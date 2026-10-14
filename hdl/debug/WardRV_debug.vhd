-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_debug.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-- WardRV debug unit (RISC-V Debug Specification 1.0), convenience top :
--  * WardRV_dbg_jtag : JTAG TAP (IEEE 1149.1) + DTM + DMI CDC (TCK domain)
--  * WardRV_dbg_dm   : Debug Module (clk domain), debug memory for the core
--                      (execution-based), optional System Bus Access
-- The two blocks can also be instantiated separately (e.g. to replace the
-- JTAG DTM by another transport on the DMI records).
-- Core side : connect dbg_hart_req_o / dbg_sbi_ini_i / dbg_sbi_tgt_o to the
-- ports of WardRV_fsm (sbi_WardRV_fsm) synthesized with DEBUG_ENABLE=true.
-- arst_b_i : power-on reset of the debug unit (never ndmreset_o).
-- hart_rst_b_i : reset of the hart (havereset, unavail).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-10-09  2.0      mrosiere Rewrite : top of WardRV_dbg_jtag and
--                               WardRV_dbg_dm (Debug Spec 1.0)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_debug is
  generic (
    IDCODE_VALUE        : std_logic_vector(31 downto 0) := x"10000001";
    TRST_ENABLE         : boolean                       := true;
    DTM_IDLE_CYCLES     : natural                       := 5;
    PROGBUF_SIZE        : natural                       := 2;
    DATA_COUNT          : natural                       := 2;
    IMPEBREAK           : boolean                       := true;
    SBA_ENABLE          : boolean                       := false;
    SBA_ADDR_WIDTH      : positive                      := 8;
    RESETHALTREQ_ENABLE : boolean                       := false;
    HARTID              : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    -- JTAG (TCK domain)
    jtag_ini_i     : in  jtag_ini_t;
    jtag_tgt_o     : out jtag_tgt_t;

    -- clk domain
    clk_i          : in  std_logic;
    arst_b_i       : in  std_logic;

    -- Hart
    hart_rst_b_i   : in  std_logic;
    dbg_hart_req_o : out dbg_hart_req_t;
    dbg_sbi_ini_i  : in  dbg_sbi_ini_t;
    dbg_sbi_tgt_o  : out dbg_sbi_tgt_t;

    -- SoC
    ndmreset_o     : out std_logic;
    sba_sbi_ini_o  : out sbi_ini_t(addr (SBA_ADDR_WIDTH-1 downto 0),
                                   wdata(7 downto 0));
    sba_sbi_tgt_i  : in  sbi_tgt_t(rdata(7 downto 0)) := (ready => '0', rdata => (others => '0'), info => (name => (others => ' ')))
  );
end entity WardRV_debug;

architecture rtl of WardRV_debug is

  signal dmi_req : dmi_req_t;
  signal dmi_rsp : dmi_rsp_t;

begin

  ins_jtag : entity work.WardRV_dbg_jtag
    generic map (
      IDCODE_VALUE        => IDCODE_VALUE,
      TRST_ENABLE         => TRST_ENABLE,
      DTM_IDLE_CYCLES     => DTM_IDLE_CYCLES
    )
    port map (
      jtag_ini_i          => jtag_ini_i,
      jtag_tgt_o          => jtag_tgt_o,
      clk_i               => clk_i,
      arst_b_i            => arst_b_i,
      dmi_req_o           => dmi_req,
      dmi_rsp_i           => dmi_rsp
    );

  ins_dm : entity work.WardRV_dbg_dm
    generic map (
      PROGBUF_SIZE        => PROGBUF_SIZE,
      DATA_COUNT          => DATA_COUNT,
      IMPEBREAK           => IMPEBREAK,
      SBA_ENABLE          => SBA_ENABLE,
      SBA_ADDR_WIDTH      => SBA_ADDR_WIDTH,
      RESETHALTREQ_ENABLE => RESETHALTREQ_ENABLE,
      HARTID              => HARTID
    )
    port map (
      clk_i               => clk_i,
      arst_b_i            => arst_b_i,
      dmi_req_i           => dmi_req,
      dmi_rsp_o           => dmi_rsp,
      hart_rst_b_i        => hart_rst_b_i,
      dbg_hart_req_o      => dbg_hart_req_o,
      dbg_sbi_ini_i       => dbg_sbi_ini_i,
      dbg_sbi_tgt_o       => dbg_sbi_tgt_o,
      ndmreset_o          => ndmreset_o,
      sba_sbi_ini_o       => sba_sbi_ini_o,
      sba_sbi_tgt_i       => sba_sbi_tgt_i
    );

end architecture rtl;
