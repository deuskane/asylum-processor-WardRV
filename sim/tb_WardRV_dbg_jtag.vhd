-------------------------------------------------------------------------------
-- Title      : WardRV Debug - JTAG unit testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: WardRV_dbg_jtag (TAP + DTM + DMI CDC) driven by the JTAG VIP,
--              with a fake Debug Module (128 registers, answer in the cycle
--              of the request). Scenarios in tb_WardRV_dbg_jtag_pkg (TEST).
--              CLK_PERIOD_NS : clk period, TCK period = 50 ns.
--              A monitor checks that TDO / tdo_en change on the falling edge
--              of TCK only.
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
use     work.tb_WardRV_dbg_jtag_pkg.all;

entity tb_WardRV_dbg_jtag is
  generic (
    TEST           : string  := "tap"
   ;CLK_PERIOD_NS  : natural := 10
   ;VERBOSE        : boolean := false
  );
end entity tb_WardRV_dbg_jtag;

architecture sim of tb_WardRV_dbg_jtag is

  constant C_SCOPE     : string := "tb_WardRV_dbg_jtag";

  signal   clk_i       : std_logic;
  signal   clk_ena     : boolean := true;
  signal   arst_b_i    : std_logic;
  signal   jtag_ini    : jtag_ini_t := (tck => '0', trst_n => '0', tms => '1', tdi => '0');
  signal   jtag_tgt    : jtag_tgt_t;
  signal   dmi_req     : dmi_req_t;
  signal   dmi_rdata   : std_logic_vector(32-1 downto 0);
  signal   dm_nb_req   : natural := 0;
  signal   dm_regs     : dbg_words_t(0 to 2**DMI_ABITS-1) := (others => (others => '0'));

begin  -- architecture sim

  clock_generator(clk_i, clk_ena, CLK_PERIOD_NS * 1 ns, "TB Clock");
  arst_b_i <= '0', '1' after 100 ns;

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  dut : entity asylum.WardRV_dbg_jtag
    generic map
    (IDCODE_VALUE    => C_IDCODE
    ,TRST_ENABLE     => true
    ,DTM_IDLE_CYCLES => 5
    )
    port map
    (jtag_ini_i      => jtag_ini
    ,jtag_tgt_o      => jtag_tgt
    ,clk_i           => clk_i
    ,arst_b_i        => arst_b_i
    ,dmi_req_o       => dmi_req
    ,dmi_rdata_i     => dmi_rdata
    );

  -----------------------------------------------------------------------------
  -- Fake Debug Module
  -----------------------------------------------------------------------------
  p_dm : process(clk_i)
  begin
    if rising_edge(clk_i) and dmi_req.valid = '1'
    then
      if dmi_req.we = '1'
      then
        dm_regs(to_integer(unsigned(dmi_req.addr))) <= dmi_req.wdata;
      end if;
      dm_nb_req <= dm_nb_req + 1;
    end if;
  end process;

  -- Read data in the cycle of the request
  dmi_rdata <= dm_regs(to_integer(unsigned(dmi_req.addr)));

  -----------------------------------------------------------------------------
  -- TDO and tdo_en change on the falling edge of TCK only
  -----------------------------------------------------------------------------
  p_tdo : process
  begin
    wait on jtag_tgt;
    if arst_b_i = '1' and jtag_ini.tck /= '0'
    then
      alert(ERROR, "TDO / tdo_en changed while TCK is high", C_SCOPE);
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Sequencer
  -----------------------------------------------------------------------------
  p_main : process
  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    if not VERBOSE
    then
      disable_log_msg(ID_BFM);
    end if;

    log(ID_LOG_HDR, "TEST " & TEST & ", clk period " & integer'image(CLK_PERIOD_NS) & " ns", C_SCOPE);
    wait until arst_b_i = '1';
    vip_jtag_init(jtag_ini);

    if    TEST = "tap"
    then
      test_tap  (jtag_ini, jtag_tgt);
    elsif TEST = "dtmcs"
    then
      test_dtmcs(jtag_ini, jtag_tgt);
    elsif TEST = "dmi"
    then
      test_dmi  (jtag_ini, jtag_tgt, dm_nb_req);
    elsif TEST = "busy"
    then
      test_busy (jtag_ini, jtag_tgt, dm_nb_req);
    else
      alert(TB_ERROR, "Unknown TEST " & TEST, C_SCOPE);
    end if;

    wait for 1 us;
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture sim;
