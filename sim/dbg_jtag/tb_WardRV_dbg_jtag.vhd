-------------------------------------------------------------------------------
-- Title      : WardRV Debug - JTAG unit testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: WardRV_dbg_jtag (TAP + DTM + DMI CDC) driven by the JTAG BFM,
--              with a fake Debug Module (128 registers, answer in the cycle
--              of the request). Scenarios chosen at run time with TESTS_LIST
--              (names in tb_WardRV_dbg_jtag_tests_pkg, one package per
--              scenario), each one after a reset pulse and TRST*.
--              CLK_PERIOD_NS : clk period, TCK period = 50 ns.
--              JTAG BFM : jtag_bfm_pkg of asylum-verification-uvvm, TDO
--              tri-stated by tdo_en as at the pad. Monitors : TDO / tdo_en
--              change on the falling edge of TCK only, Capture-IR value
--              (jtag_ir_capture_monitor of the BFM).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-- 2026-11-20  1.1      mrosiere TESTS_LIST, one package per scenario, reset
--                               pulse before each scenario
-- 2026-11-22  1.2      mrosiere JTAG BFM of asylum-verification-uvvm
-- 2026-11-22  1.3      mrosiere jtag_trst and jtag_ir_capture_monitor of the
--                               BFM
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum_uvvm;
use     asylum_uvvm.jtag_bfm_pkg.all;

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;
use     asylum.string_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_dbg_jtag_bfm_pkg.all;
use     work.tb_WardRV_dbg_jtag_tests_pkg.all;

entity tb_WardRV_dbg_jtag is
  generic (
    TESTS_LIST     : string  := ""                              -- Scenarios, "" : build only
   ;CLK_PERIOD_NS  : natural := 10
   ;VERBOSE        : boolean := false
  );
end entity tb_WardRV_dbg_jtag;

architecture sim of tb_WardRV_dbg_jtag is

  constant C_SCOPE     : string := "tb_WardRV_dbg_jtag";

  signal   clk_i       : std_logic;
  signal   clk_ena     : boolean := true;
  signal   arst_b_i    : std_logic := '0';
  signal   jtag_if     : t_jtag_if := init_jtag_if_signals;
  signal   jtag_tgt    : jtag_tgt_t;
  signal   dmi_req     : dmi_req_t;
  signal   dmi_rdata   : std_logic_vector(32-1 downto 0);
  signal   dm_nb_req   : natural := 0;
  signal   dm_regs     : dbg_words_t(0 to 2**DMI_ABITS-1) := (others => (others => '0'));

begin  -- architecture sim

  clock_generator(clk_i, clk_ena, CLK_PERIOD_NS * 1 ns, "TB Clock");

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  dut : entity asylum.WardRV_dbg_jtag
    generic map
    (IDCODE_VALUE      => C_IDCODE
    ,TRST_ENABLE       => true
    ,DTM_IDLE_CYCLES   => 5
    )
    port map
    (jtag_ini_i.tck    => jtag_if.tck
    ,jtag_ini_i.trst_n => jtag_if.trst_b
    ,jtag_ini_i.tms    => jtag_if.tms
    ,jtag_ini_i.tdi    => jtag_if.tdi
    ,jtag_tgt_o        => jtag_tgt
    ,clk_i             => clk_i
    ,arst_b_i          => arst_b_i
    ,dmi_req_o         => dmi_req
    ,dmi_rdata_i       => dmi_rdata
    );

  -- TDO tri-stated by tdo_en (pad)
  jtag_if.tdo <= jtag_tgt.tdo when jtag_tgt.tdo_en = '1' else
                 'Z';

  -----------------------------------------------------------------------------
  -- Fake Debug Module (registers and request counter cleared by the reset)
  -----------------------------------------------------------------------------
  p_dm : process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0'
    then
      dm_regs   <= (others => (others => '0'));
      dm_nb_req <= 0;
    elsif rising_edge(clk_i) and dmi_req.valid = '1'
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
    if arst_b_i = '1' and jtag_if.tck /= '0'
    then
      alert(ERROR, "TDO / tdo_en changed while TCK is high", C_SCOPE);
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Capture-IR value of each IR scan
  -----------------------------------------------------------------------------
  jtag_ir_capture_monitor(jtag_if  => jtag_if,
                          expected => "00001",
                          scope    => C_SCOPE,
                          config   => C_JTAG_BFM_CONFIG);

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

    log(ID_LOG_HDR, "clk period " & integer'image(CLK_PERIOD_NS) & " ns, TESTS_LIST : " & TESTS_LIST, C_SCOPE);

    -- Every name is checked before the first scenario
    for i in 1 to list_length(TESTS_LIST) loop
      if not list_contains(C_TESTS, list_item(TESTS_LIST, i))
      then
        alert(TB_ERROR, "Unknown test """ & list_item(TESTS_LIST, i) & """ (tests : " & C_TESTS & ")", C_SCOPE);
      end if;
    end loop;

    for i in 1 to list_length(TESTS_LIST) loop
      -- Reset pulse, then TRST* (Test-Logic-Reset) : each scenario starts from reset
      vip_reset_scenario(clk_i, arst_b_i, 100 ns);
      jtag_trst(msg => "TRST*", jtag_if => jtag_if, config => C_JTAG_BFM_CONFIG_TRST);

      log(ID_LOG_HDR, "TEST " & integer'image(i) & "/" & integer'image(list_length(TESTS_LIST)) & " : " & list_item(TESTS_LIST, i), C_SCOPE);
      run_test(list_item(TESTS_LIST, i), jtag_if, dm_nb_req);
      wait for 1 us;
    end loop;

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture sim;
