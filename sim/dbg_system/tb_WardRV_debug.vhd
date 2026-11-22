-------------------------------------------------------------------------------
-- Title      : WardRV Debug - System testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: WardRV_fsm (DEBUG_ENABLE, 2 triggers, RESETHALTREQ_ENABLE,
--              EBREAK_EXCEPTION generic) + WardRV_debug (JTAG, Debug Module
--              with a program buffer of 2 words, 2 data registers), driven
--              by the JTAG BFM like a debugger. Scenarios chosen at run
--              time with TESTS_LIST (names in tb_WardRV_debug_tests_pkg,
--              one package per scenario), each one after a reset pulse.
--              The core runs FIRMWARE_FILE (esw/debug/debug_fw.hex) from a
--              memory answering in one cycle, loaded again by each reset
--              pulse ; a store at C_MEIP_ADDR sets meip_i to wdata(0).
--              ndmreset resets the core, not the debug unit.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-- 2026-11-20  1.1      mrosiere TESTS_LIST, one package per scenario, reset
--                               pulse before each scenario
-- 2026-11-22  1.2      mrosiere JTAG BFM of asylum-verification-uvvm
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
use     work.tb_WardRV_pkg.all;
use     work.tb_WardRV_debug_bfm_pkg.all;
use     work.tb_WardRV_debug_tests_pkg.all;

entity tb_WardRV_debug is
  generic (
    TESTS_LIST           : string  := ""                        -- Scenarios, "" : build only
   ;EBREAK_EXCEPTION     : boolean := false
   ;TRIGGER_RANGE_ENABLE : boolean := false
   ;FIRMWARE_FILE        : string  := "debug/debug_fw.hex"
   ;VERBOSE              : boolean := false
  );
end entity tb_WardRV_debug;

architecture sim of tb_WardRV_debug is

  constant C_SCOPE     : string := "tb_WardRV_debug";

  signal   clk_i       : std_logic;
  signal   clk_ena     : boolean := true;
  signal   arst_b_i    : std_logic := '0';                        -- Power-on reset
  signal   hart_rst_b  : std_logic;                               -- Power-on reset and ndmreset
  signal   ndmreset    : std_logic;
  signal   meip        : std_logic := '0';

  signal   mem         : ram_t(0 to C_MEM_SIZE-1);
  signal   imem_ini    : imem_ini_t;
  signal   imem_tgt    : imem_tgt_t;
  signal   dmem_ini    : dmem_ini_t;
  signal   dmem_tgt    : dmem_tgt_t;

  signal   jtag_if     : t_jtag_if := init_jtag_if_signals;
  signal   jtag_tgt    : jtag_tgt_t;
  signal   dbg_hart_req: dbg_hart_req_t;
  signal   dbg_mem_ini : dmem_ini_t;
  signal   dbg_mem_tgt : dmem_tgt_t;

begin  -- architecture sim

  clock_generator(clk_i, clk_ena, C_CLK_PERIOD, "TB Clock");
  hart_rst_b <= arst_b_i and not ndmreset;

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  ins_core : entity asylum.WardRV_fsm
    generic map
    (HARTID               => C_HARTID
    ,RESET_ADDR           => C_FIRMWARE_ADDR
    ,VERBOSE              => VERBOSE
    ,DEBUG_ENABLE         => true
    ,NB_TRIGGERS          => C_NB_TRIGGERS
    ,EBREAK_EXCEPTION     => EBREAK_EXCEPTION
    ,RESETHALTREQ_ENABLE  => true
    ,TRIGGER_RANGE_ENABLE => TRIGGER_RANGE_ENABLE
    )
    port map
    (clk_i               => clk_i
    ,arst_b_i            => hart_rst_b
    ,imem_ini_o          => imem_ini
    ,imem_tgt_i          => imem_tgt
    ,dmem_ini_o          => dmem_ini
    ,dmem_tgt_i          => dmem_tgt
    ,meip_i              => meip
    ,dbg_hart_req_i      => dbg_hart_req
    ,dbg_mem_ini_o       => dbg_mem_ini
    ,dbg_mem_tgt_i       => dbg_mem_tgt
    );

  ins_debug : entity asylum.WardRV_debug
    generic map
    (IDCODE_VALUE        => C_IDCODE
    ,PROGBUF_SIZE        => 2
    ,DATA_COUNT          => 2
    ,SBA_ENABLE          => false
    ,RESETHALTREQ_ENABLE => true
    )
    port map
    (jtag_ini_i.tck      => jtag_if.tck
    ,jtag_ini_i.trst_n   => jtag_if.trst_b
    ,jtag_ini_i.tms      => jtag_if.tms
    ,jtag_ini_i.tdi      => jtag_if.tdi
    ,jtag_tgt_o          => jtag_tgt
    ,clk_i               => clk_i
    ,arst_b_i            => arst_b_i
    ,hart_rst_b_i        => hart_rst_b
    ,dbg_hart_req_o      => dbg_hart_req
    ,dbg_mem_ini_i       => dbg_mem_ini
    ,dbg_mem_tgt_o       => dbg_mem_tgt
    ,ndmreset_o          => ndmreset
    ,sba_dmem_ini_o      => open
    );

  -- TDO tri-stated by tdo_en (pad)
  jtag_if.tdo <= jtag_tgt.tdo when jtag_tgt.tdo_en = '1' else
                 'Z';

  -- Capture-IR value of each IR scan
  vip_jtag_ir_capture_monitor(jtag_if);

  -----------------------------------------------------------------------------
  -- Memory : answer in one cycle, meip_i written at C_MEIP_ADDR. Each reset
  -- pulse of the sequencer loads the firmware again and clears meip_i : a
  -- scenario does not see the memory writes of the previous one
  -----------------------------------------------------------------------------
  p_mem : process
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    loop
      if arst_b_i = '0'
      then
        init_ram(FIRMWARE_FILE, mem);
        meip <= '0';
        wait until arst_b_i = '1';
      end if;
      wait until rising_edge(clk_i);
      imem_tgt <= (ready => '0', inst  => (others => '0'));
      dmem_tgt <= (ready => '0', rdata => (others => '0'), err => '0');
      if imem_ini.valid = '1'
      then
        check_value(imem_ini.addr(32-1 downto DBG_MEM_AWIDTH) /= DBG_MEM_BASE(32-1 downto DBG_MEM_AWIDTH), ERROR,
                    "Fetch from the debug memory on the system bus", C_SCOPE, ID_NEVER);
        read_mem(mem, imem_ini.addr, v_rdata, VERBOSE);
        imem_tgt <= (ready => '1', inst => v_rdata);
      end if;
      if dmem_ini.valid = '1'
      then
        check_value(dmem_ini.addr(32-1 downto DBG_MEM_AWIDTH) /= DBG_MEM_BASE(32-1 downto DBG_MEM_AWIDTH), ERROR,
                    "Access to the debug memory on the system bus", C_SCOPE, ID_NEVER);
        if dmem_ini.we = '1' and dmem_ini.addr = C_MEIP_ADDR
        then
          meip <= dmem_ini.wdata(0);
        elsif dmem_ini.we = '1'
        then
          write_mem(mem, dmem_ini.addr, dmem_ini.wdata, dmem_ini.be, VERBOSE);
        else
          read_mem(mem, dmem_ini.addr, v_rdata, VERBOSE);
          dmem_tgt.rdata <= v_rdata;
        end if;
        dmem_tgt.ready <= '1';
      end if;
    end loop;
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

    log(ID_LOG_HDR, "EBREAK_EXCEPTION " & boolean'image(EBREAK_EXCEPTION) & ", TRIGGER_RANGE_ENABLE " & boolean'image(TRIGGER_RANGE_ENABLE) & ", TESTS_LIST : " & TESTS_LIST, C_SCOPE);

    -- Every name is checked before the first scenario
    for i in 1 to list_length(TESTS_LIST) loop
      if not list_contains(C_TESTS, list_item(TESTS_LIST, i))
      then
        alert(TB_ERROR, "Unknown test """ & list_item(TESTS_LIST, i) & """ (tests : " & C_TESTS & ")", C_SCOPE);
      elsif not test_applies(list_item(TESTS_LIST, i), EBREAK_EXCEPTION, TRIGGER_RANGE_ENABLE)
      then
        alert(TB_ERROR, "Test " & list_item(TESTS_LIST, i) & " : exc_* needs EBREAK_EXCEPTION, trigger_ge / _lt / _range_* TRIGGER_RANGE_ENABLE, the others the default generics", C_SCOPE);
      end if;
    end loop;

    for i in 1 to list_length(TESTS_LIST) loop
      -- Reset pulse (firmware loaded again), then TRST* : each scenario starts from reset
      vip_reset_scenario(clk_i, arst_b_i, 100 ns);
      vip_jtag_trst(jtag_if);

      log(ID_LOG_HDR, "TEST " & integer'image(i) & "/" & integer'image(list_length(TESTS_LIST)) & " : " & list_item(TESTS_LIST, i), C_SCOPE);
      run_test(list_item(TESTS_LIST, i), jtag_if, mem);
      wait for 1 us;
    end loop;

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture sim;
