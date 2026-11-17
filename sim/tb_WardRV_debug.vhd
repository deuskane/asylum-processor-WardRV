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
--              by the JTAG VIP like a debugger. Scenarios in
--              tb_WardRV_debug_pkg (TEST). The core runs FIRMWARE_FILE
--              (esw/debug/debug_fw.hex) from a memory answering in one
--              cycle ; a store at C_MEIP_ADDR sets meip_i to wdata(0).
--              ndmreset resets the core, not the debug unit.
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
use     work.tb_WardRV_pkg.all;
use     work.tb_WardRV_debug_pkg.all;

entity tb_WardRV_debug is
  generic (
    TEST             : string  := "halt_resume"
   ;EBREAK_EXCEPTION : boolean := false
   ;FIRMWARE_FILE    : string  := "debug/debug_fw.hex"
   ;VERBOSE          : boolean := false
  );
end entity tb_WardRV_debug;

architecture sim of tb_WardRV_debug is

  constant C_SCOPE     : string := "tb_WardRV_debug";

  signal   clk_i       : std_logic;
  signal   clk_ena     : boolean := true;
  signal   arst_b_i    : std_logic;                               -- Power-on reset
  signal   hart_rst_b  : std_logic;                               -- Power-on reset and ndmreset
  signal   ndmreset    : std_logic;
  signal   meip        : std_logic := '0';

  signal   mem         : ram_t(0 to C_MEM_SIZE-1);
  signal   imem_ini    : imem_ini_t;
  signal   imem_tgt    : imem_tgt_t;
  signal   dmem_ini    : dmem_ini_t;
  signal   dmem_tgt    : dmem_tgt_t;

  signal   jtag_ini    : jtag_ini_t := (tck => '0', trst_n => '0', tms => '1', tdi => '0');
  signal   jtag_tgt    : jtag_tgt_t;
  signal   dbg_hart_req: dbg_hart_req_t;
  signal   dbg_mem_ini : dmem_ini_t;
  signal   dbg_mem_tgt : dmem_tgt_t;

begin  -- architecture sim

  clock_generator(clk_i, clk_ena, C_CLK_PERIOD, "TB Clock");
  arst_b_i   <= '0', '1' after 100 ns;
  hart_rst_b <= arst_b_i and not ndmreset;

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  ins_core : entity asylum.WardRV_fsm
    generic map
    (HARTID              => C_HARTID
    ,RESET_ADDR          => C_FIRMWARE_ADDR
    ,VERBOSE             => VERBOSE
    ,DEBUG_ENABLE        => true
    ,NB_TRIGGERS         => C_NB_TRIGGERS
    ,EBREAK_EXCEPTION    => EBREAK_EXCEPTION
    ,RESETHALTREQ_ENABLE => true
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
    ,IMPEBREAK           => true
    ,SBA_ENABLE          => false
    ,RESETHALTREQ_ENABLE => true
    )
    port map
    (jtag_ini_i          => jtag_ini
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

  -----------------------------------------------------------------------------
  -- Memory : answer in one cycle, meip_i written at C_MEIP_ADDR
  -----------------------------------------------------------------------------
  p_mem : process
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    init_ram(FIRMWARE_FILE, mem);
    loop
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
    disable_log_msg(ID_POS_ACK);
    if not VERBOSE
    then
      disable_log_msg(ID_BFM);
    end if;

    log(ID_LOG_HDR, "TEST " & TEST & ", EBREAK_EXCEPTION " & boolean'image(EBREAK_EXCEPTION), C_SCOPE);
    if (TEST(1 to minimum(4, TEST'length)) = "exc_") /= EBREAK_EXCEPTION
    then
      alert(TB_ERROR, "TEST exc_* needs EBREAK_EXCEPTION = true, the others false", C_SCOPE);
    end if;
    wait until arst_b_i = '1';
    vip_jtag_init(jtag_ini);

    if    TEST = "halt_resume"
    then
      test_halt_resume    (jtag_ini, jtag_tgt);
    elsif TEST = "registers"
    then
      test_registers      (jtag_ini, jtag_tgt);
    elsif TEST = "cmderr"
    then
      test_cmderr         (jtag_ini, jtag_tgt);
    elsif TEST = "progbuf"
    then
      test_progbuf        (jtag_ini, jtag_tgt);
    elsif TEST = "step"
    then
      test_step           (jtag_ini, jtag_tgt);
    elsif TEST = "ebreak"
    then
      test_ebreak         (jtag_ini, jtag_tgt);
    elsif TEST = "trigger_execute"
    then
      test_trigger_execute(jtag_ini, jtag_tgt);
    elsif TEST = "trigger_store"
    then
      test_trigger_store  (jtag_ini, jtag_tgt);
    elsif TEST = "trigger_load"
    then
      test_trigger_load   (jtag_ini, jtag_tgt);
    elsif TEST = "resethaltreq"
    then
      test_resethaltreq   (jtag_ini, jtag_tgt);
    elsif TEST = "interrupt"
    then
      test_interrupt      (jtag_ini, jtag_tgt);
    elsif TEST = "exc_ebreak"
    then
      test_exc_ebreak     (jtag_ini, jtag_tgt);
    elsif TEST = "exc_trigger"
    then
      test_exc_trigger    (jtag_ini, jtag_tgt);
    else
      alert(TB_ERROR, "Unknown TEST " & TEST, C_SCOPE);
    end if;

    wait for 1 us;
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture sim;
