-------------------------------------------------------------------------------
-- Title      : WardRV Debug - JTAG Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Unit testbench of WardRV_dbg_jtag (TAP + DTM + DMI CDC) with
--              the JTAG VIP and a fake Debug Module (register file with a
--              programmable latency). The same test sequence runs on two
--              instances : clk 10x faster than TCK and clk 10x slower.
--              Checked :
--              - Test-Logic-Reset through TRST* and after 5 TMS=1
--              - Capture-IR = 00001, IDCODE, BYPASS (1 bit delay, capture 0),
--                undecoded opcodes -> BYPASS
--              - TDO / tdo_en change on the falling edge of TCK only, tdo_en
--                only in Shift-IR / Shift-DR, IR updated on the falling edge
--              - dtmcs fields, DMI read / write, busy sticky status and
--                request ignored while busy, dmireset, failed status,
--                dmihardreset (dmi address / data back to 0)
--              - TRST* during a DMI transaction : the next DMI access gets
--                its own result (not the one of the abandoned transaction)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere Review : dmihardreset, TRST* during a DMI
--                               transaction (slow address of the fake DM)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

use work.WardRV_vips.all;

entity tb_WardRV_dbg_jtag is
  generic (
    IDCODE_VALUE : std_logic_vector(31 downto 0) := x"1BADC0DF"
  );
end entity tb_WardRV_dbg_jtag;

architecture tb of tb_WardRV_dbg_jtag is

  constant C_NB_DUT       : natural := 2;
  constant C_TCK_PERIOD   : time    := 100 ns;
  type time_array_t is array (0 to C_NB_DUT-1) of time;
  constant C_CLK_PERIOD   : time_array_t := (0 => 10 ns,    -- clk 10x faster than TCK
                                             1 => 1000 ns); -- clk 10x slower than TCK
  constant C_DM_LATENCY   : natural := 3;                  -- Fake DM response latency (clk cycles)
  constant C_ADDR_FAILED  : std_logic_vector(6 downto 0) := "1111111"; -- Fake DM : failed response
  constant C_ADDR_SLOW    : std_logic_vector(6 downto 0) := "1111110"; -- Fake DM : long latency
  constant C_DM_LATENCY_SLOW : natural := 30;              -- clk cycles

  type jtag_ini_array_t is array (0 to C_NB_DUT-1) of jtag_ini_t;
  type jtag_tgt_array_t is array (0 to C_NB_DUT-1) of jtag_tgt_t;
  type dmi_req_array_t  is array (0 to C_NB_DUT-1) of dmi_req_t;
  type dmi_rsp_array_t  is array (0 to C_NB_DUT-1) of dmi_rsp_t;
  type nat_array_t      is array (0 to C_NB_DUT-1) of natural;

  signal clk        : std_logic_vector(C_NB_DUT-1 downto 0) := (others => '0');
  signal arst_b     : std_logic := '0';
  signal jtag_ini   : jtag_ini_array_t;
  signal jtag_tgt   : jtag_tgt_array_t;
  signal dmi_req    : dmi_req_array_t;
  signal dmi_rsp    : dmi_rsp_array_t;
  signal dm_nb_req  : nat_array_t := (others => 0);  -- Number of requests seen by the fake DM
  signal tap_ir         : std_logic_vector(4 downto 0);  -- IR of the standalone TAP
  signal tap_ir_updates : natural := 0;

begin

  arst_b <= '0', '1' after 50 ns;

  gen_dut : for d in 0 to C_NB_DUT-1 generate
    signal clk_en : boolean := true;
  begin
    clock_generator(clk(d), clk_en, C_CLK_PERIOD(d), "clk " & integer'image(d));

    dut : entity asylum.WardRV_dbg_jtag
      generic map (
        IDCODE_VALUE    => IDCODE_VALUE,
        TRST_ENABLE     => true,
        DTM_IDLE_CYCLES => 5
      )
      port map (
        jtag_ini_i => jtag_ini(d),
        jtag_tgt_o => jtag_tgt(d),
        clk_i      => clk(d),
        arst_b_i   => arst_b,
        dmi_req_o  => dmi_req(d),
        dmi_rsp_i  => dmi_rsp(d)
      );

    ------------------------------------------------------------------
    -- Fake Debug Module : 128 x 32 bits, ready immediately,
    -- response after C_DM_LATENCY cycles (C_ADDR_SLOW : + C_DM_LATENCY_SLOW).
    -- The request (op, address, data) is latched when it is accepted.
    ------------------------------------------------------------------
    p_dm : process
      type regs_t is array (0 to 127) of std_logic_vector(31 downto 0);
      variable v_regs  : regs_t := (others => (others => '0'));
      variable v_addr  : natural;
      variable v_op    : std_logic_vector(1 downto 0);
      variable v_wdata : std_logic_vector(31 downto 0);
    begin
      dmi_rsp(d) <= DMI_RSP_NULL;
      loop
        wait until rising_edge(clk(d));
        dmi_rsp(d) <= DMI_RSP_NULL;
        if dmi_req(d).valid = '1' then
          dmi_rsp(d).ready <= '1';
          dm_nb_req(d)     <= dm_nb_req(d) + 1;
          v_addr           := to_integer(unsigned(dmi_req(d).addr));
          v_op             := dmi_req(d).op;
          v_wdata          := dmi_req(d).data;
          wait until rising_edge(clk(d));
          dmi_rsp(d).ready <= '0';
          for i in 2 to C_DM_LATENCY loop
            wait until rising_edge(clk(d));
          end loop;
          if v_addr = to_integer(unsigned(C_ADDR_SLOW)) then
            for i in 1 to C_DM_LATENCY_SLOW loop
              wait until rising_edge(clk(d));
            end loop;
          end if;
          dmi_rsp(d).valid <= '1';
          dmi_rsp(d).op    <= DMI_RSP_SUCCESS;
          dmi_rsp(d).data  <= v_regs(v_addr);
          if v_op = DMI_OP_WRITE then
            v_regs(v_addr) := v_wdata;
          end if;
          if v_addr = to_integer(unsigned(C_ADDR_FAILED)) then
            dmi_rsp(d).op  <= DMI_RSP_FAILED;
          end if;
        end if;
      end loop;
    end process;

    ------------------------------------------------------------------
    -- TCK edge checkers
    ------------------------------------------------------------------
    p_tdo_edge : process
    begin
      wait on jtag_tgt(d).tdo, jtag_tgt(d).tdo_en;
      if arst_b = '1' and jtag_ini(d).trst_n = '1' then
        check_value(jtag_ini(d).tck, '0', ERROR, "TDO / tdo_en must change on the falling edge of TCK", C_TB_SCOPE_DEFAULT, ID_NEVER);
      end if;
    end process;

  end generate gen_dut;

  ------------------------------------------------------------------
  -- IR update on the falling edge of TCK (external names)
  ------------------------------------------------------------------
  ------------------------------------------------------------------
  -- Standalone TAP on the JTAG bus of dut 0 : its IR must be updated on
  -- the falling edge of TCK and must follow the IR of dut 0
  ------------------------------------------------------------------
  tap : entity asylum.WardRV_dbg_tap
    generic map (
      IDCODE_VALUE => IDCODE_VALUE,
      TRST_ENABLE  => true
    )
    port map (
      arst_b_i     => arst_b,
      jtag_ini_i   => jtag_ini(0),
      jtag_tgt_o   => open,
      tck_o        => open,
      trst_b_o     => open,
      tlr_o        => open,
      ir_o         => tap_ir,
      capture_dr_o => open,
      shift_dr_o   => open,
      update_dr_o  => open,
      tdi_o        => open,
      user_tdo_i   => '0'
    );

  p_ir_edge : process
  begin
    wait on tap_ir;
    if arst_b = '1' and jtag_ini(0).trst_n = '1' then
      check_value(jtag_ini(0).tck, '0', ERROR, "IR must be updated on the falling edge of TCK", C_TB_SCOPE_DEFAULT, ID_NEVER);
    end if;
    tap_ir_updates <= tap_ir_updates + 1;
  end process;

  ------------------------------------------------------------------
  -- Test sequencer
  ------------------------------------------------------------------
  p_main : process

    procedure run_test(
      constant d          : in    natural;
      signal   jtag_ini_s : out   jtag_ini_t;
      signal   jtag_tgt_s : in    jtag_tgt_t
    ) is
      constant C_P      : time := C_TCK_PERIOD;
      variable v_ir     : std_logic_vector(4 downto 0);
      variable v_dr32   : std_logic_vector(31 downto 0);
      variable v_dr9    : std_logic_vector(8 downto 0);
      variable v_data   : std_logic_vector(31 downto 0);
      variable v_status : std_logic_vector(1 downto 0);
      variable v_nb_req : natural;
      constant C_MSG    : string := "[dut " & integer'image(d) & "] ";
    begin
      log(ID_LOG_HDR, C_MSG & "clk period " & time'image(C_CLK_PERIOD(d)) & ", TCK period " & time'image(C_P));

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "TRST* : IR = IDCODE, IDCODE read without IR scan");
      vip_jtag_init(jtag_ini_s, C_P);
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- TLR -> RTI
      vip_jtag_dr_scan(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32, IDCODE_VALUE, ERROR, C_MSG & "IDCODE after TRST*");
      check_value(jtag_tgt_s.tdo_en, '0', ERROR, C_MSG & "tdo_en low in Run-Test/Idle");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "Capture-IR = 00001, BYPASS");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_BYPASS, v_ir, C_MSG, C_P);
      check_value(v_ir, std_logic_vector'("00001"), ERROR, C_MSG & "Capture-IR");
      vip_jtag_dr_scan(jtag_ini_s, jtag_tgt_s, "101100111", v_dr9, C_MSG, C_P);
      -- shifted LSB first : the output is the input delayed by 1 bit, first bit = 0
      check_value(v_dr9, std_logic_vector'("011001110"), ERROR, C_MSG & "BYPASS");

      log(ID_SEQUENCER, C_MSG & "Undecoded opcode -> BYPASS");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, "00101", v_ir, C_MSG, C_P);
      vip_jtag_dr_scan(jtag_ini_s, jtag_tgt_s, "111111111", v_dr9, C_MSG, C_P);
      check_value(v_dr9, std_logic_vector'("111111110"), ERROR, C_MSG & "Undecoded opcode is BYPASS");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, "00000", v_ir, C_MSG, C_P);
      vip_jtag_dr_scan(jtag_ini_s, jtag_tgt_s, "000000011", v_dr9, C_MSG, C_P);
      check_value(v_dr9, std_logic_vector'("000000110"), ERROR, C_MSG & "Opcode 00000 is BYPASS");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "Test-Logic-Reset after 5 TMS = 1 (from Shift-DR)");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_BYPASS, C_MSG, C_P);
      vip_jtag_clock(jtag_ini_s, '1', '0', C_P);                       -- Select-DR
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- Capture-DR
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- Shift-DR
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- Shift-DR
      vip_jtag_reset(jtag_ini_s, C_MSG, C_P);                          -- 5 x TMS=1 then RTI
      vip_jtag_dr_scan(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32, IDCODE_VALUE, ERROR, C_MSG & "IDCODE after Test-Logic-Reset");
      vip_jtag_read_idcode(jtag_ini_s, jtag_tgt_s, IDCODE_VALUE, C_MSG, C_P);

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "dtmcs");
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32(3 downto 0)  , std_logic_vector'("0001")   , ERROR, C_MSG & "dtmcs.version");
      check_value(v_dr32(9 downto 4)  , std_logic_vector'("000111") , ERROR, C_MSG & "dtmcs.abits");
      check_value(v_dr32(11 downto 10), std_logic_vector'("00")     , ERROR, C_MSG & "dtmcs.dmistat");
      check_value(v_dr32(14 downto 12), std_logic_vector'("101")    , ERROR, C_MSG & "dtmcs.idle");
      check_value(v_dr32(31 downto 15), std_logic_vector'("00000000000000000"), ERROR, C_MSG & "dtmcs upper bits");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "DMI write / read");
      vip_jtag_dmi_write(jtag_ini_s, jtag_tgt_s, "0000100", x"12345678", C_MSG, C_P);
      vip_jtag_dmi_write(jtag_ini_s, jtag_tgt_s, "0100000", x"CAFEDECA", C_MSG, C_P);
      vip_jtag_dmi_read (jtag_ini_s, jtag_tgt_s, "0000100", v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"12345678"), ERROR, C_MSG & "DMI read 0x04");
      vip_jtag_dmi_read (jtag_ini_s, jtag_tgt_s, "0100000", v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"CAFEDECA"), ERROR, C_MSG & "DMI read 0x20");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "DMI busy : scan without idle, request ignored, dmireset");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_DMI, C_MSG, C_P);
      v_nb_req := dm_nb_req(d);
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_WRITE, "0000101", x"AAAA5555", v_data, v_status, 0, C_MSG, C_P);
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_WRITE, "0000101", x"DEADBEEF", v_data, v_status, 0, C_MSG, C_P);
      check_value(v_status, DMI_RSP_BUSY, ERROR, C_MSG & "busy status when scanning during a transaction");
      vip_jtag_idle(jtag_ini_s, 100, C_P);
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_NOP, "0000000", x"00000000", v_data, v_status, 5, C_MSG, C_P);
      check_value(v_status, DMI_RSP_BUSY, ERROR, C_MSG & "busy status is sticky");
      check_value(dm_nb_req(d), v_nb_req + 1, ERROR, C_MSG & "request ignored while busy");
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32(11 downto 10), DMI_RSP_BUSY, ERROR, C_MSG & "dtmcs.dmistat busy");
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00010000", v_dr32, C_MSG & "dmireset", C_P);
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32(11 downto 10), DMI_RSP_SUCCESS, ERROR, C_MSG & "dtmcs.dmistat cleared by dmireset");
      vip_jtag_dmi_read (jtag_ini_s, jtag_tgt_s, "0000101", v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"AAAA5555"), ERROR, C_MSG & "only the first write was done");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "DMI failed response");
      vip_jtag_dmi_access(jtag_ini_s, jtag_tgt_s, DMI_OP_READ, C_ADDR_FAILED, x"00000000", v_data, v_status, 5, C_MSG, C_P);
      check_value(v_status, DMI_RSP_FAILED, ERROR, C_MSG & "failed status");
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_READ, "0000100", x"00000000", v_data, v_status, 50, C_MSG, C_P);
      check_value(v_status, DMI_RSP_FAILED, ERROR, C_MSG & "failed status is sticky");
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00010000", v_dr32, C_MSG & "dmireset", C_P);
      vip_jtag_dmi_read (jtag_ini_s, jtag_tgt_s, "0000100", v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"12345678"), ERROR, C_MSG & "DMI read after dmireset");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "dmihardreset during a transaction");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_DMI, C_MSG, C_P);
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_READ, "0100000", x"00000000", v_data, v_status, 0, C_MSG, C_P);
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00020000", v_dr32, C_MSG & "dmihardreset", C_P);
      vip_jtag_idle(jtag_ini_s, 100, C_P);
      vip_jtag_dtmcs(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32(11 downto 10), DMI_RSP_SUCCESS, ERROR, C_MSG & "dtmcs.dmistat after dmihardreset");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_DMI, C_MSG, C_P);
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_NOP, "0000000", x"00000000", v_data, v_status, 5, C_MSG, C_P);
      check_value(v_status, DMI_RSP_SUCCESS, ERROR, C_MSG & "status after dmihardreset");
      -- dmihardreset : dmi data back to its reset value (slow clk : the read
      -- was still in progress and its result is ignored ; fast clk : the read
      -- was already done before dmihardreset)
      check_value(v_data, std_logic_vector'(x"00000000"), ERROR, C_MSG & "dmi data reset by dmihardreset, abandoned result ignored");
      vip_jtag_dmi_read (jtag_ini_s, jtag_tgt_s, "0100000", v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"CAFEDECA"), ERROR, C_MSG & "DMI read after dmihardreset");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "TRST* during a DMI scan");
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_DMI, C_MSG, C_P);
      vip_jtag_clock(jtag_ini_s, '1', '0', C_P);                       -- Select-DR
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- Capture-DR
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- Shift-DR
      vip_jtag_init(jtag_ini_s, C_P);
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- TLR -> RTI
      vip_jtag_dr_scan(jtag_ini_s, jtag_tgt_s, x"00000000", v_dr32, C_MSG, C_P);
      check_value(v_dr32, IDCODE_VALUE, ERROR, C_MSG & "IDCODE after TRST* during a scan");

      ----------------------------------------------------------------
      log(ID_SEQUENCER, C_MSG & "TRST* during a DMI transaction");
      vip_jtag_dmi_write(jtag_ini_s, jtag_tgt_s, C_ADDR_SLOW, x"5107A44E", C_MSG, C_P);
      vip_jtag_write_ir(jtag_ini_s, jtag_tgt_s, JTAG_IR_DMI, C_MSG, C_P);
      vip_jtag_dmi_scan(jtag_ini_s, jtag_tgt_s, DMI_OP_READ, C_ADDR_SLOW, x"00000000", v_data, v_status, 0, C_MSG, C_P);
      vip_jtag_idle(jtag_ini_s, 30, C_P);                              -- the clk side takes the request
      vip_jtag_init(jtag_ini_s, C_P);                                  -- TRST*
      vip_jtag_clock(jtag_ini_s, '0', '0', C_P);                       -- TLR -> RTI
      vip_jtag_dmi_read(jtag_ini_s, jtag_tgt_s, "0000100", v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"12345678"), ERROR, C_MSG & "DMI read after TRST* during a transaction : its own result");
      vip_jtag_dmi_read(jtag_ini_s, jtag_tgt_s, C_ADDR_SLOW, v_data, C_MSG, C_P);
      check_value(v_data, std_logic_vector'(x"5107A44E"), ERROR, C_MSG & "DMI read of the slow address");
    end procedure;

  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    disable_log_msg(ID_POS_ACK);

    for d in 0 to C_NB_DUT-1 loop
      jtag_ini(d) <= (tck => '0', trst_n => '0', tms => '1', tdi => '0');
    end loop;

    wait until arst_b = '1';
    wait for 100 ns;

    run_test(0, jtag_ini(0), jtag_tgt(0));
    check_value(tap_ir_updates > 10, ERROR, "IR updates seen on the standalone TAP");
    run_test(1, jtag_ini(1), jtag_tgt(1));

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture tb;
