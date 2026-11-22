-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_vips.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: VIPs for WardRV simulation
--  * Reset pulse, reset pulse before a scenario (TESTS_LIST)
--  * JTAG : the JTAG BFM jtag_bfm_pkg of asylum-verification-uvvm, with the
--    configuration of the WardRV testbenches (C_JTAG_BFM_CONFIG : TCK 50 ns,
--    5-bit IR). Here only what the BFM does not do : a TRST* pulse alone
--    (jtag_reset always adds the TMS sequence) and a monitor of the full
--    Capture-IR value (jtag_write_ir checks its two LSB "01" only)
--  * RISC-V DTM : dtmcs and dmi scans, DMI read / write with the busy retry
--    of a debugger (dmireset, then more Run-Test/Idle cycles)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-11-17  1.1      mrosiere JTAG and DMI procedures of the debug unit
-- 2026-11-20  1.2      mrosiere vip_reset_scenario (reset pulse of TESTS_LIST)
-- 2026-11-22  1.3      mrosiere JTAG on the JTAG BFM of asylum-verification-uvvm
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum_uvvm;
use     asylum_uvvm.jtag_bfm_pkg.all;

library asylum;
use     asylum.WardRV_debug_pkg.all;

package WardRV_vips is

  constant C_DMI_IDLE    : natural := 5;                          -- Run-Test/Idle cycles after a dmi scan

  -- JTAG BFM : TCK 50 ns, IR of the debug unit, TRST* by vip_jtag_trst
  constant C_JTAG_BFM_CONFIG : t_jtag_bfm_config := (
    tck_period       => 50 ns,
    ir_width         => JTAG_IR_WIDTH,
    idle_cycles      => 0,
    trst_enable      => false,
    match_strictness => MATCH_EXACT,
    id_for_bfm       => ID_BFM
    );

  -- dmi.op
  constant DMI_OP_NOP    : std_logic_vector(1 downto 0) := "00";
  constant DMI_OP_READ   : std_logic_vector(1 downto 0) := "01";
  constant DMI_OP_WRITE  : std_logic_vector(1 downto 0) := "10";
  constant DMI_OP_BUSY   : std_logic_vector(1 downto 0) := "11";

  -- ===========================================================================
  -- Reset VIP
  -- ===========================================================================
  procedure vip_reset_pulse(
    signal   reset_n  : out std_logic;
    constant width    : in  time;
    constant msg      : in  string := ""
  );

  -- Reset pulse before a scenario (TESTS_LIST) : asserted during width,
  -- released on the falling (inactive) edge of the clock
  procedure vip_reset_scenario(
    signal   clk      : in  std_logic;
    signal   reset_n  : out std_logic;
    constant width    : in  time
  );

  -- ===========================================================================
  -- JTAG VIP
  -- ===========================================================================
  -- TRST* pulse alone, TMS = 1 : Test-Logic-Reset
  procedure vip_jtag_trst(
    signal   jtag_if  : inout t_jtag_if
  );

  -- Concurrent procedure : follows the TAP controller on TMS and checks the
  -- bits shifted out by each IR scan against the Capture-IR value "00001"
  procedure vip_jtag_ir_capture_monitor(
    signal   jtag_if  : in    t_jtag_if
  );

  -- ===========================================================================
  -- RISC-V DTM VIP
  -- ===========================================================================
  procedure vip_jtag_dtmcs(
    signal   jtag_if  : inout t_jtag_if;
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0)
  );

  -- One dmi scan (IR = DMI), then idle cycles in Run-Test/Idle
  procedure vip_jtag_dmi_scan(
    signal   jtag_if  : inout t_jtag_if;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := C_DMI_IDLE
  );

  -- DMI read / write as a debugger : selects IR DMI, request then nop scan
  -- for the result, dmireset and retry with more idle cycles on busy
  procedure vip_jtag_dmi_write(
    signal   jtag_if  : inout t_jtag_if;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant data     : in  std_logic_vector(32-1 downto 0)
  );

  procedure vip_jtag_dmi_read(
    signal   jtag_if  : inout t_jtag_if;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    variable data     : out std_logic_vector(32-1 downto 0)
  );

end package WardRV_vips;

package body WardRV_vips is

  constant C_VIP_SCOPE   : string := "WardRV_vips";

  -- ===========================================================================
  -- Reset VIP
  -- ===========================================================================
  procedure vip_reset_pulse(
    signal   reset_n  : out std_logic;
    constant width    : in  time;
    constant msg      : in  string := ""
  ) is
  begin
    log(ID_BFM, "Asserting Reset " & msg);
    reset_n <= '0';
    wait for width;
    reset_n <= '1';
    log(ID_BFM, "Deasserting Reset " & msg);
  end procedure;

  procedure vip_reset_scenario(
    signal   clk      : in  std_logic;
    signal   reset_n  : out std_logic;
    constant width    : in  time
  ) is
  begin
    reset_n <= '0';
    wait for width;
    wait until falling_edge(clk);
    reset_n <= '1';
  end procedure;

  -- ===========================================================================
  -- JTAG VIP
  -- ===========================================================================
  procedure vip_jtag_trst(
    signal   jtag_if  : inout t_jtag_if
  ) is
  begin
    jtag_if.tck    <= '0';
    jtag_if.tms    <= '1';
    jtag_if.tdi    <= '0';
    jtag_if.trst_b <= '0';
    wait for C_JTAG_BFM_CONFIG.tck_period;
    jtag_if.trst_b <= '1';
    wait for C_JTAG_BFM_CONFIG.tck_period;
  end procedure;

  procedure vip_jtag_ir_capture_monitor(
    signal   jtag_if  : in    t_jtag_if
  ) is
    type     tap_state_t is (TEST_LOGIC_RESET, RUN_TEST_IDLE,
                             SELECT_DR_SCAN, CAPTURE_DR, SHIFT_DR, EXIT1_DR, PAUSE_DR, EXIT2_DR, UPDATE_DR,
                             SELECT_IR_SCAN, CAPTURE_IR, SHIFT_IR, EXIT1_IR, PAUSE_IR, EXIT2_IR, UPDATE_IR);
    variable v_state   : tap_state_t := TEST_LOGIC_RESET;
    variable v_tms     : std_logic;
    variable v_capture : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  begin
    loop
      wait until rising_edge(jtag_if.tck) or jtag_if.trst_b = '0';
      if jtag_if.trst_b = '0'
      then
        v_state := TEST_LOGIC_RESET;
      else
        -- Shift-IR : TDO is the next bit of the captured value, LSB first
        if v_state = SHIFT_IR
        then
          v_capture := jtag_if.tdo & v_capture(JTAG_IR_WIDTH-1 downto 1);
        end if;

        -- TAP controller (IEEE 1149.1 figure 6-1)
        v_tms := jtag_if.tms;
        case v_state is
          when TEST_LOGIC_RESET => v_state := TEST_LOGIC_RESET when v_tms = '1' else RUN_TEST_IDLE;
          when RUN_TEST_IDLE    => v_state := SELECT_DR_SCAN   when v_tms = '1' else RUN_TEST_IDLE;
          when SELECT_DR_SCAN   => v_state := SELECT_IR_SCAN   when v_tms = '1' else CAPTURE_DR;
          when CAPTURE_DR       => v_state := EXIT1_DR         when v_tms = '1' else SHIFT_DR;
          when SHIFT_DR         => v_state := EXIT1_DR         when v_tms = '1' else SHIFT_DR;
          when EXIT1_DR         => v_state := UPDATE_DR        when v_tms = '1' else PAUSE_DR;
          when PAUSE_DR         => v_state := EXIT2_DR         when v_tms = '1' else PAUSE_DR;
          when EXIT2_DR         => v_state := UPDATE_DR        when v_tms = '1' else SHIFT_DR;
          when UPDATE_DR        => v_state := SELECT_DR_SCAN   when v_tms = '1' else RUN_TEST_IDLE;
          when SELECT_IR_SCAN   => v_state := TEST_LOGIC_RESET when v_tms = '1' else CAPTURE_IR;
          when CAPTURE_IR       => v_state := EXIT1_IR         when v_tms = '1' else SHIFT_IR;
          when SHIFT_IR         => v_state := EXIT1_IR         when v_tms = '1' else SHIFT_IR;
          when EXIT1_IR         => v_state := UPDATE_IR        when v_tms = '1' else PAUSE_IR;
          when PAUSE_IR         => v_state := EXIT2_IR         when v_tms = '1' else PAUSE_IR;
          when EXIT2_IR         => v_state := UPDATE_IR        when v_tms = '1' else SHIFT_IR;
          when UPDATE_IR        => v_state := SELECT_DR_SCAN   when v_tms = '1' else RUN_TEST_IDLE;
        end case;

        -- End of the IR scan
        if v_state = EXIT1_IR
        then
          check_value(v_capture, "00001", ERROR, "Capture-IR", C_VIP_SCOPE);
        end if;
      end if;
    end loop;
  end procedure;

  -- ===========================================================================
  -- RISC-V DTM VIP
  -- ===========================================================================
  procedure vip_jtag_dtmcs(
    signal   jtag_if  : inout t_jtag_if;
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0)
  ) is
  begin
    jtag_write_ir(ir_value => JTAG_IR_DTMCS, msg => "dtmcs",
                  jtag_if  => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_scan_dr (data_value => wdata, data_out => rdata, msg => "dtmcs",
                  jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
  end procedure;

  procedure vip_jtag_dmi_scan(
    signal   jtag_if  : inout t_jtag_if;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := C_DMI_IDLE
  ) is
    variable v_dmi    : std_logic_vector(DMI_ABITS+32+2-1 downto 0);
  begin
    jtag_scan_dr(data_value => addr & wdata & op, data_out => v_dmi, msg => "dmi",
                 jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    jtag_idle   (num_cycles => idle, msg => "after the dmi scan",
                 jtag_if    => jtag_if, config => C_JTAG_BFM_CONFIG);
    rdata  := v_dmi(33 downto 2);
    status := v_dmi( 1 downto 0);
  end procedure;

  -- Request, then nop scans until the result is not busy
  procedure vip_jtag_dmi_access(
    signal   jtag_if  : inout t_jtag_if;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0)
  ) is
    variable v_idle   : natural := C_DMI_IDLE;
    variable v_op     : std_logic_vector(1 downto 0) := op;
    variable v_status : std_logic_vector(1 downto 0);
    variable v_dtmcs  : std_logic_vector(32-1 downto 0);
  begin
    jtag_write_ir(ir_value => JTAG_IR_DMI, msg => "dmi",
                  jtag_if  => jtag_if, config => C_JTAG_BFM_CONFIG);
    for retry in 1 to 100 loop
      vip_jtag_dmi_scan(jtag_if, v_op, addr, wdata, rdata, v_status, v_idle);
      if v_status = DMI_OP_BUSY
      then
        -- The scan was ignored : clear the status, wait longer, scan again
        log(ID_BFM, "DMI busy : dmireset, " & integer'image(v_idle+1) & " idle cycles", C_VIP_SCOPE);
        vip_jtag_dtmcs(jtag_if, x"00010000", v_dtmcs);
        jtag_write_ir (ir_value => JTAG_IR_DMI, msg => "dmi",
                       jtag_if  => jtag_if, config => C_JTAG_BFM_CONFIG);
        v_idle := v_idle + 1;
      elsif v_op = DMI_OP_NOP
      then
        return;
      else
        -- Request accepted, the next scan returns its result
        v_op   := DMI_OP_NOP;
      end if;
    end loop;
    alert(ERROR, "DMI access : always busy", C_VIP_SCOPE);
  end procedure;

  procedure vip_jtag_dmi_write(
    signal   jtag_if  : inout t_jtag_if;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant data     : in  std_logic_vector(32-1 downto 0)
  ) is
    variable v_rdata  : std_logic_vector(32-1 downto 0);
  begin
    log(ID_BFM, "DMI write 0x" & to_hstring(addr) & " = 0x" & to_hstring(data), C_VIP_SCOPE);
    vip_jtag_dmi_access(jtag_if, DMI_OP_WRITE, addr, data, v_rdata);
  end procedure;

  procedure vip_jtag_dmi_read(
    signal   jtag_if  : inout t_jtag_if;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    variable data     : out std_logic_vector(32-1 downto 0)
  ) is
    variable v_rdata  : std_logic_vector(32-1 downto 0);
  begin
    vip_jtag_dmi_access(jtag_if, DMI_OP_READ, addr, x"00000000", v_rdata);
    log(ID_BFM, "DMI read  0x" & to_hstring(addr) & " = 0x" & to_hstring(v_rdata), C_VIP_SCOPE);
    data := v_rdata;
  end procedure;

end package body WardRV_vips;
