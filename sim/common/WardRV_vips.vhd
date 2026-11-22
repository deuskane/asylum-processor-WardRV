-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_vips.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: VIPs for WardRV simulation
--  * Reset pulse, reset pulse before a scenario (TESTS_LIST)
--  * JTAG : configuration of the JTAG BFM jtag_bfm_pkg of
--    asylum-verification-uvvm for the WardRV testbenches (TCK 50 ns, 5-bit
--    IR) : C_JTAG_BFM_CONFIG (jtag_reset by TMS only) and
--    C_JTAG_BFM_CONFIG_TRST (jtag_trst). The JTAG protocol (TRST* pulse,
--    scans, Capture-IR monitor) is in the BFM
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
-- 2026-11-22  1.4      mrosiere vip_jtag_trst and vip_jtag_ir_capture_monitor
--                               removed : jtag_trst and jtag_ir_capture_monitor
--                               of the BFM
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

  -- JTAG BFM : TCK 50 ns, IR of the debug unit, jtag_reset by TMS only
  constant C_JTAG_BFM_CONFIG : t_jtag_bfm_config := (
    tck_period       => 50 ns,
    ir_width         => JTAG_IR_WIDTH,
    idle_cycles      => 0,
    trst_enable      => false,
    match_strictness => MATCH_EXACT,
    id_for_bfm       => ID_BFM
    );

  -- The same with TRST* : configuration of jtag_trst
  constant C_JTAG_BFM_CONFIG_TRST : t_jtag_bfm_config := (
    tck_period       => C_JTAG_BFM_CONFIG.tck_period,
    ir_width         => C_JTAG_BFM_CONFIG.ir_width,
    idle_cycles      => C_JTAG_BFM_CONFIG.idle_cycles,
    trst_enable      => true,
    match_strictness => C_JTAG_BFM_CONFIG.match_strictness,
    id_for_bfm       => C_JTAG_BFM_CONFIG.id_for_bfm
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
