-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_vips.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: VIPs for WardRV simulation
--  * Reset pulse, reset pulse before a scenario (TESTS_LIST)
--  * JTAG : TMS / TDI change while TCK is low, TDO is sampled just before the
--    rising edge of TCK, vectors are shifted LSB first. Every scan starts and
--    ends in Run-Test/Idle.
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
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

package WardRV_vips is

  constant C_TCK_PERIOD  : time    := 50 ns;
  constant C_DMI_IDLE    : natural := 5;                          -- Run-Test/Idle cycles after a dmi scan

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
  -- TRST* pulse, TMS = 1 : Test-Logic-Reset
  procedure vip_jtag_init(
    signal   jtag_ini : out jtag_ini_t
  );

  -- One TCK cycle
  procedure vip_jtag_clock(
    signal   jtag_ini : out jtag_ini_t;
    constant tms      : in  std_logic;
    constant tdi      : in  std_logic := '0'
  );

  -- Cycles in Run-Test/Idle
  procedure vip_jtag_idle(
    signal   jtag_ini : out jtag_ini_t;
    constant cycles   : in  natural
  );

  -- 5 TCK with TMS = 1 (Test-Logic-Reset), then Run-Test/Idle
  procedure vip_jtag_reset(
    signal   jtag_ini : out jtag_ini_t
  );

  -- IR scan (ir = true) or DR scan : returns the captured value
  procedure vip_jtag_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir       : in  boolean;
    constant data_out : in  std_logic_vector;
    variable data_in  : out std_logic_vector
  );

  procedure vip_jtag_write_ir(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir_val   : in  std_logic_vector(JTAG_IR_WIDTH-1 downto 0)
  );

  -- ===========================================================================
  -- RISC-V DTM VIP
  -- ===========================================================================
  procedure vip_jtag_dtmcs(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0)
  );

  -- One dmi scan (IR = DMI), then idle cycles in Run-Test/Idle
  procedure vip_jtag_dmi_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
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
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant data     : in  std_logic_vector(32-1 downto 0)
  );

  procedure vip_jtag_dmi_read(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
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
  procedure vip_jtag_init(
    signal   jtag_ini : out jtag_ini_t
  ) is
  begin
    jtag_ini <= (tck => '0', trst_n => '0', tms => '1', tdi => '0');
    wait for C_TCK_PERIOD;
    jtag_ini.trst_n <= '1';
    wait for C_TCK_PERIOD;
  end procedure;

  procedure vip_jtag_clock(
    signal   jtag_ini : out jtag_ini_t;
    constant tms      : in  std_logic;
    constant tdi      : in  std_logic := '0'
  ) is
  begin
    jtag_ini.tck <= '0';
    jtag_ini.tms <= tms;
    jtag_ini.tdi <= tdi;
    wait for C_TCK_PERIOD / 2;
    jtag_ini.tck <= '1';
    wait for C_TCK_PERIOD / 2;
  end procedure;

  procedure vip_jtag_idle(
    signal   jtag_ini : out jtag_ini_t;
    constant cycles   : in  natural
  ) is
  begin
    for i in 1 to cycles loop
      vip_jtag_clock(jtag_ini, '0');
    end loop;
  end procedure;

  procedure vip_jtag_reset(
    signal   jtag_ini : out jtag_ini_t
  ) is
  begin
    for i in 1 to 5 loop
      vip_jtag_clock(jtag_ini, '1');
    end loop;
    vip_jtag_clock(jtag_ini, '0');
  end procedure;

  procedure vip_jtag_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir       : in  boolean;
    constant data_out : in  std_logic_vector;
    variable data_in  : out std_logic_vector
  ) is
    alias    v_out    : std_logic_vector(data_out'length-1 downto 0) is data_out;
    variable v_in     : std_logic_vector(data_out'length-1 downto 0);
  begin
    -- Run-Test/Idle -> Select-DR -> (Select-IR) -> Capture -> Shift
    vip_jtag_clock(jtag_ini, '1');
    if ir
    then
      vip_jtag_clock(jtag_ini, '1');
    end if;
    vip_jtag_clock(jtag_ini, '0');
    vip_jtag_clock(jtag_ini, '0');
    -- Shift, the last bit with TMS = 1 (Exit1)
    for i in 0 to v_out'length-1 loop
      jtag_ini.tck <= '0';
      jtag_ini.tms <= '1' when i = v_out'length-1 else '0';
      jtag_ini.tdi <= v_out(i);
      wait for C_TCK_PERIOD / 2;
      v_in(i)      := jtag_tgt.tdo;
      jtag_ini.tck <= '1';
      wait for C_TCK_PERIOD / 2;
    end loop;
    data_in := v_in;
    -- Exit1 -> Update -> Run-Test/Idle
    vip_jtag_clock(jtag_ini, '1');
    vip_jtag_clock(jtag_ini, '0');
  end procedure;

  procedure vip_jtag_write_ir(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir_val   : in  std_logic_vector(JTAG_IR_WIDTH-1 downto 0)
  ) is
    variable v_capture : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  begin
    vip_jtag_scan(jtag_ini, jtag_tgt, true, ir_val, v_capture);
    check_value(v_capture, "00001", ERROR, "Capture-IR", C_VIP_SCOPE);
  end procedure;

  -- ===========================================================================
  -- RISC-V DTM VIP
  -- ===========================================================================
  procedure vip_jtag_dtmcs(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0)
  ) is
  begin
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DTMCS);
    vip_jtag_scan    (jtag_ini, jtag_tgt, false, wdata, rdata);
  end procedure;

  procedure vip_jtag_dmi_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant wdata    : in  std_logic_vector(32-1 downto 0);
    variable rdata    : out std_logic_vector(32-1 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := C_DMI_IDLE
  ) is
    variable v_dmi    : std_logic_vector(DMI_ABITS+32+2-1 downto 0);
  begin
    vip_jtag_scan(jtag_ini, jtag_tgt, false, addr & wdata & op, v_dmi);
    vip_jtag_idle(jtag_ini, idle);
    rdata  := v_dmi(33 downto 2);
    status := v_dmi( 1 downto 0);
  end procedure;

  -- Request, then nop scans until the result is not busy
  procedure vip_jtag_dmi_access(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
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
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI);
    for retry in 1 to 100 loop
      vip_jtag_dmi_scan(jtag_ini, jtag_tgt, v_op, addr, wdata, rdata, v_status, v_idle);
      if v_status = DMI_OP_BUSY
      then
        -- The scan was ignored : clear the status, wait longer, scan again
        log(ID_BFM, "DMI busy : dmireset, " & integer'image(v_idle+1) & " idle cycles", C_VIP_SCOPE);
        vip_jtag_dtmcs   (jtag_ini, jtag_tgt, x"00010000", v_dtmcs);
        vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI);
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
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    constant data     : in  std_logic_vector(32-1 downto 0)
  ) is
    variable v_rdata  : std_logic_vector(32-1 downto 0);
  begin
    log(ID_BFM, "DMI write 0x" & to_hstring(addr) & " = 0x" & to_hstring(data), C_VIP_SCOPE);
    vip_jtag_dmi_access(jtag_ini, jtag_tgt, DMI_OP_WRITE, addr, data, v_rdata);
  end procedure;

  procedure vip_jtag_dmi_read(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(DMI_ABITS-1 downto 0);
    variable data     : out std_logic_vector(32-1 downto 0)
  ) is
    variable v_rdata  : std_logic_vector(32-1 downto 0);
  begin
    vip_jtag_dmi_access(jtag_ini, jtag_tgt, DMI_OP_READ, addr, x"00000000", v_rdata);
    log(ID_BFM, "DMI read  0x" & to_hstring(addr) & " = 0x" & to_hstring(v_rdata), C_VIP_SCOPE);
    data := v_rdata;
  end procedure;

end package body WardRV_vips;
