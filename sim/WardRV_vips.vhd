-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_vips.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: VIPs for WardRV simulation
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere JTAG : TCK period parameter, LSB first shift
--                               whatever the vector direction, DTMCS / DMI
--                               scans with status, busy retry, dmireset
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use asylum.WardRV_pkg.all;
use asylum.WardRV_debug_pkg.all;

package WardRV_vips is

  constant C_JTAG_PERIOD : time := 100 ns;

  -- ===========================================================================
  -- Reset VIP
  -- ===========================================================================
  procedure vip_reset_pulse(
    signal   reset_n : out std_logic;
    constant width   : in  time;
    constant msg     : in  string := ""
  );

  -- ===========================================================================
  -- JTAG VIP
  -- TMS / TDI change while TCK is low, TDO is sampled just before the rising
  -- edge of TCK. Vectors are shifted LSB first whatever their direction.
  -- All the scans start and end in Run-Test/Idle.
  -- ===========================================================================
  procedure vip_jtag_init(
    signal   jtag_ini : out jtag_ini_t;
    constant period   : in  time := C_JTAG_PERIOD
  );

  procedure vip_jtag_clock(
    signal   jtag_ini : out jtag_ini_t;
    constant tms      : in  std_logic;
    constant tdi      : in  std_logic;
    constant period   : in  time := C_JTAG_PERIOD
  );

  procedure vip_jtag_idle(
    signal   jtag_ini : out jtag_ini_t;
    constant cycles   : in  natural;
    constant period   : in  time := C_JTAG_PERIOD
  );

  procedure vip_jtag_reset(
    signal   jtag_ini : out jtag_ini_t;
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  procedure vip_jtag_shift(
    signal   jtag_ini  : out jtag_ini_t;
    signal   jtag_tgt  : in  jtag_tgt_t;
    constant tms_pre   : in  std_logic_vector; -- TMS sequence to reach Shift
    constant data_out  : in  std_logic_vector; -- Data to shift out (TDI)
    variable data_in   : out std_logic_vector; -- Data shifted in (TDO)
    constant tms_post  : in  std_logic_vector; -- TMS sequence to leave Shift
    constant msg       : in  string := "";
    constant period    : in  time   := C_JTAG_PERIOD
  );

  procedure vip_jtag_read_idcode(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant exp_id   : in  std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  procedure vip_jtag_write_ir(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir_val   : in  std_logic_vector(4 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  procedure vip_jtag_write_ir(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir_val   : in  std_logic_vector(4 downto 0);
    variable ir_cap   : out std_logic_vector(4 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  -- One data register scan (current IR), returns the captured value
  procedure vip_jtag_dr_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant data_out : in  std_logic_vector;
    variable data_in  : out std_logic_vector;
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  -- dtmcs scan (selects IR DTMCS)
  procedure vip_jtag_dtmcs(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant wdata    : in  std_logic_vector(31 downto 0);
    variable rdata    : out std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  -- One dmi scan (IR must be DMI) followed by idle cycles in Run-Test/Idle
  procedure vip_jtag_dmi_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(6 downto 0);
    constant wdata    : in  std_logic_vector(31 downto 0);
    variable rdata    : out std_logic_vector(31 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := 5;
    constant msg      : in  string  := "";
    constant period   : in  time    := C_JTAG_PERIOD
  );

  -- Complete DMI access : request, then nop scans until the status is not
  -- busy (dmireset and retry on busy). Selects IR DMI.
  procedure vip_jtag_dmi_access(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(6 downto 0);
    constant wdata    : in  std_logic_vector(31 downto 0);
    variable rdata    : out std_logic_vector(31 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := 5;
    constant msg      : in  string  := "";
    constant period   : in  time    := C_JTAG_PERIOD
  );

  procedure vip_jtag_dmi_write(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(6 downto 0);
    constant data     : in  std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

  procedure vip_jtag_dmi_read(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(6 downto 0);
    variable data     : out std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  );

end package wardrv_vips;

package body wardrv_vips is

  -- ===========================================================================
  -- Reset VIP Implementation
  -- ===========================================================================
  procedure vip_reset_pulse(
    signal   reset_n : out std_logic;
    constant width   : in  time;
    constant msg     : in  string := ""
  ) is
  begin
    log(ID_BFM, "Asserting Reset " & msg);
    reset_n <= '0';
    wait for width;
    reset_n <= '1';
    log(ID_BFM, "Deasserting Reset " & msg);
  end procedure;

  -- ===========================================================================
  -- JTAG VIP Implementation
  -- ===========================================================================
  procedure vip_jtag_init(
    signal   jtag_ini : out jtag_ini_t;
    constant period   : in  time := C_JTAG_PERIOD
  ) is
  begin
    jtag_ini.tck    <= '0';
    jtag_ini.trst_n <= '0'; -- Active Reset
    jtag_ini.tms    <= '1';
    jtag_ini.tdi    <= '0';
    wait for period;
    jtag_ini.trst_n <= '1'; -- Release Reset
    wait for period;
  end procedure;

  procedure vip_jtag_clock(
    signal   jtag_ini : out jtag_ini_t;
    constant tms      : in  std_logic;
    constant tdi      : in  std_logic;
    constant period   : in  time := C_JTAG_PERIOD
  ) is
  begin
    jtag_ini.tck <= '0';
    jtag_ini.tms <= tms;
    jtag_ini.tdi <= tdi;
    wait for period / 2;
    jtag_ini.tck <= '1';
    wait for period / 2;
  end procedure;

  procedure vip_jtag_idle(
    signal   jtag_ini : out jtag_ini_t;
    constant cycles   : in  natural;
    constant period   : in  time := C_JTAG_PERIOD
  ) is
  begin
    for i in 1 to cycles loop
      vip_jtag_clock(jtag_ini, '0', '0', period);
    end loop;
  end procedure;

  procedure vip_jtag_reset(
    signal   jtag_ini : out jtag_ini_t;
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
  begin
    log(ID_BFM, "JTAG Reset (5 TMS High) " & msg);
    for i in 1 to 5 loop
      vip_jtag_clock(jtag_ini, '1', '0', period);
    end loop;
    -- Go to Run-Test/Idle
    vip_jtag_clock(jtag_ini, '0', '0', period);
  end procedure;

  procedure vip_jtag_shift(
    signal   jtag_ini  : out jtag_ini_t;
    signal   jtag_tgt  : in  jtag_tgt_t;
    constant tms_pre   : in  std_logic_vector;
    constant data_out  : in  std_logic_vector;
    variable data_in   : out std_logic_vector;
    constant tms_post  : in  std_logic_vector;
    constant msg       : in  string := "";
    constant period    : in  time   := C_JTAG_PERIOD
  ) is
    -- LSB first whatever the direction of the actual vectors
    alias    v_out     : std_logic_vector(data_out'length-1 downto 0) is data_out;
    variable v_in      : std_logic_vector(data_out'length-1 downto 0);
  begin
    -- Preamble (Navigate to Shift-DR/IR), first element first
    for i in tms_pre'range loop
      vip_jtag_clock(jtag_ini, tms_pre(i), '0', period);
    end loop;

    -- Shift Data
    for i in 0 to v_out'length - 1 loop
      -- 1. Drive TDI and TMS
      if i = v_out'length - 1 then
        jtag_ini.tms <= '1'; -- Exit1-DR/IR
      else
        jtag_ini.tms <= '0'; -- Shift-DR/IR
      end if;
      jtag_ini.tdi <= v_out(i);

      -- 2. Clock Pulse with Sampling
      jtag_ini.tck <= '0';          -- Falling Edge (TDO updates)
      wait for period / 2;
      v_in(i)      := jtag_tgt.tdo; -- Sample TDO
      jtag_ini.tck <= '1';          -- Rising Edge (Shift/Capture)
      wait for period / 2;
    end loop;
    data_in := v_in;

    -- Postamble (Navigate to Run-Test/Idle)
    for i in tms_post'range loop
      vip_jtag_clock(jtag_ini, tms_post(i), '0', period);
    end loop;
  end procedure;

  procedure vip_jtag_read_idcode(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant exp_id   : in  std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
    variable v_id : std_logic_vector(31 downto 0);
  begin
    log(ID_BFM, "JTAG Read IDCODE " & msg);
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_IDCODE, msg, period);
    vip_jtag_dr_scan (jtag_ini, jtag_tgt, x"00000000", v_id, msg, period);
    check_value(v_id, exp_id, ERROR, "Checking IDCODE");
  end procedure;

  procedure vip_jtag_write_ir(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir_val   : in  std_logic_vector(4 downto 0);
    variable ir_cap   : out std_logic_vector(4 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
  begin
    log(ID_BFM, "JTAG Write IR 0b" & to_string(ir_val) & " " & msg);
    -- Path: RTI -> Select-DR(1) -> Select-IR(1) -> Capture-IR(0) -> Shift-IR(0)
    -- Post: Exit1-IR -> Update-IR(1) -> RTI(0)
    vip_jtag_shift(jtag_ini, jtag_tgt, "1100", ir_val, ir_cap, "10", msg, period);
  end procedure;

  procedure vip_jtag_write_ir(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant ir_val   : in  std_logic_vector(4 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
    variable v_dummy : std_logic_vector(4 downto 0);
  begin
    vip_jtag_write_ir(jtag_ini, jtag_tgt, ir_val, v_dummy, msg, period);
  end procedure;

  procedure vip_jtag_dr_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant data_out : in  std_logic_vector;
    variable data_in  : out std_logic_vector;
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
  begin
    -- Path: RTI -> Select-DR(1) -> Capture-DR(0) -> Shift-DR(0)
    -- Post: Exit1-DR -> Update-DR(1) -> RTI(0)
    vip_jtag_shift(jtag_ini, jtag_tgt, "100", data_out, data_in, "10", msg, period);
  end procedure;

  procedure vip_jtag_dtmcs(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant wdata    : in  std_logic_vector(31 downto 0);
    variable rdata    : out std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
  begin
    log(ID_BFM, "JTAG DTMCS 0x" & to_hstring(wdata) & " " & msg);
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DTMCS, msg, period);
    vip_jtag_dr_scan (jtag_ini, jtag_tgt, wdata, rdata, msg, period);
  end procedure;

  procedure vip_jtag_dmi_scan(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(6 downto 0);
    constant wdata    : in  std_logic_vector(31 downto 0);
    variable rdata    : out std_logic_vector(31 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := 5;
    constant msg      : in  string  := "";
    constant period   : in  time    := C_JTAG_PERIOD
  ) is
    variable v_dr_out : std_logic_vector(40 downto 0);
    variable v_dr_in  : std_logic_vector(40 downto 0);
  begin
    v_dr_out := addr & wdata & op;
    vip_jtag_dr_scan(jtag_ini, jtag_tgt, v_dr_out, v_dr_in, msg, period);
    vip_jtag_idle   (jtag_ini, idle, period);
    rdata  := v_dr_in(33 downto 2);
    status := v_dr_in( 1 downto 0);
  end procedure;

  procedure vip_jtag_dmi_access(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant op       : in  std_logic_vector(1 downto 0);
    constant addr     : in  std_logic_vector(6 downto 0);
    constant wdata    : in  std_logic_vector(31 downto 0);
    variable rdata    : out std_logic_vector(31 downto 0);
    variable status   : out std_logic_vector(1 downto 0);
    constant idle     : in  natural := 5;
    constant msg      : in  string  := "";
    constant period   : in  time    := C_JTAG_PERIOD
  ) is
    constant C_RETRY_MAX : natural := 100;
    variable v_idle      : natural := idle;
    variable v_rdata     : std_logic_vector(31 downto 0);
    variable v_status    : std_logic_vector(1 downto 0);
    variable v_dtmcs     : std_logic_vector(31 downto 0);
  begin
    vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI, msg, period);

    -- Request : retried while the DTM is busy (the request was ignored)
    for retry in 1 to C_RETRY_MAX loop
      vip_jtag_dmi_scan(jtag_ini, jtag_tgt, op, addr, wdata, v_rdata, v_status, v_idle, msg & " (req)", period);
      exit when v_status /= DMI_RSP_BUSY;
      log(ID_BFM, "JTAG DMI busy on request, dmireset " & msg);
      vip_jtag_dtmcs   (jtag_ini, jtag_tgt, x"00010000", v_dtmcs, "dmireset", period);
      vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI, msg, period);
      v_idle := v_idle + 1;
    end loop;

    -- Response : nop scans until not busy (the request is still in progress)
    for retry in 1 to C_RETRY_MAX loop
      vip_jtag_dmi_scan(jtag_ini, jtag_tgt, DMI_OP_NOP, addr, x"00000000", v_rdata, v_status, v_idle, msg & " (rsp)", period);
      exit when v_status /= DMI_RSP_BUSY;
      log(ID_BFM, "JTAG DMI busy on response, dmireset " & msg);
      vip_jtag_dtmcs   (jtag_ini, jtag_tgt, x"00010000", v_dtmcs, "dmireset", period);
      vip_jtag_write_ir(jtag_ini, jtag_tgt, JTAG_IR_DMI, msg, period);
      v_idle := v_idle + 1;
    end loop;

    rdata  := v_rdata;
    status := v_status;
  end procedure;

  procedure vip_jtag_dmi_write(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(6 downto 0);
    constant data     : in  std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
    variable v_rdata  : std_logic_vector(31 downto 0);
    variable v_status : std_logic_vector(1 downto 0);
  begin
    log(ID_BFM, "JTAG DMI Write @0x" & to_hstring(addr) & " = 0x" & to_hstring(data) & " " & msg);
    vip_jtag_dmi_access(jtag_ini, jtag_tgt, DMI_OP_WRITE, addr, data, v_rdata, v_status, 5, msg, period);
    check_value(v_status, DMI_RSP_SUCCESS, ERROR, "DMI write status " & msg);
  end procedure;

  procedure vip_jtag_dmi_read(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant addr     : in  std_logic_vector(6 downto 0);
    variable data     : out std_logic_vector(31 downto 0);
    constant msg      : in  string := "";
    constant period   : in  time   := C_JTAG_PERIOD
  ) is
    variable v_rdata  : std_logic_vector(31 downto 0);
    variable v_status : std_logic_vector(1 downto 0);
  begin
    vip_jtag_dmi_access(jtag_ini, jtag_tgt, DMI_OP_READ, addr, x"00000000", v_rdata, v_status, 5, msg, period);
    check_value(v_status, DMI_RSP_SUCCESS, ERROR, "DMI read status " & msg);
    log(ID_BFM, "JTAG DMI Read  @0x" & to_hstring(addr) & " = 0x" & to_hstring(v_rdata) & " " & msg);
    data := v_rdata;
  end procedure;

end package body;