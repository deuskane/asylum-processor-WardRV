library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use asylum.WardRV_pkg.all;

package wardrv_vips is

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
  -- ===========================================================================
  procedure vip_jtag_init(
    signal jtag_ini : out jtag_ini_t
  );

  procedure vip_jtag_reset(
    signal jtag_ini : out jtag_ini_t;
    constant msg    : in  string := ""
  );

  procedure vip_jtag_shift(
    signal   jtag_ini  : out jtag_ini_t;
    signal   jtag_tgt  : in  jtag_tgt_t;
    constant tms_pre   : in  std_logic_vector; -- TMS sequence to reach Shift
    constant data_out  : in  std_logic_vector; -- Data to shift out (TDI)
    variable data_in   : out std_logic_vector; -- Data shifted in (TDO)
    constant tms_post  : in  std_logic_vector; -- TMS sequence to leave Shift
    constant msg       : in  string := ""
  );

  procedure vip_jtag_read_idcode(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant exp_id   : in  std_logic_vector(31 downto 0);
    constant msg      : in  string := ""
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
    signal jtag_ini : out jtag_ini_t
  ) is
  begin
    jtag_ini.tck    <= '0';
    jtag_ini.trst_n <= '0'; -- Active Reset
    jtag_ini.tms    <= '1';
    jtag_ini.tdi    <= '0';
    wait for C_JTAG_PERIOD;
    jtag_ini.trst_n <= '1'; -- Release Reset
  end procedure;

  procedure vip_jtag_clock(
    signal jtag_ini : out jtag_ini_t;
    constant tms    : in  std_logic;
    constant tdi    : in  std_logic
  ) is
  begin
    jtag_ini.tck <= '0';
    jtag_ini.tms <= tms;
    jtag_ini.tdi <= tdi;
    wait for C_JTAG_PERIOD / 2;
    jtag_ini.tck <= '1';
    wait for C_JTAG_PERIOD / 2;
  end procedure;

  procedure vip_jtag_reset(
    signal jtag_ini : out jtag_ini_t;
    constant msg    : in  string := ""
  ) is
  begin
    log(ID_BFM, "JTAG Reset (5 TMS High) " & msg);
    for i in 1 to 5 loop
      vip_jtag_clock(jtag_ini, '1', '0');
    end loop;
    -- Go to Run-Test/Idle
    vip_jtag_clock(jtag_ini, '0', '0');
  end procedure;

  procedure vip_jtag_shift(
    signal   jtag_ini  : out jtag_ini_t;
    signal   jtag_tgt  : in  jtag_tgt_t;
    constant tms_pre   : in  std_logic_vector;
    constant data_out  : in  std_logic_vector;
    variable data_in   : out std_logic_vector;
    constant tms_post  : in  std_logic_vector;
    constant msg       : in  string := ""
  ) is
  begin
    -- Preamble (Navigate to Shift-DR/IR)
    for i in tms_pre'range loop
      vip_jtag_clock(jtag_ini, tms_pre(i), '0');
    end loop;

    -- Shift Data
    for i in 0 to data_out'length - 1 loop
      -- Sample TDO (on falling edge of previous clock, effectively now before rising)
      data_in(i) := jtag_tgt.tdo;
      
      -- Drive TDI and TMS (Last bit needs TMS=1 to exit Shift state)
      if i = data_out'length - 1 then
        vip_jtag_clock(jtag_ini, '1', data_out(i)); -- Exit1-DR/IR
      else
        vip_jtag_clock(jtag_ini, '0', data_out(i)); -- Shift-DR/IR
      end if;
    end loop;

    -- Postamble (Navigate to Run-Test/Idle)
    for i in tms_post'range loop
      vip_jtag_clock(jtag_ini, tms_post(i), '0');
    end loop;
  end procedure;

  procedure vip_jtag_read_idcode(
    signal   jtag_ini : out jtag_ini_t;
    signal   jtag_tgt : in  jtag_tgt_t;
    constant exp_id   : in  std_logic_vector(31 downto 0);
    constant msg      : in  string := ""
  ) is
    variable v_id : std_logic_vector(31 downto 0);
  begin
    log(ID_BFM, "JTAG Read IDCODE " & msg);
    -- Path to Shift-DR from RTI: 1 (Select-DR), 0 (Capture-DR), 0 (Shift-DR) -> "001" (LSB first in loop logic above? No, range loop)
    -- Using standard range loop: tms_pre(low) is first.
    -- Path: RTI->Select-DR(1)->Capture-DR(0)->Shift-DR(0).
    vip_jtag_shift(jtag_ini, jtag_tgt, "100", x"00000000", v_id, "10", msg); -- Post: Exit1->Update(1)->RTI(0)
    check_value(v_id, exp_id, ERROR, "Checking IDCODE");
  end procedure;

end package body;