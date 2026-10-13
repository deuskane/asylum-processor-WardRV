-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_tap.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- JTAG Test Access Port controller (TCK domain).
-- The TAP state machine and its clocking follow IEEE 1149.1-2013, without
-- boundary scan : only the IDCODE, BYPASS and the two RISC-V DTM user data
-- registers (DTMCS, DMI) are implemented.
--  * 16-state FSM, transitions on the rising edge of TCK. Test-Logic-Reset is
--    reached after at most 5 TCK with TMS=1 from any state.
--  * Asynchronous reset : TRST* (optional, generic TRST_ENABLE) combined with
--    the power-on reset arst_b_i (TAP in Test-Logic-Reset at power-up).
--  * Dedicated IR shift register. Capture-IR loads "00001", the IR is
--    updated on the falling edge of TCK in Update-IR, reset value IDCODE.
--  * DR capture/shift on the rising edge of TCK, update on the falling edge
--    of TCK in Update-DR (update_dr_o is a level, the user register samples
--    it on the falling edge).
--  * IR all-ones and any undecoded opcode select BYPASS.
--  * TDO and its enable are retimed on the falling edge of TCK. tdo_en is
--    high only in Shift-IR and Shift-DR. No combinational TDI/TMS -> TDO path.
--  * Fully static design (flip-flops only, no clock gating) : TCK can be
--    stopped at 0 or 1 indefinitely.
-- Note : the pull-ups on TMS, TDI and TRST* belong to the pads.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_tap is
  generic (
    IDCODE_VALUE : std_logic_vector(31 downto 0) := x"10000001";
    TRST_ENABLE  : boolean                       := true
  );
  port (
    arst_b_i     : in  std_logic;  -- Power-on reset (asynchronous, active low)

    -- JTAG pins
    jtag_ini_i   : in  jtag_ini_t;
    jtag_tgt_o   : out jtag_tgt_t;

    -- User data registers interface (TCK domain)
    tck_o        : out std_logic;  -- TCK
    trst_b_o     : out std_logic;  -- TCK domain asynchronous reset (power-on and TRST*)
    tlr_o        : out std_logic;  -- TAP in Test-Logic-Reset
    ir_o         : out std_logic_vector(JTAG_IR_WIDTH-1 downto 0); -- Current instruction
    capture_dr_o : out std_logic;  -- Capture-DR : load on the rising  edge of TCK
    shift_dr_o   : out std_logic;  -- Shift-DR   : shift on the rising edge of TCK
    update_dr_o  : out std_logic;  -- Update-DR  : update on the falling edge of TCK
    tdi_o        : out std_logic;
    user_tdo_i   : in  std_logic   -- LSB of the selected user data register
  );
end entity WardRV_dbg_tap;

architecture rtl of WardRV_dbg_tap is

  type tap_state_t is (TEST_LOGIC_RESET, RUN_TEST_IDLE,
                       SELECT_DR_SCAN, CAPTURE_DR, SHIFT_DR, EXIT1_DR, PAUSE_DR, EXIT2_DR, UPDATE_DR,
                       SELECT_IR_SCAN, CAPTURE_IR, SHIFT_IR, EXIT1_IR, PAUSE_IR, EXIT2_IR, UPDATE_IR);

  constant IR_CAPTURE   : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := (0 => '1', others => '0');

  signal tck            : std_logic;
  signal tms            : std_logic;
  signal tdi            : std_logic;
  signal rst_b          : std_logic;

  signal state_r        : tap_state_t;
  signal state_r_next   : tap_state_t;

  signal ir_shift_r     : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  signal ir_r           : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  signal idcode_r       : std_logic_vector(31 downto 0);
  signal bypass_r       : std_logic;

  signal sel_idcode     : std_logic;
  signal sel_user       : std_logic;
  signal tdo_mux        : std_logic;
  signal tdo_r          : std_logic;
  signal tdo_en_r       : std_logic;

begin

  -- IEEE 1149.1 : manufacturer identity 0x7F (bits 11:1) is reserved, bit 0 = 1
  assert IDCODE_VALUE(0) = '1'
    report "WardRV_dbg_tap : IDCODE_VALUE(0) must be '1'" severity failure;
  assert IDCODE_VALUE(11 downto 1) /= "00001111111"
    report "WardRV_dbg_tap : IDCODE_VALUE manufacturer identity 0x7F is reserved" severity failure;

  tck   <= jtag_ini_i.tck;
  tms   <= jtag_ini_i.tms;
  tdi   <= jtag_ini_i.tdi;

  -- Asynchronous reset of the test logic : power-on reset and optional TRST*
  rst_b <= arst_b_i and jtag_ini_i.trst_n when TRST_ENABLE else
           arst_b_i;

  --------------------------------------------------------------------
  -- TAP controller state machine (IEEE 1149.1 figure 6-1)
  --------------------------------------------------------------------
  process(all)
  begin
    state_r_next <= state_r;
    case state_r is
      when TEST_LOGIC_RESET => if tms = '1' then state_r_next <= TEST_LOGIC_RESET; else state_r_next <= RUN_TEST_IDLE; end if;
      when RUN_TEST_IDLE    => if tms = '1' then state_r_next <= SELECT_DR_SCAN  ; else state_r_next <= RUN_TEST_IDLE; end if;
      when SELECT_DR_SCAN   => if tms = '1' then state_r_next <= SELECT_IR_SCAN  ; else state_r_next <= CAPTURE_DR   ; end if;
      when CAPTURE_DR       => if tms = '1' then state_r_next <= EXIT1_DR        ; else state_r_next <= SHIFT_DR     ; end if;
      when SHIFT_DR         => if tms = '1' then state_r_next <= EXIT1_DR        ; else state_r_next <= SHIFT_DR     ; end if;
      when EXIT1_DR         => if tms = '1' then state_r_next <= UPDATE_DR       ; else state_r_next <= PAUSE_DR     ; end if;
      when PAUSE_DR         => if tms = '1' then state_r_next <= EXIT2_DR        ; else state_r_next <= PAUSE_DR     ; end if;
      when EXIT2_DR         => if tms = '1' then state_r_next <= UPDATE_DR       ; else state_r_next <= SHIFT_DR     ; end if;
      when UPDATE_DR        => if tms = '1' then state_r_next <= SELECT_DR_SCAN  ; else state_r_next <= RUN_TEST_IDLE; end if;
      when SELECT_IR_SCAN   => if tms = '1' then state_r_next <= TEST_LOGIC_RESET; else state_r_next <= CAPTURE_IR   ; end if;
      when CAPTURE_IR       => if tms = '1' then state_r_next <= EXIT1_IR        ; else state_r_next <= SHIFT_IR     ; end if;
      when SHIFT_IR         => if tms = '1' then state_r_next <= EXIT1_IR        ; else state_r_next <= SHIFT_IR     ; end if;
      when EXIT1_IR         => if tms = '1' then state_r_next <= UPDATE_IR       ; else state_r_next <= PAUSE_IR     ; end if;
      when PAUSE_IR         => if tms = '1' then state_r_next <= EXIT2_IR        ; else state_r_next <= PAUSE_IR     ; end if;
      when EXIT2_IR         => if tms = '1' then state_r_next <= UPDATE_IR       ; else state_r_next <= SHIFT_IR     ; end if;
      when UPDATE_IR        => if tms = '1' then state_r_next <= SELECT_DR_SCAN  ; else state_r_next <= RUN_TEST_IDLE; end if;
    end case;
  end process;

  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      state_r <= TEST_LOGIC_RESET;
    elsif rising_edge(tck)
    then
      state_r <= state_r_next;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Instruction register
  -- Shift register : capture / shift on the rising edge of TCK
  -- Instruction    : update on the falling edge of TCK in Update-IR,
  --                  IDCODE in Test-Logic-Reset
  --------------------------------------------------------------------
  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      ir_shift_r <= IR_CAPTURE;
    elsif rising_edge(tck)
    then
      if    state_r = CAPTURE_IR
      then
        ir_shift_r <= IR_CAPTURE;
      elsif state_r = SHIFT_IR
      then
        ir_shift_r <= tdi & ir_shift_r(JTAG_IR_WIDTH-1 downto 1);
      end if;
    end if;
  end process;

  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      ir_r <= JTAG_IR_IDCODE;
    elsif falling_edge(tck)
    then
      if    state_r = TEST_LOGIC_RESET
      then
        ir_r <= JTAG_IR_IDCODE;
      elsif state_r = UPDATE_IR
      then
        ir_r <= ir_shift_r;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Internal data registers : IDCODE and BYPASS
  --------------------------------------------------------------------
  sel_idcode <= '1' when ir_r = JTAG_IR_IDCODE else '0';
  sel_user   <= '1' when ir_r = JTAG_IR_DTMCS or ir_r = JTAG_IR_DMI else '0';

  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      idcode_r <= IDCODE_VALUE;
      bypass_r <= '0';
    elsif rising_edge(tck)
    then
      if    state_r = CAPTURE_DR
      then
        idcode_r <= IDCODE_VALUE;
        bypass_r <= '0';
      elsif state_r = SHIFT_DR
      then
        idcode_r <= tdi & idcode_r(31 downto 1);
        bypass_r <= tdi;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- TDO : retimed on the falling edge of TCK
  --------------------------------------------------------------------
  tdo_mux <= ir_shift_r(0) when state_r = SHIFT_IR else
             idcode_r(0)   when sel_idcode = '1'   else
             user_tdo_i    when sel_user   = '1'   else
             bypass_r;

  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      tdo_r    <= '0';
      tdo_en_r <= '0';
    elsif falling_edge(tck)
    then
      tdo_r    <= tdo_mux;
      if state_r = SHIFT_IR or state_r = SHIFT_DR
      then
        tdo_en_r <= '1';
      else
        tdo_en_r <= '0';
      end if;
    end if;
  end process;

  jtag_tgt_o.tdo    <= tdo_r;
  jtag_tgt_o.tdo_en <= tdo_en_r;

  --------------------------------------------------------------------
  -- User data registers interface
  --------------------------------------------------------------------
  tck_o        <= tck;
  trst_b_o     <= rst_b;
  tlr_o        <= '1' when state_r = TEST_LOGIC_RESET else '0';
  ir_o         <= ir_r;
  capture_dr_o <= '1' when state_r = CAPTURE_DR else '0';
  shift_dr_o   <= '1' when state_r = SHIFT_DR   else '0';
  update_dr_o  <= '1' when state_r = UPDATE_DR  else '0';
  tdi_o        <= tdi;

end architecture rtl;
