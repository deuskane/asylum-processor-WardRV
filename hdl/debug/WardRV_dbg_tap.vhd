-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_tap.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- JTAG Test Access Port (IEEE 1149.1, TCK domain), RISC-V Debug Spec 1.0
-- chapter 6.1 : IDCODE, BYPASS and the user data registers DTMCS / DMI
-- (WardRV_dbg_dtm). No boundary scan.
--  * 16-state TAP controller, transitions on the rising edge of TCK, back to
--    Test-Logic-Reset after 5 TCK with TMS = 1. Asynchronous reset : power-on
--    reset and TRST* (TRST_ENABLE).
--  * Capture / shift on the rising edge of TCK. The IR, the user registers
--    (update_dr_o) and TDO change on the falling edge of TCK.
--  * IR : Capture-IR loads "00001", IDCODE after reset, any opcode other than
--    IDCODE, DTMCS and DMI selects BYPASS.
--  * tdo_en is high in Shift-IR / Shift-DR only (tri-state at the pad).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_tap is
  generic (
    IDCODE_VALUE   : std_logic_vector(32-1 downto 0) := x"10000001"
   ;TRST_ENABLE    : boolean                         := true      -- Use TRST*
  );
  port (
    -- Power-on reset (asynchronous, active low)
    arst_b_i       : in  std_logic
    -- JTAG pins
   ;jtag_ini_i     : in  jtag_ini_t
   ;jtag_tgt_o     : out jtag_tgt_t
    -- User data registers (TCK domain)
   ;tck_o          : out std_logic
   ;trst_b_o       : out std_logic                                -- Power-on reset and TRST*
   ;tlr_o          : out std_logic                                -- Test-Logic-Reset
   ;ir_o           : out std_logic_vector(JTAG_IR_WIDTH-1 downto 0)
   ;capture_dr_o   : out std_logic                                -- Capture-DR
   ;shift_dr_o     : out std_logic                                -- Shift-DR
   ;update_dr_o    : out std_logic                                -- Update-DR
   ;tdi_o          : out std_logic
   ;user_tdo_i     : in  std_logic                                -- LSB of the user register
  );
end entity WardRV_dbg_tap;

architecture rtl of WardRV_dbg_tap is

  type tap_state_t is (TEST_LOGIC_RESET, RUN_TEST_IDLE,
                       SELECT_DR_SCAN, CAPTURE_DR, SHIFT_DR, EXIT1_DR, PAUSE_DR, EXIT2_DR, UPDATE_DR,
                       SELECT_IR_SCAN, CAPTURE_IR, SHIFT_IR, EXIT1_IR, PAUSE_IR, EXIT2_IR, UPDATE_IR);

  constant IR_CAPTURE  : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "00001";

  signal   tck         : std_logic;
  signal   tms         : std_logic;
  signal   tdi         : std_logic;
  signal   rst_b       : std_logic;

  signal   state_r     : tap_state_t;
  signal   state_next  : tap_state_t;

  signal   ir_shift_r  : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  signal   ir_r        : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  signal   idcode_r    : std_logic_vector(32-1 downto 0);
  signal   bypass_r    : std_logic;
  signal   tdo         : std_logic;
  signal   tdo_r       : std_logic;
  signal   tdo_en_r    : std_logic;

begin  -- architecture rtl

  -- IEEE 1149.1 : IDCODE bit 0 = 1, manufacturer identity 0x7F reserved
  assert IDCODE_VALUE(0) = '1' and IDCODE_VALUE(11 downto 1) /= "00001111111"
    report "WardRV_dbg_tap : illegal IDCODE_VALUE" severity failure;

  tck   <= jtag_ini_i.tck;
  tms   <= jtag_ini_i.tms;
  tdi   <= jtag_ini_i.tdi;
  rst_b <= arst_b_i and jtag_ini_i.trst_n when TRST_ENABLE else
           arst_b_i;

  -----------------------------------------------------------------------------
  -- TAP controller (IEEE 1149.1 figure 6-1)
  -----------------------------------------------------------------------------
  process(all)
  begin
    case state_r is
      when TEST_LOGIC_RESET => state_next <= TEST_LOGIC_RESET when tms = '1' else RUN_TEST_IDLE;
      when RUN_TEST_IDLE    => state_next <= SELECT_DR_SCAN   when tms = '1' else RUN_TEST_IDLE;
      when SELECT_DR_SCAN   => state_next <= SELECT_IR_SCAN   when tms = '1' else CAPTURE_DR;
      when CAPTURE_DR       => state_next <= EXIT1_DR         when tms = '1' else SHIFT_DR;
      when SHIFT_DR         => state_next <= EXIT1_DR         when tms = '1' else SHIFT_DR;
      when EXIT1_DR         => state_next <= UPDATE_DR        when tms = '1' else PAUSE_DR;
      when PAUSE_DR         => state_next <= EXIT2_DR         when tms = '1' else PAUSE_DR;
      when EXIT2_DR         => state_next <= UPDATE_DR        when tms = '1' else SHIFT_DR;
      when UPDATE_DR        => state_next <= SELECT_DR_SCAN   when tms = '1' else RUN_TEST_IDLE;
      when SELECT_IR_SCAN   => state_next <= TEST_LOGIC_RESET when tms = '1' else CAPTURE_IR;
      when CAPTURE_IR       => state_next <= EXIT1_IR         when tms = '1' else SHIFT_IR;
      when SHIFT_IR         => state_next <= EXIT1_IR         when tms = '1' else SHIFT_IR;
      when EXIT1_IR         => state_next <= UPDATE_IR        when tms = '1' else PAUSE_IR;
      when PAUSE_IR         => state_next <= EXIT2_IR         when tms = '1' else PAUSE_IR;
      when EXIT2_IR         => state_next <= UPDATE_IR        when tms = '1' else SHIFT_IR;
      when UPDATE_IR        => state_next <= SELECT_DR_SCAN   when tms = '1' else RUN_TEST_IDLE;
    end case;
  end process;

  -----------------------------------------------------------------------------
  -- Rising edge of TCK : state, capture and shift of IR, IDCODE and BYPASS
  -----------------------------------------------------------------------------
  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      state_r    <= TEST_LOGIC_RESET;
      ir_shift_r <= IR_CAPTURE;
      idcode_r   <= IDCODE_VALUE;
      bypass_r   <= '0';
    elsif rising_edge(tck)
    then
      state_r    <= state_next;

      if state_r = CAPTURE_IR
      then
        ir_shift_r <= IR_CAPTURE;
      elsif state_r = SHIFT_IR
      then
        ir_shift_r <= tdi & ir_shift_r(JTAG_IR_WIDTH-1 downto 1);
      end if;

      if state_r = CAPTURE_DR
      then
        idcode_r   <= IDCODE_VALUE;
        bypass_r   <= '0';
      elsif state_r = SHIFT_DR
      then
        idcode_r   <= tdi & idcode_r(32-1 downto 1);
        bypass_r   <= tdi;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Falling edge of TCK : instruction register and TDO
  -----------------------------------------------------------------------------
  tdo <= ir_shift_r(0) when state_r = SHIFT_IR                         else
         idcode_r(0)   when ir_r = JTAG_IR_IDCODE                      else
         user_tdo_i    when ir_r = JTAG_IR_DTMCS or ir_r = JTAG_IR_DMI else
         bypass_r;

  process(tck, rst_b)
  begin
    if rst_b = '0'
    then
      ir_r     <= JTAG_IR_IDCODE;
      tdo_r    <= '0';
      tdo_en_r <= '0';
    elsif falling_edge(tck)
    then
      if state_r = TEST_LOGIC_RESET
      then
        ir_r   <= JTAG_IR_IDCODE;
      elsif state_r = UPDATE_IR
      then
        ir_r   <= ir_shift_r;
      end if;

      tdo_r    <= tdo;
      tdo_en_r <= '1' when state_r = SHIFT_IR or state_r = SHIFT_DR else '0';
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------
  jtag_tgt_o.tdo    <= tdo_r;
  jtag_tgt_o.tdo_en <= tdo_en_r;

  tck_o             <= tck;
  trst_b_o          <= rst_b;
  tlr_o             <= '1' when state_r = TEST_LOGIC_RESET else '0';
  ir_o              <= ir_r;
  capture_dr_o      <= '1' when state_r = CAPTURE_DR       else '0';
  shift_dr_o        <= '1' when state_r = SHIFT_DR         else '0';
  update_dr_o       <= '1' when state_r = UPDATE_DR        else '0';
  tdi_o             <= tdi;

end architecture rtl;
