-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dtm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- RISC-V JTAG Debug Transport Module (Debug Spec 1.0 chapter 6.1.4 / 6.1.5),
-- TCK domain : the user data registers of jtag_tap (asylum-debug-jtag).
--  * dtmcs (32 bits) : version 1, abits 7, dmistat, idle, dmireset and
--    dmihardreset (both clear the sticky busy status).
--  * dmi (41 bits)   : address[40:34], data[33:2], op[1:0].
--    Update-DR with op = 1 (read) or 2 (write) launches a DMI request.
--    A request (or a capture) while the previous request is in progress sets
--    the sticky busy status (op = 3) : the request is ignored until dmireset.
--    Capture-DR returns the address of the last request, the read data of
--    the last request and the status.
-- The Debug Module never fails a DMI access : the status is 0 or 3.
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

library asylum;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_dtm is
  generic (
    IDLE_CYCLES    : natural := 5                                 -- dtmcs.idle (0..7)
  );
  port (
    -- TAP (TCK domain)
    tck_i          : in  std_logic
   ;trst_b_i       : in  std_logic
   ;tlr_i          : in  std_logic
   ;ir_i           : in  std_logic_vector(JTAG_IR_WIDTH-1 downto 0)
   ;capture_dr_i   : in  std_logic
   ;shift_dr_i     : in  std_logic
   ;update_dr_i    : in  std_logic
   ;tdi_i          : in  std_logic
   ;tdo_o          : out std_logic
    -- DMI (TCK side of WardRV_dbg_dmi_cdc)
   ;dmi_req_o      : out dmi_req_t                                -- valid during Update-DR
   ;dmi_busy_i     : in  std_logic                                -- Request in progress
   ;dmi_rdata_i    : in  std_logic_vector(32-1 downto 0)          -- Read data of the last request
  );
end entity WardRV_dbg_dtm;

architecture rtl of WardRV_dbg_dtm is

  constant DMI_WIDTH   : natural := DMI_ABITS + 32 + 2;           -- address & data & op

  signal   sel_dtmcs   : std_logic;
  signal   sel_dmi     : std_logic;
  signal   dtmcs       : std_logic_vector(32-1 downto 0);
  signal   dr_r        : std_logic_vector(DMI_WIDTH-1 downto 0);  -- dtmcs : bits 31:0
  alias    dr_addr     : std_logic_vector(DMI_ABITS-1 downto 0) is dr_r(DMI_WIDTH-1 downto 34);
  alias    dr_data     : std_logic_vector(32-1 downto 0)        is dr_r(33 downto 2);
  alias    dr_op       : std_logic_vector(1 downto 0)           is dr_r(1 downto 0);
  signal   dr_request  : std_logic;                               -- op = 1 or 2
  signal   busy_err_r  : std_logic;                               -- Sticky busy status
  signal   addr_r      : std_logic_vector(DMI_ABITS-1 downto 0);
  signal   launch      : std_logic;

begin  -- architecture rtl

  sel_dtmcs  <= '1' when ir_i = JTAG_IR_DTMCS else '0';
  sel_dmi    <= '1' when ir_i = JTAG_IR_DMI   else '0';

  dtmcs      <= (31 downto 15 => '0')                                         &
                std_logic_vector(to_unsigned(minimum(IDLE_CYCLES, 7), 3))     & -- idle
                busy_err_r & busy_err_r                                       & -- dmistat
                std_logic_vector(to_unsigned(DMI_ABITS, 6))                   & -- abits
                "0001";                                                         -- version 1

  -----------------------------------------------------------------------------
  -- Shift register : capture and shift on the rising edge of TCK
  -----------------------------------------------------------------------------
  process(tck_i, trst_b_i)
  begin
    if trst_b_i = '0'
    then
      dr_r <= (others => '0');
    elsif rising_edge(tck_i)
    then
      if sel_dtmcs = '1'
      then
        if capture_dr_i = '1'
        then
          dr_r(32-1 downto 0) <= dtmcs;
        elsif shift_dr_i = '1'
        then
          dr_r(32-1 downto 0) <= tdi_i & dr_r(32-1 downto 1);
        end if;
      elsif sel_dmi = '1'
      then
        if capture_dr_i = '1'
        then
          dr_r <= addr_r & dmi_rdata_i & busy_err_r & busy_err_r;
        elsif shift_dr_i = '1'
        then
          dr_r <= tdi_i & dr_r(DMI_WIDTH-1 downto 1);
        end if;
      end if;
    end if;
  end process;

  tdo_o      <= dr_r(0);

  -----------------------------------------------------------------------------
  -- DMI request : Update-DR of dmi, sampled on the falling edge of TCK
  -----------------------------------------------------------------------------
  dr_request <= '1' when dr_op = "01" or dr_op = "10" else '0';
  launch     <= sel_dmi and update_dr_i and dr_request and not busy_err_r and not dmi_busy_i;

  dmi_req_o.valid <= launch;
  dmi_req_o.we    <= dr_op(1);
  dmi_req_o.addr  <= dr_addr;
  dmi_req_o.wdata <= dr_data;

  -----------------------------------------------------------------------------
  -- Status and address of the last request (falling edge of TCK)
  -----------------------------------------------------------------------------
  process(tck_i, trst_b_i)
  begin
    if trst_b_i = '0'
    then
      busy_err_r <= '0';
      addr_r     <= (others => '0');
    elsif falling_edge(tck_i)
    then
      if tlr_i = '1'
      then
        busy_err_r <= '0';
      elsif sel_dmi = '1' and dmi_busy_i = '1' and (capture_dr_i = '1' or (update_dr_i = '1' and dr_request = '1'))
      then
        -- The debugger did not wait long enough in Run-Test/Idle
        busy_err_r <= '1';
      elsif launch = '1'
      then
        addr_r     <= dr_addr;
      elsif sel_dtmcs = '1' and update_dr_i = '1' and (dr_r(16) = '1' or dr_r(17) = '1')
      then
        -- dmireset / dmihardreset (the request in progress always completes)
        busy_err_r <= '0';
      end if;
    end if;
  end process;

end architecture rtl;
