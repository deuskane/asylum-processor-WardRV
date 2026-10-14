-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dtm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- RISC-V JTAG Debug Transport Module (Debug Spec 1.0, TCK domain only).
-- User data registers of WardRV_dbg_tap :
--  * dtmcs (32 bits) : version=1, abits=7, dmistat (sticky), idle,
--                      dmireset (W1) and dmihardreset (W1)
--  * dmi   (41 bits) : address[40:34], data[33:2], op[1:0]
-- Capture and shift on the rising edge of TCK, update on the falling edge of
-- TCK in Update-DR.
-- Update-DR of dmi with op=1 (read) or op=2 (write) launches a DMI request,
-- only when the sticky status is 0 and the DTM is not busy. Otherwise the
-- sticky status goes to busy (3) and the request is ignored. op=0 (nop) and
-- op=3 (reserved) launch nothing.
-- Capture-DR of dmi loads the address of the last request, the data of the
-- last response and the sticky status in op (busy if a transaction is still
-- in progress).
-- dmihardreset abandons the transaction in progress : its result is ignored
-- when it comes back, the CDC handshake is not broken (the DTM stays busy
-- until the handshake ends). The address and data of dmi go back to 0.
-- A failure received while the sticky status is busy is not lost : it is
-- reported (sticky failed) by the dmireset that clears the busy status.
-- A response received while no transaction is in flight for the DTM (request
-- launched before TRST*, the CDC is not reset by TRST*) is ignored.
-- No clk domain logic here : the DMI request / response go through
-- WardRV_dbg_dmi_cdc (TCK side, falling edge of TCK).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere dmihardreset : address / data back to reset
--                               Response ignored when no transaction is in
--                               flight (request launched before TRST*)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_dtm is
  generic (
    IDLE_CYCLES  : natural := 5    -- dtmcs.idle : Run-Test/Idle cycles advised between DMI scans
  );
  port (
    -- TAP interface (TCK domain)
    tck_i        : in  std_logic;
    trst_b_i     : in  std_logic;
    tlr_i        : in  std_logic;
    ir_i         : in  std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
    capture_dr_i : in  std_logic;
    shift_dr_i   : in  std_logic;
    update_dr_i  : in  std_logic;
    tdi_i        : in  std_logic;
    tdo_o        : out std_logic;

    -- DMI (TCK side of WardRV_dbg_dmi_cdc)
    dmi_req_o    : out dmi_req_t;  -- valid : launch strobe, sampled on the falling edge of TCK
    dmi_rsp_i    : in  dmi_rsp_t;  -- valid : response available (one TCK cycle)
    dmi_busy_i   : in  std_logic   -- A transaction is in progress
  );
end entity WardRV_dbg_dtm;

architecture rtl of WardRV_dbg_dtm is

  constant DTMCS_VERSION : std_logic_vector(3 downto 0) := "0001";      -- Debug Spec 0.13 / 1.0
  constant DTMCS_ABITS   : std_logic_vector(5 downto 0) := std_logic_vector(to_unsigned(DMI_ABITS, 6));

  function idle_field return std_logic_vector is
  begin
    if IDLE_CYCLES > 7 then
      return "111";
    end if;
    return std_logic_vector(to_unsigned(IDLE_CYCLES, 3));
  end function;

  signal sel_dtmcs      : std_logic;
  signal sel_dmi        : std_logic;

  -- Shift register shared by dtmcs (bits 31:0) and dmi (bits 40:0)
  signal dr_r           : std_logic_vector(DMI_DR_WIDTH-1 downto 0);
  alias  dr_addr        : std_logic_vector(DMI_ABITS-1 downto 0) is dr_r(DMI_DR_WIDTH-1 downto 34);
  alias  dr_data        : std_logic_vector(31 downto 0)          is dr_r(33 downto 2);
  alias  dr_op          : std_logic_vector(1 downto 0)           is dr_r(1 downto 0);

  signal dtmcs          : std_logic_vector(31 downto 0);

  -- DTM state (falling edge of TCK)
  signal sticky_r       : std_logic_vector(1 downto 0);
  signal addr_r         : std_logic_vector(DMI_ABITS-1 downto 0);
  signal data_r         : std_logic_vector(31 downto 0);
  signal inflight_r     : std_logic;
  signal discard_r      : std_logic;
  signal fail_pending_r : std_logic;  -- Failure seen while the sticky status was busy

  signal dmi_op_valid   : std_logic;
  signal dmi_launch     : std_logic;

begin

  sel_dtmcs <= '1' when ir_i = JTAG_IR_DTMCS else '0';
  sel_dmi   <= '1' when ir_i = JTAG_IR_DMI   else '0';

  dtmcs     <= (31 downto 21 => '0') &
               "000"         & -- errinfo (not implemented)
               '0'           & -- dmihardreset (W1)
               '0'           & -- dmireset     (W1)
               '0'           &
               idle_field    &
               sticky_r      & -- dmistat
               DTMCS_ABITS   &
               DTMCS_VERSION;

  --------------------------------------------------------------------
  -- Shift register : capture and shift on the rising edge of TCK
  --------------------------------------------------------------------
  process(tck_i, trst_b_i)
  begin
    if trst_b_i = '0'
    then
      dr_r <= (others => '0');
    elsif rising_edge(tck_i)
    then
      if capture_dr_i = '1'
      then
        if sel_dtmcs = '1'
        then
          dr_r(31 downto 0) <= dtmcs;
        elsif sel_dmi = '1'
        then
          dr_r <= addr_r & data_r & sticky_r;
        end if;
      elsif shift_dr_i = '1'
      then
        if sel_dtmcs = '1'
        then
          dr_r(31 downto 0) <= tdi_i & dr_r(31 downto 1);
        elsif sel_dmi = '1'
        then
          dr_r <= tdi_i & dr_r(DMI_DR_WIDTH-1 downto 1);
        end if;
      end if;
    end if;
  end process;

  tdo_o <= dr_r(0);

  --------------------------------------------------------------------
  -- DMI request launch (Update-DR of dmi, falling edge of TCK)
  --------------------------------------------------------------------
  dmi_op_valid    <= '1' when dr_op = DMI_OP_READ or dr_op = DMI_OP_WRITE else '0';
  dmi_launch      <= '1' when update_dr_i  = '1' and sel_dmi    = '1' and
                                dmi_op_valid = '1' and dmi_busy_i = '0' and
                                sticky_r     = DMI_RSP_SUCCESS else
                     '0';

  dmi_req_o.valid <= dmi_launch;
  dmi_req_o.op    <= dr_op;
  dmi_req_o.addr  <= dr_addr;
  dmi_req_o.data  <= dr_data;

  --------------------------------------------------------------------
  -- DTM state : sticky status, last address / data (falling edge of TCK)
  --------------------------------------------------------------------
  process(tck_i, trst_b_i)
    variable v_inflight : std_logic;
  begin
    if trst_b_i = '0'
    then
      sticky_r   <= DMI_RSP_SUCCESS;
      addr_r     <= (others => '0');
      data_r     <= (others => '0');
      inflight_r <= '0';
      discard_r  <= '0';
      fail_pending_r <= '0';
    elsif falling_edge(tck_i)
    then
      v_inflight := inflight_r;

      -- Response of the DM
      if dmi_rsp_i.valid = '1'
      then
        v_inflight := '0';
        discard_r  <= '0';
        -- Result of a transaction abandoned (dmihardreset, Test-Logic-Reset)
        -- or launched before TRST* (inflight_r reset) : ignored
        if discard_r = '0' and inflight_r = '1'
        then
          data_r <= dmi_rsp_i.data;
          if dmi_rsp_i.op /= DMI_RSP_SUCCESS
          then
            if sticky_r = DMI_RSP_SUCCESS
            then
              sticky_r       <= DMI_RSP_FAILED;
            else
              -- Busy reported : the failure is reported after dmireset
              fail_pending_r <= '1';
            end if;
          end if;
        end if;
      end if;

      if tlr_i = '1'
      then
        -- Test-Logic-Reset : reset the DTM, forget the transaction in progress
        sticky_r       <= DMI_RSP_SUCCESS;
        fail_pending_r <= '0';
        discard_r      <= v_inflight;

      elsif capture_dr_i = '1' and sel_dmi = '1'
      then
        -- dmi scan while a transaction is in progress
        if dmi_busy_i = '1' and sticky_r = DMI_RSP_SUCCESS
        then
          sticky_r <= DMI_RSP_BUSY;
        end if;

      elsif update_dr_i = '1' and sel_dmi = '1'
      then
        if dmi_launch = '1'
        then
          v_inflight := '1';
          addr_r     <= dr_addr;
        elsif dmi_op_valid = '1' and sticky_r = DMI_RSP_SUCCESS
        then
          -- Request while busy : ignored
          sticky_r   <= DMI_RSP_BUSY;
        end if;

      elsif update_dr_i = '1' and sel_dtmcs = '1'
      then
        -- dmireset : clear the sticky status (a failure hidden by busy is
        -- reported now)
        if dr_r(16) = '1'
        then
          if fail_pending_r = '1'
          then
            sticky_r     <= DMI_RSP_FAILED;
          else
            sticky_r     <= DMI_RSP_SUCCESS;
          end if;
          fail_pending_r <= '0';
        end if;
        -- dmihardreset : DTM registers back to their reset value, ignore the
        -- result in progress
        if dr_r(17) = '1'
        then
          sticky_r       <= DMI_RSP_SUCCESS;
          fail_pending_r <= '0';
          discard_r      <= v_inflight;
          addr_r         <= (others => '0');
          data_r         <= (others => '0');
        end if;
      end if;

      inflight_r <= v_inflight;
    end if;
  end process;

end architecture rtl;
