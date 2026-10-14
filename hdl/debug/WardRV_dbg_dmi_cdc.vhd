-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dmi_cdc.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Clock domain crossing of the DMI between the DTM (TCK domain) and the
-- Debug Module (clk domain). Isolated block, reusable by any other DTM.
-- 4-phase req/ack handshake through sync2dffrn (asylum:target:techmap) :
--  * TCK side (falling edge of TCK) : a start strobe (tck_req_i.valid) loads
--    the request payload and raises req. The payload registers are stable
--    while req=1. When ack is seen, the response (stable while ack=1) is
--    latched, tck_rsp_o.valid pulses for one TCK cycle and req falls. A new
--    request is accepted once ack has fallen (tck_busy_o=0).
--  * clk side : on req synchronized -> dmi_req_o.valid until dmi_rsp_i.ready,
--    then wait dmi_rsp_i.valid, latch the result and raise ack. ack falls
--    when req synchronized falls.
-- The handshake only progresses while TCK is running : this is why the
-- debugger keeps the TAP in Run-Test/Idle for dtmcs.idle cycles.
-- trst_b_i must not be asserted alone during a transaction (the clk side
-- would finish it, and a new request could take its response) :
-- WardRV_dbg_jtag connects the power-on reset only, not TRST*.
-- Timing : the payload paths (TCK registers -> clk logic, clk registers ->
-- TCK logic) are quasi-static (multi-cycle / max delay constraints).
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
use     asylum.WardRV_debug_pkg.all;
use     asylum.techmap_pkg.all;

entity WardRV_dbg_dmi_cdc is
  port (
    -- TCK side
    tck_i        : in  std_logic;
    trst_b_i     : in  std_logic;
    tck_req_i    : in  dmi_req_t;   -- valid : start strobe (falling edge of TCK)
    tck_rsp_o    : out dmi_rsp_t;   -- valid : response pulse (one TCK cycle)
    tck_busy_o   : out std_logic;   -- Transaction in progress, start ignored

    -- clk side
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    dmi_req_o    : out dmi_req_t;
    dmi_rsp_i    : in  dmi_rsp_t
  );
end entity WardRV_dbg_dmi_cdc;

architecture rtl of WardRV_dbg_dmi_cdc is

  -- TCK side
  type tck_state_t is (T_IDLE, T_REQ, T_ACK);
  signal tck_state_r  : tck_state_t;
  signal req_r        : std_logic;
  signal req_op_r     : std_logic_vector(1 downto 0);
  signal req_addr_r   : std_logic_vector(DMI_ABITS-1 downto 0);
  signal req_data_r   : std_logic_vector(31 downto 0);
  signal ack_sync     : std_logic;
  signal rsp_valid_r  : std_logic;
  signal rsp_op_r     : std_logic_vector(1 downto 0);
  signal rsp_data_r   : std_logic_vector(31 downto 0);

  -- clk side
  type clk_state_t is (C_IDLE, C_REQ, C_WAIT, C_ACK);
  signal clk_state_r  : clk_state_t;
  signal req_sync     : std_logic;
  signal ack_r        : std_logic;
  signal res_op_r     : std_logic_vector(1 downto 0);
  signal res_data_r   : std_logic_vector(31 downto 0);

begin

  --------------------------------------------------------------------
  -- TCK side
  --------------------------------------------------------------------
  ins_ack_sync : sync2dffrn
    port map (
      clk_i    => tck_i,
      arst_b_i => trst_b_i,
      d_i      => ack_r,
      q_o      => ack_sync
    );

  process(tck_i, trst_b_i)
  begin
    if trst_b_i = '0'
    then
      tck_state_r <= T_IDLE;
      req_r       <= '0';
      req_op_r    <= DMI_OP_NOP;
      req_addr_r  <= (others => '0');
      req_data_r  <= (others => '0');
      rsp_valid_r <= '0';
      rsp_op_r    <= DMI_RSP_SUCCESS;
      rsp_data_r  <= (others => '0');
    elsif falling_edge(tck_i)
    then
      rsp_valid_r <= '0';

      case tck_state_r is
        when T_IDLE =>
          if tck_req_i.valid = '1' and ack_sync = '0'
          then
            req_op_r    <= tck_req_i.op;
            req_addr_r  <= tck_req_i.addr;
            req_data_r  <= tck_req_i.data;
            req_r       <= '1';
            tck_state_r <= T_REQ;
          end if;

        when T_REQ =>
          if ack_sync = '1'
          then
            -- clk side result is stable while ack = 1
            rsp_op_r    <= res_op_r;
            rsp_data_r  <= res_data_r;
            rsp_valid_r <= '1';
            req_r       <= '0';
            tck_state_r <= T_ACK;
          end if;

        when T_ACK =>
          if ack_sync = '0'
          then
            tck_state_r <= T_IDLE;
          end if;
      end case;
    end if;
  end process;

  tck_busy_o      <= '0' when tck_state_r = T_IDLE and ack_sync = '0' else '1';

  tck_rsp_o.ready <= '1' when tck_state_r = T_IDLE and ack_sync = '0' else '0';
  tck_rsp_o.valid <= rsp_valid_r;
  tck_rsp_o.op    <= rsp_op_r;
  tck_rsp_o.data  <= rsp_data_r;

  --------------------------------------------------------------------
  -- clk side
  --------------------------------------------------------------------
  ins_req_sync : sync2dffrn
    port map (
      clk_i    => clk_i,
      arst_b_i => arst_b_i,
      d_i      => req_r,
      q_o      => req_sync
    );

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0'
    then
      clk_state_r <= C_IDLE;
      ack_r       <= '0';
      res_op_r    <= DMI_RSP_SUCCESS;
      res_data_r  <= (others => '0');
    elsif rising_edge(clk_i)
    then
      case clk_state_r is
        when C_IDLE =>
          if req_sync = '1'
          then
            clk_state_r <= C_REQ;
          end if;

        when C_REQ =>
          if dmi_rsp_i.ready = '1'
          then
            if dmi_rsp_i.valid = '1'
            then
              res_op_r    <= dmi_rsp_i.op;
              res_data_r  <= dmi_rsp_i.data;
              ack_r       <= '1';
              clk_state_r <= C_ACK;
            else
              clk_state_r <= C_WAIT;
            end if;
          end if;

        when C_WAIT =>
          if dmi_rsp_i.valid = '1'
          then
            res_op_r    <= dmi_rsp_i.op;
            res_data_r  <= dmi_rsp_i.data;
            ack_r       <= '1';
            clk_state_r <= C_ACK;
          end if;

        when C_ACK =>
          if req_sync = '0'
          then
            ack_r       <= '0';
            clk_state_r <= C_IDLE;
          end if;
      end case;
    end if;
  end process;

  -- Payload : TCK registers, stable while req = 1
  dmi_req_o.valid <= '1' when clk_state_r = C_REQ else '0';
  dmi_req_o.op    <= req_op_r;
  dmi_req_o.addr  <= req_addr_r;
  dmi_req_o.data  <= req_data_r;

end architecture rtl;
