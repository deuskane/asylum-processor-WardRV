-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dmi_cdc.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Clock domain crossing of the DMI (Debug Spec 1.0 chapter 3.1) between the
-- DTM (TCK domain) and the Debug Module (clk domain) : 4-phase req / ack
-- handshake, req and ack go through sync2dffrn (asylum:target:techmap).
--  1. TCK : a request (valid) loads the payload registers and raises req
--  2. clk : req seen -> one-cycle DMI request with the payload (stable while
--           req = 1), the read data is registered, ack rises
--  3. TCK : ack seen -> the read data (stable while ack = 1) is registered,
--           req falls
--  4. clk : req low -> ack falls. The TCK side is busy until ack is low.
-- The handshake moves only while TCK runs : the debugger waits dtmcs.idle
-- cycles in Run-Test/Idle. The TCK side is reset by the power-on reset only
-- (not TRST*), so a request in progress always completes.
-- The payload paths are quasi-static (max delay / false path constraints).
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
use     asylum.WardRV_debug_pkg.all;
use     asylum.techmap_pkg.all;

entity WardRV_dbg_dmi_cdc is
  port (
    -- TCK domain (falling edge)
    tck_i          : in  std_logic
   ;tck_arst_b_i   : in  std_logic
   ;tck_req_i      : in  dmi_req_t                                -- valid : new request
   ;tck_busy_o     : out std_logic                                -- Request in progress
   ;tck_rdata_o    : out std_logic_vector(32-1 downto 0)          -- Read data of the last request
    -- clk domain
   ;clk_i          : in  std_logic
   ;arst_b_i       : in  std_logic
   ;dmi_req_o      : out dmi_req_t                                -- valid : one cycle
   ;dmi_rdata_i    : in  std_logic_vector(32-1 downto 0)          -- Read data, same cycle
  );
end entity WardRV_dbg_dmi_cdc;

architecture rtl of WardRV_dbg_dmi_cdc is

  -- TCK domain
  signal   req_r       : std_logic;
  signal   payload_r   : dmi_req_t;
  signal   ack_sync    : std_logic;
  signal   tck_rdata_r : std_logic_vector(32-1 downto 0);

  -- clk domain
  signal   req_sync    : std_logic;
  signal   ack_r       : std_logic;
  signal   clk_rdata_r : std_logic_vector(32-1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- TCK domain
  -----------------------------------------------------------------------------
  ins_ack_sync : sync2dffrn
    port map
    (clk_i       => tck_i
    ,arst_b_i    => tck_arst_b_i
    ,d_i         => ack_r
    ,q_o         => ack_sync
    );

  process(tck_i, tck_arst_b_i)
  begin
    if tck_arst_b_i = '0'
    then
      req_r       <= '0';
      payload_r   <= (valid => '0', we => '0', addr => (others => '0'), wdata => (others => '0'));
      tck_rdata_r <= (others => '0');
    elsif falling_edge(tck_i)
    then
      if req_r = '0' and ack_sync = '0'
      then
        -- 1. Idle : new request
        if tck_req_i.valid = '1'
        then
          payload_r   <= tck_req_i;
          req_r       <= '1';
        end if;
      elsif req_r = '1' and ack_sync = '1'
      then
        -- 3. Response
        tck_rdata_r <= clk_rdata_r;
        req_r       <= '0';
      end if;
    end if;
  end process;

  tck_busy_o  <= req_r or ack_sync;
  tck_rdata_o <= tck_rdata_r;

  -----------------------------------------------------------------------------
  -- clk domain
  -----------------------------------------------------------------------------
  ins_req_sync : sync2dffrn
    port map
    (clk_i       => clk_i
    ,arst_b_i    => arst_b_i
    ,d_i         => req_r
    ,q_o         => req_sync
    );

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0'
    then
      ack_r       <= '0';
      clk_rdata_r <= (others => '0');
    elsif rising_edge(clk_i)
    then
      if req_sync = '1' and ack_r = '0'
      then
        -- 2. Request
        clk_rdata_r <= dmi_rdata_i;
        ack_r       <= '1';
      elsif req_sync = '0'
      then
        -- 4. End of the handshake
        ack_r       <= '0';
      end if;
    end if;
  end process;

  dmi_req_o.valid <= req_sync and not ack_r;
  dmi_req_o.we    <= payload_r.we;
  dmi_req_o.addr  <= payload_r.addr;
  dmi_req_o.wdata <= payload_r.wdata;

end architecture rtl;
