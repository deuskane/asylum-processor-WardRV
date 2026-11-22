-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_jtag.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- JTAG Debug Transport (Debug Spec 1.0 chapter 6) : JTAG pins in (TCK
-- domain), DMI requests out (clk domain).
--   jtag_tap           : IEEE 1149.1 TAP, IDCODE, BYPASS (asylum-debug-jtag),
--                        DTMCS and DMI decoded here (user_tdo_en)
--   WardRV_dbg_dtm     : dtmcs and dmi registers
--   WardRV_dbg_dmi_cdc : TCK -> clk handshake
-- arst_b_i is the power-on reset of the debug unit (never ndmreset).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-- 2026-11-22  1.1      mrosiere TAP : jtag_tap of asylum-debug-jtag
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_jtag is
  generic (
    IDCODE_VALUE    : std_logic_vector(32-1 downto 0) := x"10000001"
   ;TRST_ENABLE     : boolean                         := true     -- Use TRST*
   ;DTM_IDLE_CYCLES : natural                         := 5        -- dtmcs.idle
  );
  port (
    -- JTAG (TCK domain)
    jtag_ini_i      : in  jtag_ini_t
   ;jtag_tgt_o      : out jtag_tgt_t
    -- Clock and Reset
   ;clk_i           : in  std_logic
   ;arst_b_i        : in  std_logic
    -- DMI (clk domain)
   ;dmi_req_o       : out dmi_req_t
   ;dmi_rdata_i     : in  std_logic_vector(32-1 downto 0)
  );
end entity WardRV_dbg_jtag;

architecture rtl of WardRV_dbg_jtag is

  signal   tck          : std_logic;
  signal   trst_b       : std_logic;
  signal   tlr          : std_logic;
  signal   ir           : std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
  signal   capture_dr   : std_logic;
  signal   shift_dr     : std_logic;
  signal   update_dr    : std_logic;
  signal   tdi          : std_logic;
  signal   user_tdo_en  : std_logic;
  signal   user_tdo     : std_logic;

  signal   tck_dmi_req  : dmi_req_t;
  signal   tck_busy     : std_logic;
  signal   tck_rdata    : std_logic_vector(32-1 downto 0);

begin  -- architecture rtl

  ins_tap : jtag_tap
    generic map
    (IR_WIDTH        => JTAG_IR_WIDTH
    ,IDCODE_VALUE    => IDCODE_VALUE
    ,TRST_ENABLE     => TRST_ENABLE
    )
    port map
    (arst_b_i        => arst_b_i
    ,jtag_ini_i      => jtag_ini_i
    ,jtag_tgt_o      => jtag_tgt_o
    ,tck_o           => tck
    ,trst_b_o        => trst_b
    ,tlr_o           => tlr
    ,ir_o            => ir
    ,capture_dr_o    => capture_dr
    ,shift_dr_o      => shift_dr
    ,update_dr_o     => update_dr
    ,tdi_o           => tdi
    ,user_tdo_en_i   => user_tdo_en
    ,user_tdo_i      => user_tdo
    );

  -- User instructions : DTMCS and DMI select the TDO of WardRV_dbg_dtm,
  -- every other opcode but IDCODE is BYPASS
  user_tdo_en <= '1' when ir = JTAG_IR_DTMCS or ir = JTAG_IR_DMI else
                 '0';

  ins_dtm : entity work.WardRV_dbg_dtm
    generic map
    (IDLE_CYCLES     => DTM_IDLE_CYCLES
    )
    port map
    (tck_i           => tck
    ,trst_b_i        => trst_b
    ,tlr_i           => tlr
    ,ir_i            => ir
    ,capture_dr_i    => capture_dr
    ,shift_dr_i      => shift_dr
    ,update_dr_i     => update_dr
    ,tdi_i           => tdi
    ,tdo_o           => user_tdo
    ,dmi_req_o       => tck_dmi_req
    ,dmi_busy_i      => tck_busy
    ,dmi_rdata_i     => tck_rdata
    );

  ins_dmi_cdc : entity work.WardRV_dbg_dmi_cdc
    port map
    (tck_i           => tck
    ,tck_arst_b_i    => arst_b_i
    ,tck_req_i       => tck_dmi_req
    ,tck_busy_o      => tck_busy
    ,tck_rdata_o     => tck_rdata
    ,clk_i           => clk_i
    ,arst_b_i        => arst_b_i
    ,dmi_req_o       => dmi_req_o
    ,dmi_rdata_i     => dmi_rdata_i
    );

end architecture rtl;
