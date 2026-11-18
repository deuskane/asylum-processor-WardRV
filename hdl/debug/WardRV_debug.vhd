-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_debug.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- WardRV debug unit (RISC-V Debug Spec 1.0, figure 2.1) : the part outside
-- the core, to connect to WardRV_fsm / sbi_WardRV_fsm with DEBUG_ENABLE.
--   WardRV_dbg_jtag : JTAG TAP + DTM + DMI CDC (TCK domain -> clk domain)
--   WardRV_dbg_dm   : Debug Module (clk domain, shared with the core)
-- arst_b_i     : power-on reset of the debug unit, never ndmreset_o
-- hart_rst_b_i : reset of the core (havereset, unavail)
-- ndmreset_o   : reset of the core and of the system, not of the debug unit
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-11-17  2.0      mrosiere Rewrite : JTAG transport + Debug Module
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_debug is
  generic (
    IDCODE_VALUE            : std_logic_vector(32-1 downto 0) := x"10000001"
   ;TRST_ENABLE             : boolean                         := true  -- Use TRST*
   ;DTM_IDLE_CYCLES         : natural                         := 5     -- dtmcs.idle
   ;PROGBUF_SIZE            : natural                         := 2     -- Program buffer (0..16 words)
   ;DATA_COUNT              : positive                        := 2     -- Data registers (1..12)
   ;SBA_ENABLE              : boolean                         := false -- System Bus Access
   ;RESETHALTREQ_ENABLE     : boolean                         := false -- Halt on reset request
   ;AARPOSTINCREMENT_ENABLE : boolean                         := false -- Access Register : aarpostincrement
  );
  port (
    -- JTAG (TCK domain)
    jtag_ini_i              : in  jtag_ini_t
   ;jtag_tgt_o              : out jtag_tgt_t
    -- Clock and Reset
   ;clk_i                   : in  std_logic
   ;arst_b_i                : in  std_logic
    -- Hart
   ;hart_rst_b_i            : in  std_logic
   ;dbg_hart_req_o          : out dbg_hart_req_t
   ;dbg_mem_ini_i           : in  dmem_ini_t
   ;dbg_mem_tgt_o           : out dmem_tgt_t
    -- System
   ;ndmreset_o              : out std_logic
   ;sba_dmem_ini_o          : out dmem_ini_t
   ;sba_dmem_tgt_i          : in  dmem_tgt_t := DMEM_TGT_NULL
  );
end entity WardRV_debug;

architecture rtl of WardRV_debug is

  signal   dmi_req      : dmi_req_t;
  signal   dmi_rdata    : std_logic_vector(32-1 downto 0);

begin  -- architecture rtl

  ins_jtag : entity work.WardRV_dbg_jtag
    generic map
    (IDCODE_VALUE        => IDCODE_VALUE
    ,TRST_ENABLE         => TRST_ENABLE
    ,DTM_IDLE_CYCLES     => DTM_IDLE_CYCLES
    )
    port map
    (jtag_ini_i          => jtag_ini_i
    ,jtag_tgt_o          => jtag_tgt_o
    ,clk_i               => clk_i
    ,arst_b_i            => arst_b_i
    ,dmi_req_o           => dmi_req
    ,dmi_rdata_i         => dmi_rdata
    );

  ins_dm : entity work.WardRV_dbg_dm
    generic map
    (PROGBUF_SIZE            => PROGBUF_SIZE
    ,DATA_COUNT              => DATA_COUNT
    ,SBA_ENABLE              => SBA_ENABLE
    ,RESETHALTREQ_ENABLE     => RESETHALTREQ_ENABLE
    ,AARPOSTINCREMENT_ENABLE => AARPOSTINCREMENT_ENABLE
    )
    port map
    (clk_i                   => clk_i
    ,arst_b_i                => arst_b_i
    ,dmi_req_i               => dmi_req
    ,dmi_rdata_o             => dmi_rdata
    ,hart_rst_b_i            => hart_rst_b_i
    ,dbg_hart_req_o          => dbg_hart_req_o
    ,dbg_mem_ini_i           => dbg_mem_ini_i
    ,dbg_mem_tgt_o           => dbg_mem_tgt_o
    ,ndmreset_o              => ndmreset_o
    ,sba_dmem_ini_o          => sba_dmem_ini_o
    ,sba_dmem_tgt_i          => sba_dmem_tgt_i
    );

end architecture rtl;
