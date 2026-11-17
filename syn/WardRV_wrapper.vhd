-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : Asylum
-------------------------------------------------------------------------------
-- File       : WardRV_wrapper.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Wrapper (lint_ghdl, lint_ghdl_synth_*) : sbi_WardRV_fsm with flat and
-- constrained SBI ports (sbi_WardRV_fsm has unconstrained SBI ports and
-- cannot be a synthesis top).
-- Default generics : word address of the whole 32-bit space
-- (IADDR_WIDTH = 30, IADDR_ALIGN_BITS = 2), no debug.
-- DEBUG_ENABLE : WardRV_debug connected as in a SoC (JTAG pins, System Bus
-- Access on its own SBI port through dmem2sbi, ndmreset resets the core).
-- Without it, the debug outputs are '0'.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-15  1.0      mrosiere Created
-- 2026-11-17  1.1      mrosiere Debug unit (DEBUG_ENABLE)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.sbi_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_wrapper is
  generic (
    IADDR_WIDTH         : natural  := 30    -- Width of iaddr_o
   ;IADDR_ALIGN_BITS    : natural  := 2     -- PC LSBs dropped on iaddr_o (2 = word address)
    -- Debug
   ;DEBUG_ENABLE        : boolean  := false
   ;NB_TRIGGERS         : natural  := 0
   ;EBREAK_EXCEPTION    : boolean  := false
   ;RESETHALTREQ_ENABLE : boolean  := false
   ;PROGBUF_SIZE        : natural  := 2
   ;IMPEBREAK           : boolean  := true
   ;DATA_COUNT          : positive := 2
   ;SBA_ENABLE          : boolean  := false
  );
  port (
    -- Clock and Reset
    clk_i            : in  std_logic
   ;cke_i            : in  std_logic
   ;arst_b_i         : in  std_logic
    -- Instructions
   ;ics_o            : out std_logic
   ;iaddr_o          : out std_logic_vector(IADDR_WIDTH-1 downto 0)
   ;idata_i          : in  std_logic_vector(32-1 downto 0)
    -- Bus (SBI)
   ;cs_o             : out std_logic
   ;re_o             : out std_logic
   ;we_o             : out std_logic
   ;addr_o           : out std_logic_vector(SBI_ADDR_WIDTH-1 downto 0)
   ;wdata_o          : out std_logic_vector(SBI_DATA_WIDTH-1 downto 0)
   ;ready_i          : in  std_logic
   ;rdata_i          : in  std_logic_vector(SBI_DATA_WIDTH-1 downto 0)
    -- Interruption
   ;interrupt_i      : in  std_logic
   ;interrupt_ack_o  : out std_logic
    -- JTAG (DEBUG_ENABLE)
   ;tck_i            : in  std_logic
   ;trst_b_i         : in  std_logic
   ;tms_i            : in  std_logic
   ;tdi_i            : in  std_logic
   ;tdo_o            : out std_logic
   ;tdo_en_o         : out std_logic
    -- System Bus Access (DEBUG_ENABLE and SBA_ENABLE)
   ;sba_cs_o         : out std_logic
   ;sba_re_o         : out std_logic
   ;sba_we_o         : out std_logic
   ;sba_addr_o       : out std_logic_vector(SBI_ADDR_WIDTH-1 downto 0)
   ;sba_wdata_o      : out std_logic_vector(SBI_DATA_WIDTH-1 downto 0)
   ;sba_ready_i      : in  std_logic
   ;sba_rdata_i      : in  std_logic_vector(SBI_DATA_WIDTH-1 downto 0)
    -- Reset request of the system (DEBUG_ENABLE)
   ;ndmreset_o       : out std_logic
  );
end entity WardRV_wrapper;

architecture rtl of WardRV_wrapper is

  signal   sbi_ini      : sbi_ini_t(addr (SBI_ADDR_WIDTH-1 downto 0),
                                    wdata(SBI_DATA_WIDTH-1 downto 0));
  signal   sbi_tgt      : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));

  signal   hart_rst_b   : std_logic;
  signal   ndmreset     : std_logic;
  signal   dbg_hart_req : dbg_hart_req_t;
  signal   dbg_mem_ini  : dmem_ini_t;
  signal   dbg_mem_tgt  : dmem_tgt_t;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Bus
  -----------------------------------------------------------------------------
  cs_o               <= sbi_ini.cs;
  re_o               <= sbi_ini.re;
  we_o               <= sbi_ini.we;
  addr_o             <= sbi_ini.addr;
  wdata_o            <= sbi_ini.wdata;
  sbi_tgt.ready      <= ready_i;
  sbi_tgt.rdata      <= rdata_i;
  sbi_tgt.info.name  <= (others => ' ');

  -----------------------------------------------------------------------------
  -- Core
  -----------------------------------------------------------------------------
  hart_rst_b         <= arst_b_i and not ndmreset;
  ndmreset_o         <= ndmreset;

  ins_sbi_WardRV_fsm : entity asylum.sbi_WardRV_fsm
    generic map
    (IADDR_WIDTH         => IADDR_WIDTH
    ,IADDR_ALIGN_BITS    => IADDR_ALIGN_BITS
    ,DEBUG_ENABLE        => DEBUG_ENABLE
    ,NB_TRIGGERS         => NB_TRIGGERS
    ,EBREAK_EXCEPTION    => EBREAK_EXCEPTION
    ,RESETHALTREQ_ENABLE => RESETHALTREQ_ENABLE
    )
    port map
    (clk_i               => clk_i
    ,cke_i               => cke_i
    ,arstn_i             => hart_rst_b
    ,ics_o               => ics_o
    ,iaddr_o             => iaddr_o
    ,idata_i             => idata_i
    ,sbi_ini_o           => sbi_ini
    ,sbi_tgt_i           => sbi_tgt
    ,interrupt_i         => interrupt_i
    ,interrupt_ack_o     => interrupt_ack_o
    ,dbg_hart_req_i      => dbg_hart_req
    ,dbg_mem_ini_o       => dbg_mem_ini
    ,dbg_mem_tgt_i       => dbg_mem_tgt
    );

  -----------------------------------------------------------------------------
  -- Debug unit
  -----------------------------------------------------------------------------
  gen_debug : if DEBUG_ENABLE
  generate
    signal   sba_dmem_ini : dmem_ini_t;
    signal   sba_dmem_tgt : dmem_tgt_t;
    signal   sba_sbi_ini  : sbi_ini_t(addr (SBI_ADDR_WIDTH-1 downto 0),
                                      wdata(SBI_DATA_WIDTH-1 downto 0));
    signal   sba_sbi_tgt  : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));
  begin
    ins_debug : entity asylum.WardRV_debug
      generic map
      (PROGBUF_SIZE        => PROGBUF_SIZE
      ,DATA_COUNT          => DATA_COUNT
      ,IMPEBREAK           => IMPEBREAK
      ,SBA_ENABLE          => SBA_ENABLE
      ,RESETHALTREQ_ENABLE => RESETHALTREQ_ENABLE
      )
      port map
      (jtag_ini_i          => (tck => tck_i, trst_n => trst_b_i, tms => tms_i, tdi => tdi_i)
      ,jtag_tgt_o.tdo      => tdo_o
      ,jtag_tgt_o.tdo_en   => tdo_en_o
      ,clk_i               => clk_i
      ,arst_b_i            => arst_b_i
      ,hart_rst_b_i        => hart_rst_b
      ,dbg_hart_req_o      => dbg_hart_req
      ,dbg_mem_ini_i       => dbg_mem_ini
      ,dbg_mem_tgt_o       => dbg_mem_tgt
      ,ndmreset_o          => ndmreset
      ,sba_dmem_ini_o      => sba_dmem_ini
      ,sba_dmem_tgt_i      => sba_dmem_tgt
      );

    -- System Bus Access on the 8-bit SBI bus of the SoC
    ins_sba_dmem2sbi : entity asylum.dmem2sbi
      port map
      (clk_i               => clk_i
      ,arst_b_i            => arst_b_i
      ,dmem_ini_i          => sba_dmem_ini
      ,dmem_tgt_o          => sba_dmem_tgt
      ,sbi_ini_o           => sba_sbi_ini
      ,sbi_tgt_i           => sba_sbi_tgt
      );

    sba_cs_o               <= sba_sbi_ini.cs;
    sba_re_o               <= sba_sbi_ini.re;
    sba_we_o               <= sba_sbi_ini.we;
    sba_addr_o             <= sba_sbi_ini.addr;
    sba_wdata_o            <= sba_sbi_ini.wdata;
    sba_sbi_tgt.ready      <= sba_ready_i;
    sba_sbi_tgt.rdata      <= sba_rdata_i;
    sba_sbi_tgt.info.name  <= (others => ' ');
  end generate gen_debug;

  gen_debug_b : if not DEBUG_ENABLE
  generate
    ndmreset               <= '0';
    dbg_hart_req           <= DBG_HART_REQ_NULL;
    dbg_mem_tgt            <= DMEM_TGT_NULL;
    tdo_o                  <= '0';
    tdo_en_o               <= '0';
    sba_cs_o               <= '0';
    sba_re_o               <= '0';
    sba_we_o               <= '0';
    sba_addr_o             <= (others => '0');
    sba_wdata_o            <= (others => '0');
  end generate gen_debug_b;

end architecture rtl;
