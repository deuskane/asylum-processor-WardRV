-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_wrapper.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Wrapper (lint_ghdl, lint_ghdl_synth_*) : sbi_WardRV_fsm + WardRV_debug
-- connected as in a SoC, with flat and constrained ports (sbi_WardRV_fsm has
-- unconstrained SBI ports and cannot be a synthesis top).
-- The hart is reset by arst_b_i and by ndmreset (illustration only : a SoC
-- uses its reset controller).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-10  1.0      mrosiere Created
-- 2026-10-24  1.1      mrosiere Was WardRV_synth_top, library asylum
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library asylum;
use asylum.sbi_pkg.all;
use asylum.WardRV_pkg.all;
use asylum.WardRV_debug_pkg.all;

entity WardRV_wrapper is
  generic (
    DEBUG_ENABLE        : boolean := true;
    NB_TRIGGERS         : natural := 2;
    EBREAK_EXCEPTION    : boolean := false;
    RESETHALTREQ_ENABLE : boolean := false;
    SBA_ENABLE          : boolean := false;
    PROGBUF_SIZE        : natural := 2;
    DATA_COUNT          : natural := 2
  );
  port (
    clk_i         : in  std_logic;
    arst_b_i      : in  std_logic;

    -- Instructions (default generics of sbi_WardRV_fsm : byte address)
    ics_o         : out std_logic;
    iaddr_o       : out std_logic_vector(31 downto 0);
    idata_i       : in  std_logic_vector(31 downto 0);

    -- Bus (SBI)
    cs_o          : out std_logic;
    re_o          : out std_logic;
    we_o          : out std_logic;
    addr_o        : out std_logic_vector(SBI_ADDR_WIDTH-1 downto 0);
    wdata_o       : out std_logic_vector(SBI_DATA_WIDTH-1 downto 0);
    ready_i       : in  std_logic;
    rdata_i       : in  std_logic_vector(SBI_DATA_WIDTH-1 downto 0);

    -- Interruption
    interrupt_i   : in  std_logic;

    -- JTAG
    tck_i         : in  std_logic;
    trst_n_i      : in  std_logic;
    tms_i         : in  std_logic;
    tdi_i         : in  std_logic;
    tdo_o         : out std_logic;
    tdo_en_o      : out std_logic;

    -- System Bus Access (SBA_ENABLE)
    sba_cs_o      : out std_logic;
    sba_re_o      : out std_logic;
    sba_we_o      : out std_logic;
    sba_addr_o    : out std_logic_vector(SBI_ADDR_WIDTH-1 downto 0);
    sba_wdata_o   : out std_logic_vector(SBI_DATA_WIDTH-1 downto 0);
    sba_ready_i   : in  std_logic;
    sba_rdata_i   : in  std_logic_vector(SBI_DATA_WIDTH-1 downto 0);

    -- Non-debug module reset request
    ndmreset_o    : out std_logic
  );
end entity WardRV_wrapper;

architecture rtl of WardRV_wrapper is

  signal hart_rst_b    : std_logic;
  signal ndmreset      : std_logic;

  signal sbi_ini       : sbi_ini_t(addr (SBI_ADDR_WIDTH-1 downto 0),
                                   wdata(SBI_DATA_WIDTH-1 downto 0));
  signal sbi_tgt       : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));

  signal dbg_hart_req  : dbg_hart_req_t;
  signal dbg_sbi_ini   : dbg_sbi_ini_t;
  signal dbg_sbi_tgt   : dbg_sbi_tgt_t;

  signal jtag_ini      : jtag_ini_t;
  signal jtag_tgt      : jtag_tgt_t;

  signal sba_sbi_ini   : sbi_ini_t(addr (SBI_ADDR_WIDTH-1 downto 0),
                                   wdata(SBI_DATA_WIDTH-1 downto 0));
  signal sba_sbi_tgt   : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));

begin

  hart_rst_b        <= arst_b_i and not ndmreset;
  ndmreset_o        <= ndmreset;

  -- Bus
  cs_o              <= sbi_ini.cs;
  re_o              <= sbi_ini.re;
  we_o              <= sbi_ini.we;
  addr_o            <= sbi_ini.addr;
  wdata_o           <= sbi_ini.wdata;
  sbi_tgt.ready     <= ready_i;
  sbi_tgt.rdata     <= rdata_i;
  sbi_tgt.info      <= (name => (others => ' '));

  -- Core
  ins_core : entity asylum.sbi_WardRV_fsm
    generic map (
      DEBUG_ENABLE        => DEBUG_ENABLE,
      NB_TRIGGERS         => NB_TRIGGERS,
      EBREAK_EXCEPTION    => EBREAK_EXCEPTION,
      RESETHALTREQ_ENABLE => RESETHALTREQ_ENABLE
    )
    port map (
      clk_i           => clk_i,
      cke_i           => '1',
      arstn_i         => hart_rst_b,
      ics_o           => ics_o,
      iaddr_o         => iaddr_o,
      idata_i         => idata_i,
      sbi_ini_o       => sbi_ini,
      sbi_tgt_i       => sbi_tgt,
      interrupt_i     => interrupt_i,
      interrupt_ack_o => open,
      dbg_hart_req_i  => dbg_hart_req,
      dbg_sbi_ini_o   => dbg_sbi_ini,
      dbg_sbi_tgt_i   => dbg_sbi_tgt
    );

  -- Debug unit
  gen_debug : if DEBUG_ENABLE generate
    jtag_ini.tck      <= tck_i;
    jtag_ini.trst_n   <= trst_n_i;
    jtag_ini.tms      <= tms_i;
    jtag_ini.tdi      <= tdi_i;
    tdo_o             <= jtag_tgt.tdo;
    tdo_en_o          <= jtag_tgt.tdo_en;

    sba_cs_o          <= sba_sbi_ini.cs;
    sba_re_o          <= sba_sbi_ini.re;
    sba_we_o          <= sba_sbi_ini.we;
    sba_addr_o        <= sba_sbi_ini.addr;
    sba_wdata_o       <= sba_sbi_ini.wdata;
    sba_sbi_tgt.ready <= sba_ready_i;
    sba_sbi_tgt.rdata <= sba_rdata_i;
    sba_sbi_tgt.info  <= (name => (others => ' '));

    ins_debug : entity asylum.WardRV_debug
      generic map (
        PROGBUF_SIZE        => PROGBUF_SIZE,
        DATA_COUNT          => DATA_COUNT,
        SBA_ENABLE          => SBA_ENABLE,
        SBA_ADDR_WIDTH      => SBI_ADDR_WIDTH,
        RESETHALTREQ_ENABLE => RESETHALTREQ_ENABLE
      )
      port map (
        jtag_ini_i     => jtag_ini,
        jtag_tgt_o     => jtag_tgt,
        clk_i          => clk_i,
        arst_b_i       => arst_b_i,
        hart_rst_b_i   => hart_rst_b,
        dbg_hart_req_o => dbg_hart_req,
        dbg_sbi_ini_i  => dbg_sbi_ini,
        dbg_sbi_tgt_o  => dbg_sbi_tgt,
        ndmreset_o     => ndmreset,
        sba_sbi_ini_o  => sba_sbi_ini,
        sba_sbi_tgt_i  => sba_sbi_tgt
      );
  end generate gen_debug;

  gen_no_debug : if not DEBUG_ENABLE generate
    dbg_hart_req      <= DBG_HART_REQ_NULL;
    dbg_sbi_tgt       <= DBG_SBI_TGT_NULL;
    ndmreset          <= '0';
    tdo_o             <= '0';
    tdo_en_o          <= '0';
    sba_cs_o          <= '0';
    sba_re_o          <= '0';
    sba_we_o          <= '0';
    sba_addr_o        <= (others => '0');
    sba_wdata_o       <= (others => '0');
  end generate gen_no_debug;

end architecture rtl;
