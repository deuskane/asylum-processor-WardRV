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
-- (IADDR_WIDTH = 30, IADDR_ALIGN_BITS = 2).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-15  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.sbi_pkg.all;

entity WardRV_wrapper is
  generic (
    IADDR_WIDTH      : natural := 30   -- Width of iaddr_o
   ;IADDR_ALIGN_BITS : natural := 2    -- PC LSBs dropped on iaddr_o (2 = word address)
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
  );
end entity WardRV_wrapper;

architecture rtl of WardRV_wrapper is

  signal   sbi_ini   : sbi_ini_t(addr (SBI_ADDR_WIDTH-1 downto 0),
                                 wdata(SBI_DATA_WIDTH-1 downto 0));
  signal   sbi_tgt   : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));

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
  ins_sbi_WardRV_fsm : entity asylum.sbi_WardRV_fsm
    generic map
    (IADDR_WIDTH      => IADDR_WIDTH
    ,IADDR_ALIGN_BITS => IADDR_ALIGN_BITS
    )
    port map
    (clk_i            => clk_i
    ,cke_i            => cke_i
    ,arstn_i          => arst_b_i
    ,ics_o            => ics_o
    ,iaddr_o          => iaddr_o
    ,idata_i          => idata_i
    ,sbi_ini_o        => sbi_ini
    ,sbi_tgt_i        => sbi_tgt
    ,interrupt_i      => interrupt_i
    ,interrupt_ack_o  => interrupt_ack_o
    );

end architecture rtl;
