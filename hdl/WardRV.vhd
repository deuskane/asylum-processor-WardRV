-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2026-01-24
-- Last update: 2026-01-24
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2026-01-24  0.0      mrosiere Add parameter for sync/async regfile
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
library asylum;
use     asylum.math_pkg.all;
use     asylum.WardRV_pkg.all;

entity WardRV is
  -- =====[ Parameters ]==========================
  generic (
    
     RESET_ADDR        : std_logic_vector := (others => '0');
     IT_ADDR           : std_logic_vector := (others => '0')
     );
  -- =====[ Interfaces ]==========================
  port (
    clk_i             : in  std_logic;
    arst_b_i          : in  std_logic;

    -- Instructions
    ics_o            : out std_logic;
    iaddr_o          : out std_logic_vector(32-1 downto 0);
    idata_i          : in  std_logic_vector(32-1 downto 0);
    
    -- Bus
    sbi_ini_o        : out   sbi_ini_t;
    sbi_tgt_i        : in    sbi_tgt_t;

    -- To/From IT Ctrl
    it_val_i         : in    std_logic;
    it_ack_o         : out   std_logic
    );
end WardRV;
