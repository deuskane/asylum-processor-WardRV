-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_tiny.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;

entity WardRV_tiny is
  generic (
    RESET_ADDR : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    -- Instruction Interface
    inst_ini_o : out inst_ini_t;
    inst_tgt_i : in  inst_tgt_t;

    -- Data Interface
    sbi_ini_o  : out sbi_ini_t;
    sbi_tgt_i  : in  sbi_tgt_t
  );
end entity WardRV_tiny;

architecture rtl of WardRV_tiny is

begin

  -- Placeholder for RTL implementation
  inst_ini_o.valid <= '0';
  inst_ini_o.addr  <= (others => '0');
  
  sbi_ini_o.valid  <= '0';
  sbi_ini_o.addr   <= (others => '0');
  sbi_ini_o.wdata  <= (others => '0');
  sbi_ini_o.we     <= '0';
  sbi_ini_o.be     <= (others => '0');

end architecture rtl;