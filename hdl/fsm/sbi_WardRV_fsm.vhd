-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : sbi_WardRV_fsm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-- This module implements a wrapper for the WardRV_fsm processor, 
-- providing an interface compatible with sbi_OpenBlaze8.
-- It includes the dmem2sbi bridge to translate the 32-bit data interface 
-- into a sequence of 8-bit SBI transactions.
-- Copyright (c) 2026
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;

entity sbi_WardRV_fsm is
  generic (
     -- WardRV specific generics
     HARTID            : std_logic_vector(31 downto 0) := (others => '0');
     RESET_ADDR        : std_logic_vector(31 downto 0) := (others => '0');
     IADDR_WIDTH       : natural := 32;
     IADDR_ALIGN_BITS  : natural := 2
  );
  port (
    clk_i            : in    std_logic;
    cke_i            : in    std_logic;
    arstn_i          : in    std_logic; -- asynchronous reset (active low)

    -- Instructions
    ics_o            : out std_logic;
    iaddr_o          : out std_logic_vector(IADDR_WIDTH-1 downto 0);
    idata_i          : in  std_logic_vector(32-1 downto 0);
    
    -- Bus (SBI)
    sbi_ini_o        : out   sbi_ini_t;
    sbi_tgt_i        : in    sbi_tgt_t;

    -- Interruption
    interrupt_i      : in    std_logic;
    interrupt_ack_o  : out   std_logic
  );
end entity sbi_WardRV_fsm;

architecture rtl of sbi_WardRV_fsm is
  signal imem_ini : imem_ini_t;
  signal imem_tgt : imem_tgt_t;
  signal dmem_ini : dmem_ini_t;
  signal dmem_tgt : dmem_tgt_t;
  
  signal ics_r    : std_logic;
begin

  -- Instruction mapping
  ics_o          <= imem_ini.valid;
  iaddr_o        <= imem_ini.addr(IADDR_WIDTH-1+IADDR_ALIGN_BITS downto IADDR_ALIGN_BITS);

  process(clk_i, arstn_i)
  begin
    if arstn_i = '0' then
      ics_r <= '0';
    elsif rising_edge(clk_i) then
      ics_r <= imem_ini.valid;
    end if;
  end process;

  imem_tgt.inst  <= idata_i;
  imem_tgt.ready <= cke_i and ics_r;
  
  interrupt_ack_o <= '0';

  ins_WardRV_fsm : entity work.WardRV_fsm
    generic map (
      HARTID     => HARTID,
      RESET_ADDR => RESET_ADDR
    )
    port map (
      clk_i      => clk_i,
      arst_b_i   => arstn_i,
      imem_ini_o => imem_ini,
      imem_tgt_i => imem_tgt,
      dmem_ini_o => dmem_ini,
      dmem_tgt_i => dmem_tgt,
      meip_i     => interrupt_i
    );

  ins_dmem2sbi : entity work.dmem2sbi
    port map (
      clk_i      => clk_i,
      arst_b_i   => arstn_i,
      dmem_ini_i => dmem_ini,
      dmem_tgt_o => dmem_tgt,
      sbi_ini_o  => sbi_ini_o,
      sbi_tgt_i  => sbi_tgt_i
    );

end architecture rtl;