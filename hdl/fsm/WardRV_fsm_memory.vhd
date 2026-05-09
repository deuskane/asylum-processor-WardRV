-------------------------------------------------------------------------------
-- Title      : WardRV Memory Unit
-- Project    : WardRV
-------------------------------------------------------------------------------
-- Description: 
-- This module handles the Data Memory (MEM) stage.
-- It manages the alignment of memory accesses (Byte, Half-word, Word) for both
-- read and write operations, and handles sign extension for load instructions.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-04-06  1.0      mrosiere Created
-------------------------------------------------------------------------------
library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;

entity WardRV_fsm_memory is
  port (
    clk_i               : in  std_logic;
    arst_b_i            : in  std_logic;

    -- Control signals from FSM/Decoder
    dmem_valid_i        : in  std_logic;
    addr_i              : in  std_logic_vector(31 downto 0);
    wdata_i             : in  std_logic_vector(31 downto 0);
    we_i                : in  std_logic;
    be_i                : in  std_logic_vector(3 downto 0);
    data_unsigned_i     : in  std_logic;

    -- Status/Data back to FSM and Register File
    dmem_ready_o        : out std_logic;
    dmem_rdata_r_o      : out std_logic_vector(31 downto 0); -- Registered data for Writeback

    -- Physical Data Memory Interface (SBI Bus)
    dmem_ini_o           : out dmem_ini_t;
    dmem_tgt_i           : in  dmem_tgt_t
  );
end entity WardRV_fsm_memory;

architecture behavioural of WardRV_fsm_memory is
  signal dmem_valid_r       : std_logic;
  signal dmem_be_aligned    : std_logic_vector(3 downto 0);
  signal dmem_wdata_aligned : std_logic_vector(31 downto 0);
  signal dmem_rdata_aligned : std_logic_vector(31 downto 0);
begin

  -- Memory request valid signal (introduces a one-cycle delay for the bus handshake)
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      dmem_valid_r <= '0';
    elsif rising_edge(clk_i) then
      dmem_valid_r <= dmem_valid_i;
    end if;
  end process;

  -- Alignment logic: Calculate Byte Enables and shift Write Data based on address LSBs
  -- Junior note: In RISC-V, sub-word accesses must be shifted to their correct byte lane.
  dmem_be_aligned     <= std_logic_vector(shift_left(unsigned(be_i   ), to_integer(unsigned(addr_i(1 downto 0)))));
  dmem_wdata_aligned  <= std_logic_vector(shift_left(unsigned(wdata_i), to_integer(unsigned(addr_i(1 downto 0))) * 8));

  -- SBI Output Assignment
  dmem_ini_o.valid <= dmem_valid_r      ;
  dmem_ini_o.addr  <= addr_i            ;
  dmem_ini_o.wdata <= dmem_wdata_aligned;
  dmem_ini_o.we    <= we_i              ;
  dmem_ini_o.be    <= dmem_be_aligned   ;

  dmem_ready_o    <= dmem_tgt_i.ready;

  --------------------------------------------------------------------
  -- Load Data Formatting (Combinatorial)
  -- Extract the correct portion of the 32b word and apply sign/zero extension.
  --------------------------------------------------------------------
  process(all)
    variable v_shamt : integer;
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    v_shamt := to_integer(unsigned(addr_i(1 downto 0))) * 8;
    v_rdata := std_logic_vector(shift_right(unsigned(dmem_tgt_i.rdata), v_shamt));

    case be_i is
      when "0001"  => -- Byte access
        dmem_rdata_aligned <= std_logic_vector(resize(signed(v_rdata(7 downto 0)), 32)) when data_unsigned_i = '0' else 
                           std_logic_vector(resize(unsigned(v_rdata(7 downto 0)), 32));
      when "0011"  => -- Half-word access
        dmem_rdata_aligned <= std_logic_vector(resize(signed(v_rdata(15 downto 0)), 32)) when data_unsigned_i = '0' else 
                           std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
      when others  => -- Word access
        dmem_rdata_aligned <= v_rdata;
    end case;
  end process;

  -- Latch the formatted read data for the Writeback stage
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      dmem_rdata_r_o <= (others => '0');
    elsif rising_edge(clk_i) then
      if dmem_valid_r = '1' and dmem_tgt_i.ready = '1' then
        dmem_rdata_r_o <= dmem_rdata_aligned;
      end if;
    end if; 
  end process;

end architecture behavioural;