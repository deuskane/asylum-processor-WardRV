-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : dmem2sbi.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-- This module implements a bridge between the internal dmem interface and 
-- the Simple Bus Interface (SBI). 
-- Since SBI is 8-bit wide and lacks byte enables, this bridge sequences 
-- 32-bit dmem requests into up to four 8-bit SBI transactions.
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-05-09  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;

entity dmem2sbi is
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    -- dmem interface (Master side)
    dmem_ini_i : in  dmem_ini_t;
    dmem_tgt_o : out dmem_tgt_t;

    -- sbi interface (Slave side)
    sbi_ini_o  : out sbi_ini_t;
    sbi_tgt_i  : in  sbi_tgt_t 
  );
end entity dmem2sbi;

architecture behavioural of dmem2sbi is

  -- SBI Address width
  constant SBI_ADDR_WIDTH : integer := sbi_ini_o.addr'length;
  
  -- State machine to sequence 32-bit accesses into four 8-bit SBI transactions
  type state_t is (S_IDLE,      -- Wait for dmem request
                   S_TRANSFER,  -- Perform SBI byte-accesses
                   S_DONE);     -- Signal completion to dmem
  
  signal state_r      : state_t;
  signal state_r_next : state_t;

  -- Registers to hold the dmem request parameters
  -- Internal registers to buffer the 32-bit request and accumulate response
  signal addr_r       : std_logic_vector(31 downto 0);
  signal addr_r_word  : std_logic_vector(31 downto 0);

  signal wdata_r      : std_logic_vector(31 downto 0);
  signal rdata_r      : std_logic_vector(31 downto 0);
  signal be_r         : std_logic_vector(3  downto 0);
  signal we_r         : std_logic;
  
  -- Counter to sequence through the 4 bytes
  signal byte_cnt_r      : unsigned(1 downto 0);
  signal byte_cnt_r_next : unsigned(1 downto 0);

begin

  --------------------------------------------------------------------
  -- Control Path
  --------------------------------------------------------------------
  process(all)
  begin
    state_r_next    <= state_r;
    byte_cnt_r_next <= byte_cnt_r;

    case state_r is
      -- Wait for valid request from the processor
      when S_IDLE =>
        byte_cnt_r_next <= (others => '0');
        if dmem_ini_i.valid = '1' 
        then
          state_r_next <= S_TRANSFER;
        end if;

      -- Iterate through all 4 bytes of the 32-bit word
      when S_TRANSFER =>
        -- If current byte is not enabled, skip it
        -- Optimization: Skip the current SBI cycle if the byte enable (BE) bit is not set
        if be_r(to_integer(byte_cnt_r)) = '0'
        then
          if byte_cnt_r = 3
          then
            state_r_next <= S_DONE;
          else
            byte_cnt_r_next <= byte_cnt_r + 1;
          end if;
        -- Otherwise wait for SBI handshake
        -- Otherwise, execute 8-bit transaction and wait for SBI target to be ready
        elsif sbi_tgt_i.ready = '1'
        then
          if byte_cnt_r = 3
          then
            state_r_next <= S_DONE;
          else
            byte_cnt_r_next <= byte_cnt_r + 1;
          end if;
        end if;

      -- Single cycle state to assert dmem_ready
      when S_DONE =>
        state_r_next <= S_IDLE;

      when others =>
        state_r_next <= S_IDLE;
    end case;
  end process;

  --------------------------------------------------------------------
  -- Datapath / Registers
  --------------------------------------------------------------------
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0'
    then
      state_r    <= S_IDLE;
      byte_cnt_r <= (others => '0');
      addr_r     <= (others => '0');
      wdata_r    <= (others => '0');
      rdata_r    <= (others => '0');
      be_r       <= (others => '0');
      we_r       <= '0';
    elsif rising_edge(clk_i)
    then
      state_r    <= state_r_next;
      byte_cnt_r <= byte_cnt_r_next;

      -- Capture request at the start
      -- Capture the 32-bit request parameters into local registers
      if state_r = S_IDLE and dmem_ini_i.valid = '1'
      then
        addr_r  <= dmem_ini_i.addr;
        wdata_r <= dmem_ini_i.wdata;
        be_r    <= dmem_ini_i.be;
        we_r    <= dmem_ini_i.we;
      end if;

      -- Accumulate read data from SBI
      -- For Read transactions, sample the 8-bit SBI data into the correct byte lane
      if state_r = S_TRANSFER and sbi_tgt_i.ready = '1' and we_r = '0'
      then
        case byte_cnt_r is
          when "00" => rdata_r(7  downto  0) <= sbi_tgt_i.rdata;
          when "01" => rdata_r(15 downto  8) <= sbi_tgt_i.rdata;
          when "10" => rdata_r(23 downto 16) <= sbi_tgt_i.rdata;
          when "11" => rdata_r(31 downto 24) <= sbi_tgt_i.rdata;
          when others => null;
        end case;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- SBI Output Mapping
  --------------------------------------------------------------------
  -- Address is the base address + current byte offset
  -- Address is word-aligned base address + byte index (0 to 3)
  addr_r_word     <= addr_r(31 downto 2) & "00"; -- Word-aligned address
  sbi_ini_o.addr  <= std_logic_vector(resize(unsigned(addr_r_word) + unsigned(byte_cnt_r), SBI_ADDR_WIDTH));
  
  -- Select the correct byte from the 32-bit word
  -- Multiplex the 32-bit write data into 8-bit chunks for SBI
  with byte_cnt_r select
    sbi_ini_o.wdata <= wdata_r(7  downto  0) when "00",
                       wdata_r(15 downto  8) when "01",
                       wdata_r(23 downto 16) when "10",
                       wdata_r(31 downto 24) when others;

  -- Drive SBI control signals only during valid transfers
  -- Enable SBI control signals only in TRANSFER state and if the specific byte is requested
  sbi_ini_o.we <= '1' when state_r = S_TRANSFER and we_r = '1' and be_r(to_integer(byte_cnt_r)) = '1' else '0';
  sbi_ini_o.re <= '1' when state_r = S_TRANSFER and we_r = '0' and be_r(to_integer(byte_cnt_r)) = '1' else '0';

  --------------------------------------------------------------------
  -- dmem Output Mapping
  --------------------------------------------------------------------
  dmem_tgt_o.rdata <= rdata_r;
  
  -- Handshake ready only when all enabled bytes have been processed
  -- Ready is only pulsed for one cycle after all sub-transactions are finished
  dmem_tgt_o.ready <= '1' when state_r = S_DONE else '0';
  dmem_tgt_o.err   <= '0'; -- Errors not handled in this version

end architecture behavioural;