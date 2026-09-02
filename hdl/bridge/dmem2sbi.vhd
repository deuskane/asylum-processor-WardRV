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
-- 2026-08-31  1.1      mrosiere LUT optimization: address concat instead of
--                               adder, merged wdata/rdata register, factored
--                               byte-enable mux
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
  signal addr_r       : std_logic_vector(31 downto 0);

  -- Shared 32-bit data register: holds write data during a write access,
  -- accumulates read data during a read access (never both at once).
  signal wdata_r      : std_logic_vector(31 downto 0);
--signal rdata_r      : std_logic_vector(31 downto 0);
  alias  rdata_r      : std_logic_vector(31 downto 0) is wdata_r;
  
  signal be_r         : std_logic_vector(3  downto 0);
  signal we_r         : std_logic;

  -- Current byte enable (single factored 4->1 mux)
  signal be_cur       : std_logic;

  signal cs           : std_logic;
  signal we           : std_logic;
  signal re           : std_logic;

  -- Counter to sequence through the 4 bytes
  signal be_sel_r      : std_logic_vector(3  downto 0);
  signal be_sel_r_next : std_logic_vector(3  downto 0);
  signal addr_byte     : unsigned(1  downto 0);

begin

  --------------------------------------------------------------------
  -- Current byte enable : one shared 4->1 mux reused everywhere
  --------------------------------------------------------------------
  be_cur <= or (be_r and be_sel_r);

  --------------------------------------------------------------------
  -- Control Path
  --------------------------------------------------------------------
  process(all)
  begin
    state_r_next    <= state_r;
    be_sel_r_next   <= be_sel_r;
    
    case state_r is
      -- Wait for valid request from the processor
      when S_IDLE =>
        be_sel_r_next   <= (0=> '1',
                            others => '0');
        if dmem_ini_i.valid = '1' 
        then
          state_r_next <= S_TRANSFER;
        end if;

      -- Iterate through all 4 bytes of the 32-bit word
      when S_TRANSFER =>
        -- Advance when the byte is disabled (skip) OR the SBI handshake completes
        if be_cur = '0' or sbi_tgt_i.ready = '1'
        then
          if be_sel_r(3) = '1'
          then
            state_r_next <= S_DONE;
          else
            be_sel_r_next   <= be_sel_r(2 downto 0)&'0';
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
      be_sel_r   <= (others => '0');
      addr_r     <= (others => '0');
      wdata_r    <= (others => '0');
      rdata_r    <= (others => '0');
      be_r       <= (others => '0');
      we_r       <= '0';
    elsif rising_edge(clk_i)
    then
      state_r    <= state_r_next;
      be_sel_r   <= be_sel_r_next;

      case state_r is
        when S_IDLE =>
          -- Capture the 32-bit request parameters into local registers.
          -- wdata_r is loaded with wdata for writes; for reads it is left free
          -- to accumulate the incoming SBI bytes below.
          if dmem_ini_i.valid = '1'
          then
            addr_r  <= dmem_ini_i.addr;
            be_r    <= dmem_ini_i.be;
            we_r    <= dmem_ini_i.we;
            if dmem_ini_i.we = '1'
            then
              wdata_r <= dmem_ini_i.wdata;
            end if;
          end if;
      
        when S_TRANSFER =>
          -- For Read transactions, sample the 8-bit SBI data into the correct
          -- byte lane of the shared data register.
          if sbi_tgt_i.ready = '1' and we_r = '0'
          then
            if (be_sel_r(0))
            then
              rdata_r(7  downto  0) <= sbi_tgt_i.rdata;
            end if;
            if (be_sel_r(1))
            then
              rdata_r(15 downto  8) <= sbi_tgt_i.rdata;
            end if;
            if (be_sel_r(2))
            then
              rdata_r(23 downto 16) <= sbi_tgt_i.rdata;
            end if;
            if (be_sel_r(3))
            then
              rdata_r(31 downto 24) <= sbi_tgt_i.rdata;
            end if;
          end if;
      
        when others =>
          null;
      end case;

    end if;
  end process;

  --------------------------------------------------------------------
  -- SBI Output Mapping
  --------------------------------------------------------------------
  -- Word-aligned base address with the 2-bit byte index concatenated as the
  -- two LSBs. No adder is inferred: addr_r(1 downto 0) are always "00" on a
  -- word-aligned access, so the offset is a pure wire concatenation.

  -- LUT3
  addr_byte        <= "11" when be_sel_r(3) else
                      "10" when be_sel_r(2) else
                      "01" when be_sel_r(1) else
                      "00";

  sbi_ini_o.addr   <= std_logic_vector(resize(
                        unsigned(addr_r(SBI_ADDR_WIDTH-1 downto 2)) & addr_byte,
                        SBI_ADDR_WIDTH));

  -- Multiplex the 32-bit write data into 8-bit chunks for SBI
  with addr_byte select
  sbi_ini_o.wdata  <= wdata_r(7  downto  0) when "00",
                      wdata_r(15 downto  8) when "01",
                      wdata_r(23 downto 16) when "10",
                      wdata_r(31 downto 24) when others;
  
  -- Enable SBI control signals only in TRANSFER state and if the specific
  -- byte is requested (be_cur reused, no extra mux).
  cs               <= '1' when state_r = S_TRANSFER and be_cur = '1' else '0';
  we               <=     we_r;
  re               <= not we_r;
                   
  sbi_ini_o.cs     <= cs;
  sbi_ini_o.we     <= we;
  sbi_ini_o.re     <= re;

  --------------------------------------------------------------------
  -- dmem Output Mapping
  --------------------------------------------------------------------
  dmem_tgt_o.rdata <= rdata_r;

  -- Ready is only pulsed for one cycle after all sub-transactions are finished
  dmem_tgt_o.ready <= '1' when state_r = S_DONE else '0';
  dmem_tgt_o.err   <= '0'; -- Errors not handled in this version

end architecture behavioural;
