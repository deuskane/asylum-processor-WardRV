--------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
--------------------------------------------------------------------
-- File       : imem2sbi.vhd
-- Author     : Mathieu Rosiere
--------------------------------------------------------------------
-- Description: 
-- This module implements a bridge between the internal imem interface and 
-- the Simple Bus Interface (SBI). 
-- It sequences 32-bit instruction fetches into four 8-bit SBI read transactions.
-- Copyright (c) 2026
--------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-05-10  1.0      mrosiere Created from dmem bridge
--------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;

entity imem2sbi is
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    -- imem interface (Master side)
    imem_ini_i : in  imem_ini_t;
    imem_tgt_o : out imem_tgt_t;

    -- sbi interface (Slave side)
    sbi_ini_o  : out sbi_ini_t;
    sbi_tgt_i  : in  sbi_tgt_t 
  );
end entity imem2sbi;

architecture behavioural of imem2sbi is
  -- State machine to sequence 32-bit instruction fetch into four 8-bit SBI reads
  type state_t is (S_IDLE,      -- Wait for imem request
                   S_TRANSFER,  -- Perform SBI byte-reads
                   S_DONE);     -- Signal completion to imem
  
  signal state_r      : state_t;
  signal state_r_next : state_t;

  -- Internal registers to buffer the request address and accumulate instruction bytes
  signal addr_r       : std_logic_vector(31 downto 0);
  signal inst_r       : std_logic_vector(31 downto 0);
  
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
      -- Wait for valid fetch request
      when S_IDLE =>
        byte_cnt_r_next <= (others => '0');
        if imem_ini_i.valid = '1' then
          state_r_next <= S_TRANSFER;
        end if;

      -- Perform 4 successive 8-bit reads
      when S_TRANSFER =>
        if sbi_tgt_i.ready = '1' then
          if byte_cnt_r = 3 then
            state_r_next <= S_DONE;
          else
            byte_cnt_r_next <= byte_cnt_r + 1;
          end if;
        end if;

      -- Pulse imem_ready
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
    if arst_b_i = '0' then
      state_r    <= S_IDLE;
      byte_cnt_r <= (others => '0');
      addr_r     <= (others => '0');
      inst_r     <= (others => '0');
    elsif rising_edge(clk_i) then
      state_r    <= state_r_next;
      byte_cnt_r <= byte_cnt_r_next;

      -- Capture target address
      if state_r = S_IDLE and imem_ini_i.valid = '1' then
        addr_r  <= imem_ini_i.addr;
      end if;

      -- Sample SBI data into the instruction register
      if state_r = S_TRANSFER and sbi_tgt_i.ready = '1' then
        case byte_cnt_r is
          when "00" => inst_r(7  downto  0) <= sbi_tgt_i.rdata;
          when "01" => inst_r(15 downto  8) <= sbi_tgt_i.rdata;
          when "10" => inst_r(23 downto 16) <= sbi_tgt_i.rdata;
          when "11" => inst_r(31 downto 24) <= sbi_tgt_i.rdata;
          when others => null;
        end case;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- SBI Output Mapping
  --------------------------------------------------------------------
  -- Address sequencing: word-aligned address + byte offset
  sbi_ini_o.addr  <= std_logic_vector(unsigned(addr_r(31 downto 2) & "00") + byte_cnt_r);
  sbi_ini_o.wdata <= (others => '0'); -- No writes in imem bridge
  sbi_ini_o.we    <= '0';             -- Read only
  sbi_ini_o.re    <= '1' when state_r = S_TRANSFER else '0';

  --------------------------------------------------------------------
  -- imem Output Mapping
  --------------------------------------------------------------------
  imem_tgt_o.inst  <= inst_r;
  imem_tgt_o.ready <= '1' when state_r = S_DONE else '0';

end architecture behavioural;