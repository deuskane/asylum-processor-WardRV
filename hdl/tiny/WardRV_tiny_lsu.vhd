-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_tiny_lsu.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: LSU (Memory Interface)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;

-- =============================================================================
-- 4. LSU (Memory Interface)
-- =============================================================================
entity WardRV_tiny_lsu is
  port (
    -- Core Interface
    alu_res_i    : in  std_logic_vector(31 downto 0);
    rs2_data_i   : in  std_logic_vector(31 downto 0);
    opcode_i     : in  std_logic_vector(6 downto 0);
    funct3_i     : in  std_logic_vector(2 downto 0);
    
    -- Bus Interface
    sbi_ini_o    : out sbi_ini_t;
    sbi_tgt_i    : in  sbi_tgt_t;
    
    -- Output
    mem_rdata_o  : out std_logic_vector(31 downto 0);
    busy_o       : out std_logic
  );
end entity;

architecture rtl of WardRV_tiny_lsu is
  signal is_load, is_store : boolean;
  signal byte_off : integer range 0 to 3 := 0;
begin
  is_load  <= (opcode_i = OPC_LOAD);
  is_store <= (opcode_i = OPC_STORE);
  byte_off <= to_integer(unsigned(alu_res_i(1 downto 0)));

  -- Request
  sbi_ini_o.valid <= '1' when (is_load or is_store) else '0';
  sbi_ini_o.addr  <= alu_res_i;
  sbi_ini_o.we    <= '1' when is_store else '0';
  
  -- Write Data & BE
  process(all)
  begin
    sbi_ini_o.wdata <= (others => '0');
    sbi_ini_o.be    <= "0000";
    
    if is_store then
      case funct3_i is
        when F3_SB => 
          sbi_ini_o.be(byte_off) <= '1';
          sbi_ini_o.wdata(7 downto 0)   <= rs2_data_i(7 downto 0);
          sbi_ini_o.wdata(15 downto 8)  <= rs2_data_i(7 downto 0);
          sbi_ini_o.wdata(23 downto 16) <= rs2_data_i(7 downto 0);
          sbi_ini_o.wdata(31 downto 24) <= rs2_data_i(7 downto 0);
        when F3_SH =>
          if byte_off = 0 then sbi_ini_o.be <= "0011"; else sbi_ini_o.be <= "1100"; end if;
          sbi_ini_o.wdata(15 downto 0)  <= rs2_data_i(15 downto 0);
          sbi_ini_o.wdata(31 downto 16) <= rs2_data_i(15 downto 0);
        when F3_SW =>
          sbi_ini_o.be <= "1111";
          sbi_ini_o.wdata <= rs2_data_i;
        when others => null;
      end case;
    end if;
  end process;

  -- Read Data
  process(all)
    variable v_byte : std_logic_vector(7 downto 0);
    variable v_half : std_logic_vector(15 downto 0);
    variable v_rdata_shifted : std_logic_vector(31 downto 0);
  begin
    v_rdata_shifted := std_logic_vector(shift_right(unsigned(sbi_tgt_i.rdata), byte_off * 8));
    v_byte := v_rdata_shifted(7 downto 0);
    v_half := v_rdata_shifted(15 downto 0);
    mem_rdata_o <= (others => '0');

    case funct3_i is
      when F3_LB  => mem_rdata_o <= std_logic_vector(resize(signed(v_byte), 32));
      when F3_LBU => mem_rdata_o <= std_logic_vector(resize(unsigned(v_byte), 32));
      when F3_LH  => mem_rdata_o <= std_logic_vector(resize(signed(v_half), 32));
      when F3_LHU => mem_rdata_o <= std_logic_vector(resize(unsigned(v_half), 32));
      when F3_LW  => mem_rdata_o <= sbi_tgt_i.rdata; -- Aligned assumed
      when others => null;
    end case;
  end process;

  busy_o <= '1' when (is_load or is_store) and sbi_tgt_i.ready = '0' else '0';

end architecture;