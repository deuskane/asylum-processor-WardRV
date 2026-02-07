-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_tiny_decode.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Decode & Register File
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
use work.WardRV_tiny_pkg.all;

-- =============================================================================
-- 2. Decode & Register File (Décodage et Registres)
-- =============================================================================
entity WardRV_tiny_decode is
  port (
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    
    -- Fetch Interface
    pc_i         : in  std_logic_vector(31 downto 0);
    inst_i       : in  std_logic_vector(31 downto 0);
    
    -- Writeback Interface (from Stage 2)
    wb_we_i      : in  std_logic;
    wb_addr_i    : in  std_logic_vector(4 downto 0);
    wb_data_i    : in  std_logic_vector(31 downto 0);

    -- Forwarding (from Stage 2 for back-to-back dependency)
    fwd_we_i     : in  std_logic;
    fwd_addr_i   : in  std_logic_vector(4 downto 0);
    fwd_data_i   : in  std_logic_vector(31 downto 0);

    -- Outputs to Execute
    rs1_data_o   : out std_logic_vector(31 downto 0);
    rs2_data_o   : out std_logic_vector(31 downto 0);
    imm_o        : out std_logic_vector(31 downto 0);
    rd_o         : out std_logic_vector(4 downto 0);
    opcode_o     : out std_logic_vector(6 downto 0);
    funct3_o     : out std_logic_vector(2 downto 0);
    funct7_o     : out std_logic_vector(6 downto 0);
    pc_o         : out std_logic_vector(31 downto 0)
  );
end entity;

architecture rtl of WardRV_tiny_decode is
  signal regs : reg_file_t := (others => (others => '0'));
  
  signal rs1_addr, rs2_addr : std_logic_vector(4 downto 0);
  signal rs1_raw, rs2_raw   : std_logic_vector(31 downto 0);
  signal imm_val            : std_logic_vector(31 downto 0);
begin
  
  -- Instruction Fields extraction
  rs1_addr <= inst_i(19 downto 15);
  rs2_addr <= inst_i(24 downto 20);
  rd_o     <= inst_i(11 downto 7);
  opcode_o <= inst_i(6 downto 0);
  funct3_o <= inst_i(14 downto 12);
  funct7_o <= inst_i(31 downto 25);
  pc_o     <= pc_i;

  -- Immediate Generation
  process(inst_i)
    variable opcode : std_logic_vector(6 downto 0);
  begin
    opcode := inst_i(6 downto 0);
    case opcode is
      when OPC_LUI | OPC_AUIPC              => imm_val <= inst_i(31 downto 12) & x"000";
      when OPC_JAL                          => imm_val <= (31 downto 20 => inst_i(31)) & inst_i(19 downto 12) & inst_i(20) & inst_i(30 downto 21) & '0';
      when OPC_JALR | OPC_LOAD | OPC_OP_IMM => imm_val <= (31 downto 11 => inst_i(31)) & inst_i(30 downto 20);
      when OPC_STORE                        => imm_val <= (31 downto 11 => inst_i(31)) & inst_i(30 downto 25) & inst_i(11 downto 7);
      when OPC_BRANCH                       => imm_val <= (31 downto 12 => inst_i(31)) & inst_i(7) & inst_i(30 downto 25) & inst_i(11 downto 8) & '0';
      when others                           => imm_val <= (others => '0');
    end case;
  end process;
  imm_o <= imm_val;

  -- Register File Write (Synchronous)
  process(clk_i)
  begin
    if rising_edge(clk_i) then
      if wb_we_i = '1' and unsigned(wb_addr_i) /= 0 then
        regs(to_integer(unsigned(wb_addr_i))) <= wb_data_i;
      end if;
    end if;
  end process;

  -- Register File Read (Asynchronous)
  rs1_raw <= regs(to_integer(unsigned(rs1_addr)));
  rs2_raw <= regs(to_integer(unsigned(rs2_addr)));

  -- Forwarding Logic (Combinatorial from Stage 2)
  -- Necessary because Stage 2 writes at the end of the cycle, but Stage 1 reads during the cycle.
  process(all)
  begin
    -- RS1 Forwarding
    if fwd_we_i = '1' and fwd_addr_i = rs1_addr and unsigned(rs1_addr) /= 0 then
      rs1_data_o <= fwd_data_i;
    else
      rs1_data_o <= rs1_raw;
    end if;

    -- RS2 Forwarding
    if fwd_we_i = '1' and fwd_addr_i = rs2_addr and unsigned(rs2_addr) /= 0 then
      rs2_data_o <= fwd_data_i;
    else
      rs2_data_o <= rs2_raw;
    end if;
  end process;

end architecture;