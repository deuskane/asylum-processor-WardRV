-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_decode_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-- This package defines the types and constants used for instruction decoding 
-- and FSM control signals throughout the processor.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-11-20  1.1      mrosiere ALU source B : 2 bits (RS2, IMM, IMM_4, CSR)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

package WardRV_decode_pkg is

  -- ALU Source A Selection
  --type alu_src_a_sel_t is (ALU_SRC_A_RS1, ALU_SRC_A_PC);
  subtype alu_src_a_sel_t is std_logic_vector(1 downto 0);
  constant ALU_SRC_A_RS1     : std_logic_vector(1 downto 0):= "00";
  constant ALU_SRC_A_PC      : std_logic_vector(1 downto 0):= "01";
  constant ALU_SRC_A_IMM_CSR : std_logic_vector(1 downto 0):= "10";

  -- ALU Source B Selection
  subtype alu_src_b_sel_t is std_logic_vector(1 downto 0);
  constant ALU_SRC_B_RS2     : std_logic_vector(1 downto 0) := "00";
  constant ALU_SRC_B_IMM     : std_logic_vector(1 downto 0) := "01"; -- Immediate of the instruction
  constant ALU_SRC_B_IMM_4   : std_logic_vector(1 downto 0) := "10";
  constant ALU_SRC_B_CSR     : std_logic_vector(1 downto 0) := "11";


  -- PC Source Selection
  --type pc_sel_t is (PC_SEL_NEXT, PC_SEL_BRANCH, PC_SEL_JUMP);
  subtype  pc_sel_t is std_logic_vector(1 downto 0);
  constant PC_SEL_NEXT     : std_logic_vector(1 downto 0) := "00"; -- PC + 4
  constant PC_SEL_XEPC     : std_logic_vector(1 downto 0) := "01"; -- EPC
  constant PC_SEL_BRANCH   : std_logic_vector(1 downto 0) := "10"; -- Branchements
  constant PC_SEL_JUMP     : std_logic_vector(1 downto 0) := "11"; -- JAL, JALR
  
  -- RD Source Selection (for writeback)
  --type rd_src_t is (RD_SRC_ALU, RD_SRC_MEM, RD_SRC_PC_PLUS4);
  subtype  rd_src_t is std_logic_vector(1 downto 0);
  constant RD_SRC_ALU      : std_logic_vector(1 downto 0) := "00"; -- ALU result
  constant RD_SRC_MEM      : std_logic_vector(1 downto 0) := "01"; -- Load instructions
  constant RD_SRC_PC_PLUS4 : std_logic_vector(1 downto 0) := "10"; -- JAL, JALR
  constant RD_SRC_CSR      : std_logic_vector(1 downto 0) := "11"; -- CSR instructions

end package;