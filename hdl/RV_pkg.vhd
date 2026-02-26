-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : RV_pkg.vhd
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

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

package RV_pkg is

  -- ===========================================================================
  -- RISC-V Opcodes (RV32I)
  -- ===========================================================================
  constant OPC_LUI      : bit_vector(6 downto 0) := "0110111";
  constant OPC_AUIPC    : bit_vector(6 downto 0) := "0010111";
  constant OPC_JAL      : bit_vector(6 downto 0) := "1101111";
  constant OPC_JALR     : bit_vector(6 downto 0) := "1100111";
  constant OPC_BRANCH   : bit_vector(6 downto 0) := "1100011";
  constant OPC_LOAD     : bit_vector(6 downto 0) := "0000011";
  constant OPC_STORE    : bit_vector(6 downto 0) := "0100011";
  constant OPC_OP_IMM   : bit_vector(6 downto 0) := "0010011";
  constant OPC_OP       : bit_vector(6 downto 0) := "0110011";
  constant OPC_MISC_MEM : bit_vector(6 downto 0) := "0001111";
  constant OPC_SYSTEM   : bit_vector(6 downto 0) := "1110011";

  -- RV32I Funct3
  constant F3_ADD_SUB   : bit_vector(2 downto 0) := "000";
  constant F3_SLL       : bit_vector(2 downto 0) := "001";
  constant F3_SLT       : bit_vector(2 downto 0) := "010";
  constant F3_SLTU      : bit_vector(2 downto 0) := "011";
  constant F3_XOR       : bit_vector(2 downto 0) := "100";
  constant F3_SRL_SRA   : bit_vector(2 downto 0) := "101";
  constant F3_OR        : bit_vector(2 downto 0) := "110";
  constant F3_AND       : bit_vector(2 downto 0) := "111";
  constant F3_ADD       : bit_vector(2 downto 0) := "000";
  constant F3_SRL       : bit_vector(2 downto 0) := "101";

  -- RV32I Funct7
  constant F7_DEFAULT   : bit_vector(6 downto 0) := "0000000";
  constant F7_ALT       : bit_vector(6 downto 0) := "0100000";

  -- RV32I Funct3 (Branch)
  constant F3_BEQ       : bit_vector(2 downto 0) := "000";
  constant F3_BNE       : bit_vector(2 downto 0) := "001";
  constant F3_BLT       : bit_vector(2 downto 0) := "100";
  constant F3_BGE       : bit_vector(2 downto 0) := "101";
  constant F3_BLTU      : bit_vector(2 downto 0) := "110";
  constant F3_BGEU      : bit_vector(2 downto 0) := "111";
  
  -- RV32I Funct3 (Load/Store)
  constant F3_LB        : bit_vector(2 downto 0) := "000";
  constant F3_LH        : bit_vector(2 downto 0) := "001";
  constant F3_LW        : bit_vector(2 downto 0) := "010";
  constant F3_LBU       : bit_vector(2 downto 0) := "100";
  constant F3_LHU       : bit_vector(2 downto 0) := "101";
  constant F3_SB        : bit_vector(2 downto 0) := "000";
  constant F3_SH        : bit_vector(2 downto 0) := "001";
  constant F3_SW        : bit_vector(2 downto 0) := "010";

  -- RV32I Funct3 (System)
  constant F3_PRIV      : bit_vector(2 downto 0) := "000";
  constant F3_CSRRW     : bit_vector(2 downto 0) := "001";
  constant F3_CSRRS     : bit_vector(2 downto 0) := "010";
  constant F3_CSRRC     : bit_vector(2 downto 0) := "011";
  constant F3_CSRRWI    : bit_vector(2 downto 0) := "101";
  constant F3_CSRRSI    : bit_vector(2 downto 0) := "110";
  constant F3_CSRRCI    : bit_vector(2 downto 0) := "111";

  -- ===========================================================================
  -- RISC-V Compressed Opcodes (RV32C)
  -- ===========================================================================
  constant OPC_C0       : bit_vector(1 downto 0) := "00";
  constant OPC_C1       : bit_vector(1 downto 0) := "01";
  constant OPC_C2       : bit_vector(1 downto 0) := "10";

  -- RV32C Funct3 (Selection)
  constant F3_C_ADDI4SPN : bit_vector(2 downto 0) := "000";
  constant F3_C_LW       : bit_vector(2 downto 0) := "010";
  constant F3_C_SW       : bit_vector(2 downto 0) := "110";
  constant F3_C_ADDI     : bit_vector(2 downto 0) := "000";
  constant F3_C_JAL      : bit_vector(2 downto 0) := "001";
  constant F3_C_LI       : bit_vector(2 downto 0) := "010";
  constant F3_C_LUI      : bit_vector(2 downto 0) := "011";
  constant F3_C_MISC_ALU : bit_vector(2 downto 0) := "100";
  constant F3_C_J        : bit_vector(2 downto 0) := "101";
  constant F3_C_BEQZ     : bit_vector(2 downto 0) := "110";
  constant F3_C_BNEZ     : bit_vector(2 downto 0) := "111";
  constant F3_C_SLLI     : bit_vector(2 downto 0) := "000";
  constant F3_C_LWSP     : bit_vector(2 downto 0) := "010";
  constant F3_C_JR_MV    : bit_vector(2 downto 0) := "100";
  constant F3_C_SWSP     : bit_vector(2 downto 0) := "110";

  -- ===========================================================================
  -- RISC-V Bit Manipulation (RV32B: Zba, Zbb, Zbc, Zbs)
  -- ===========================================================================
  -- Zba (Address generation)
  constant F3_SH1ADD    : bit_vector(2 downto 0) := "010";
  constant F3_SH2ADD    : bit_vector(2 downto 0) := "100";
  constant F3_SH3ADD    : bit_vector(2 downto 0) := "110";
  constant F7_ZBA       : bit_vector(6 downto 0) := "0100000";

  -- Zbb (Basic bit-manip)
  constant F3_ANDN      : bit_vector(2 downto 0) := "111";
  constant F3_ORN       : bit_vector(2 downto 0) := "110";
  constant F3_XNOR      : bit_vector(2 downto 0) := "100";
  constant F7_ZBB       : bit_vector(6 downto 0) := "0100000";

  constant F3_MAX       : bit_vector(2 downto 0) := "110";
  constant F3_MAXU      : bit_vector(2 downto 0) := "111";
  constant F3_MIN       : bit_vector(2 downto 0) := "100";
  constant F3_MINU      : bit_vector(2 downto 0) := "101";
  constant F7_MINMAX    : bit_vector(6 downto 0) := "0000101";

end RV_pkg;
