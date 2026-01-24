library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

package RV_pkg is

  -- ===========================================================================
  -- RISC-V Opcodes (RV32I)
  -- ===========================================================================
  constant OPC_LUI      : std_logic_vector(6 downto 0) := "0110111";
  constant OPC_AUIPC    : std_logic_vector(6 downto 0) := "0010111";
  constant OPC_JAL      : std_logic_vector(6 downto 0) := "1101111";
  constant OPC_JALR     : std_logic_vector(6 downto 0) := "1100111";
  constant OPC_BRANCH   : std_logic_vector(6 downto 0) := "1100011";
  constant OPC_LOAD     : std_logic_vector(6 downto 0) := "0000011";
  constant OPC_STORE    : std_logic_vector(6 downto 0) := "0100011";
  constant OPC_OP_IMM   : std_logic_vector(6 downto 0) := "0010011";
  constant OPC_OP       : std_logic_vector(6 downto 0) := "0110011";
  constant OPC_MISC_MEM : std_logic_vector(6 downto 0) := "0001111";
  constant OPC_SYSTEM   : std_logic_vector(6 downto 0) := "1110011";

  -- RV32I Funct3
  constant F3_ADD_SUB   : std_logic_vector(2 downto 0) := "000";
  constant F3_SLL       : std_logic_vector(2 downto 0) := "001";
  constant F3_SLT       : std_logic_vector(2 downto 0) := "010";
  constant F3_SLTU      : std_logic_vector(2 downto 0) := "011";
  constant F3_XOR       : std_logic_vector(2 downto 0) := "100";
  constant F3_SRL_SRA   : std_logic_vector(2 downto 0) := "101";
  constant F3_OR        : std_logic_vector(2 downto 0) := "110";
  constant F3_AND       : std_logic_vector(2 downto 0) := "111";

  -- RV32I Funct7
  constant F7_DEFAULT   : std_logic_vector(6 downto 0) := "0000000";
  constant F7_ALT       : std_logic_vector(6 downto 0) := "0100000";

  -- RV32I Funct3 (Branch)
  constant F3_BEQ       : std_logic_vector(2 downto 0) := "000";
  
  -- ===========================================================================
  -- RISC-V Compressed Opcodes (RV32C)
  -- ===========================================================================
  constant OPC_C0       : std_logic_vector(1 downto 0) := "00";
  constant OPC_C1       : std_logic_vector(1 downto 0) := "01";
  constant OPC_C2       : std_logic_vector(1 downto 0) := "10";

  -- RV32C Funct3 (Selection)
  constant F3_C_ADDI4SPN : std_logic_vector(2 downto 0) := "000";
  constant F3_C_LW       : std_logic_vector(2 downto 0) := "010";
  constant F3_C_SW       : std_logic_vector(2 downto 0) := "110";
  constant F3_C_ADDI     : std_logic_vector(2 downto 0) := "000";
  constant F3_C_JAL      : std_logic_vector(2 downto 0) := "001";
  constant F3_C_LI       : std_logic_vector(2 downto 0) := "010";
  constant F3_C_LUI      : std_logic_vector(2 downto 0) := "011";
  constant F3_C_MISC_ALU : std_logic_vector(2 downto 0) := "100";
  constant F3_C_J        : std_logic_vector(2 downto 0) := "101";
  constant F3_C_BEQZ     : std_logic_vector(2 downto 0) := "110";
  constant F3_C_BNEZ     : std_logic_vector(2 downto 0) := "111";
  constant F3_C_SLLI     : std_logic_vector(2 downto 0) := "000";
  constant F3_C_LWSP     : std_logic_vector(2 downto 0) := "010";
  constant F3_C_JR_MV    : std_logic_vector(2 downto 0) := "100";
  constant F3_C_SWSP     : std_logic_vector(2 downto 0) := "110";

  -- ===========================================================================
  -- RISC-V Bit Manipulation (RV32B: Zba, Zbb, Zbc, Zbs)
  -- ===========================================================================
  -- Zba (Address generation)
  constant F3_SH1ADD    : std_logic_vector(2 downto 0) := "010";
  constant F3_SH2ADD    : std_logic_vector(2 downto 0) := "100";
  constant F3_SH3ADD    : std_logic_vector(2 downto 0) := "110";
  constant F7_ZBA       : std_logic_vector(6 downto 0) := "0100000";

  -- Zbb (Basic bit-manip)
  constant F3_ANDN      : std_logic_vector(2 downto 0) := "111";
  constant F3_ORN       : std_logic_vector(2 downto 0) := "110";
  constant F3_XNOR      : std_logic_vector(2 downto 0) := "100";
  constant F7_ZBB       : std_logic_vector(6 downto 0) := "0100000";

  constant F3_MAX       : std_logic_vector(2 downto 0) := "110";
  constant F3_MAXU      : std_logic_vector(2 downto 0) := "111";
  constant F3_MIN       : std_logic_vector(2 downto 0) := "100";
  constant F3_MINU      : std_logic_vector(2 downto 0) := "101";
  constant F7_MINMAX    : std_logic_vector(6 downto 0) := "

end RV_pkg;
