library ieee;
use     ieee.std_logic_1164.all;

package WardRV_decode_pkg is

  -- ALU Source A Selection
  constant ALU_SRC_A_RS1   : std_logic := '0';
  constant ALU_SRC_A_PC    : std_logic := '1';

  -- ALU Source B Selection
  constant ALU_SRC_B_RS2   : std_logic_vector(2 downto 0) := "000";
  constant ALU_SRC_B_IMM_I : std_logic_vector(2 downto 0) := "001";
  constant ALU_SRC_B_IMM_S : std_logic_vector(2 downto 0) := "010";
  constant ALU_SRC_B_IMM_U : std_logic_vector(2 downto 0) := "011";
  constant ALU_SRC_B_IMM_J : std_logic_vector(2 downto 0) := "100";

  -- PC Source Selection
  constant PC_SEL_NEXT   : std_logic_vector(1 downto 0) := "00"; -- PC + 4
  constant PC_SEL_JUMP   : std_logic_vector(1 downto 0) := "01"; -- JAL, JALR
  constant PC_SEL_BRANCH : std_logic_vector(1 downto 0) := "10"; -- Branchements

end package;