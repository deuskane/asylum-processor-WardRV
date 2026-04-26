library ieee;
use     ieee.std_logic_1164.all;

package WardRV_decode_pkg is

  -- ALU Source A Selection
  --type alu_src_a_sel_t is (ALU_SRC_A_RS1, ALU_SRC_A_PC);
  subtype alu_src_a_sel_t is std_logic_vector(0 downto 0);
  constant ALU_SRC_A_RS1   : std_logic_vector(0 downto 0):= "0";
  constant ALU_SRC_A_PC    : std_logic_vector(0 downto 0):= "1";

  -- ALU Source B Selection
  --type alu_src_b_sel_t is (ALU_SRC_B_RS2, ALU_SRC_B_IMM_I, ALU_SRC_B_IMM_S, ALU_SRC_B_IMM_U, ALU_SRC_B_IMM_J, ALU_SRC_B_IMM_B, ALU_SRC_B_IMM_4);
  subtype alu_src_b_sel_t is std_logic_vector(2 downto 0);
  constant ALU_SRC_B_RS2     : std_logic_vector(2 downto 0) := "000";
  constant ALU_SRC_B_IMM_I   : std_logic_vector(2 downto 0) := "001";
  constant ALU_SRC_B_IMM_S   : std_logic_vector(2 downto 0) := "010";
  constant ALU_SRC_B_IMM_U   : std_logic_vector(2 downto 0) := "011";
  constant ALU_SRC_B_IMM_J   : std_logic_vector(2 downto 0) := "100";
  constant ALU_SRC_B_IMM_B   : std_logic_vector(2 downto 0) := "101";
  constant ALU_SRC_B_IMM_4   : std_logic_vector(2 downto 0) := "110"; 
  constant ALU_SRC_B_IMM_CSR : std_logic_vector(2 downto 0) := "111";


  -- PC Source Selection
  --type pc_sel_t is (PC_SEL_NEXT, PC_SEL_BRANCH, PC_SEL_JUMP);
  subtype  pc_sel_t is std_logic_vector(1 downto 0);
  constant PC_SEL_NEXT     : std_logic_vector(1 downto 0) := "00"; -- PC + 4
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