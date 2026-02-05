library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package WardRV_tiny_pkg is

  -- From WardRV_tiny_decode
  type reg_file_t is array (0 to 31) of std_logic_vector(31 downto 0);

  -- From WardRV_tiny (Pipeline Register)
  type pipe_reg_t is record
    pc       : std_logic_vector(31 downto 0);
    rs1_data : std_logic_vector(31 downto 0);
    rs2_data : std_logic_vector(31 downto 0);
    imm      : std_logic_vector(31 downto 0);
    rd       : std_logic_vector(4 downto 0);
    opcode   : std_logic_vector(6 downto 0);
    funct3   : std_logic_vector(2 downto 0);
    funct7   : std_logic_vector(6 downto 0);
    valid    : std_logic;
  end record;

  -- From WardRV_tiny_lsu_8b
  type lsu_8b_state_t is (IDLE, MEM_ACCESS, UPDATE_PIPE);

end package;