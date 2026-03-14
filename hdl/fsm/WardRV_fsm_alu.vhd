library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;

package WardRV_fsm_alu_pkg is
  type alu_op_t is (ALU_ADD, ALU_SUB, ALU_SLL, ALU_SLT, ALU_SLTU, ALU_XOR, ALU_SRL, ALU_SRA, ALU_OR, ALU_AND, ALU_PASS_B);
end package WardRV_fsm_alu_pkg;

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_fsm_alu_pkg.all;

entity WardRV_fsm_alu is
  port (
    src_a_i : in  std_logic_vector(31 downto 0);
    src_b_i : in  std_logic_vector(31 downto 0);
    op_i    : in  alu_op_t;
    res_o   : out std_logic_vector(31 downto 0);
    zero_o  : out std_logic;
    sign_o  : out std_logic
  );
end entity WardRV_fsm_alu;

architecture behavioural of WardRV_fsm_alu is
  signal res : std_logic_vector(31 downto 0);
begin

  process(src_a_i, src_b_i, op_i)
  begin
    case op_i is
      when ALU_ADD => res <= std_logic_vector(signed(src_a_i) + signed(src_b_i));
      when ALU_SUB => res <= std_logic_vector(signed(src_a_i) - signed(src_b_i));
      when ALU_SLL => res <= std_logic_vector(shift_left(unsigned(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_SLT => 
        if signed(src_a_i) < signed(src_b_i) then 
          res <= x"00000001"; 
        else 
          res <= (others => '0'); 
        end if;
      when ALU_SLTU=> 
        if unsigned(src_a_i) < unsigned(src_b_i) then 
          res <= x"00000001"; 
        else 
          res <= (others => '0'); 
        end if;
      when ALU_XOR => res <= src_a_i xor src_b_i;
      when ALU_SRL => res <= std_logic_vector(shift_right(unsigned(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_SRA => res <= std_logic_vector(shift_right(signed(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_OR  => res <= src_a_i or src_b_i;
      when ALU_AND => res <= src_a_i and src_b_i;
      when ALU_PASS_B => res <= src_b_i;
      when others  => res <= (others => '0');
    end case;
  end process;

  res_o <= res;
  
  zero_o <= '1' when res = x"00000000" else '0';
  sign_o <= res(31);

end architecture behavioural;