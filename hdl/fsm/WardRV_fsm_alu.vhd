-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_alu.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-04-06  1.0      mrosiere Created
-------------------------------------------------------------------------------
library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;

package WardRV_fsm_alu_pkg is
  type alu_op_t is (ALU_ADD, ALU_SUB,ALU_SLL, ALU_SLT, ALU_SLTU, ALU_XOR, ALU_SRL, ALU_SRA, ALU_OR, ALU_AND
                    --, ALU_PASS_B
                    );
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
    sign_o  : out std_logic;
    carry_o : out std_logic
  );
end entity WardRV_fsm_alu;

architecture behavioural of WardRV_fsm_alu is
  signal res : std_logic_vector(31 downto 0);
  signal carry : std_logic;
begin

  process(all)
    variable v_res     : std_logic_vector(31 downto 0);
    variable v_res_ext : unsigned(32 downto 0) := (others => '0');
    variable v_carry   : std_logic := '0';
  begin
    v_res   := (others => '0');
    v_carry := '0';

    case op_i is
      when ALU_ADD     => 
        v_res_ext := unsigned('0' & src_a_i) + unsigned('0' & src_b_i);
        v_res     := std_logic_vector(v_res_ext(31 downto 0));
        v_carry   := v_res_ext(32);

      when ALU_SUB    => 
        v_res_ext := unsigned('0' & src_a_i) - unsigned('0' & src_b_i);
        v_res     := std_logic_vector(v_res_ext(31 downto 0));
        v_carry   := v_res_ext(32); -- Borrow flag

      when ALU_SLL    => v_res := std_logic_vector(shift_left(unsigned(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));

      when ALU_SLT    => 
        if signed(src_a_i) < signed(src_b_i) then v_res := x"00000001"; end if;

      when ALU_SLTU   => 
        if unsigned(src_a_i) < unsigned(src_b_i) then v_res := x"00000001"; end if;

      when ALU_XOR    => v_res := src_a_i xor src_b_i;
      when ALU_SRL    => v_res := std_logic_vector(shift_right(unsigned(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_SRA    => v_res := std_logic_vector(shift_right(  signed(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_OR     => v_res := src_a_i or  src_b_i;
      when ALU_AND    => v_res := src_a_i and src_b_i;
      --when ALU_PASS_B => v_res := src_b_i;
      when others     => null;
    end case;

    res   <= v_res;
    carry <= v_carry;
  end process;

  res_o   <= res;
  zero_o  <= '1' when res = x"00000000" else '0';
  sign_o  <= res(31);
  carry_o <= carry;

end architecture behavioural;