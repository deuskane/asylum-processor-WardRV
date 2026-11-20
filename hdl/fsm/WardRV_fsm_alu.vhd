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
-- 2026-10-05  1.1      mrosiere Add ALU_CLR (B and not A) for CSRRC/CSRRCI
--                               sign_o is now the signed less-than flag
--                               (N xor V) for ALU_SUB
-- 2026-11-20  1.2      mrosiere One adder for ADD, SUB, SLT and SLTU (flags of the
--                               subtraction for the branches and SLT / SLTU)
-------------------------------------------------------------------------------
library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;

package WardRV_fsm_alu_pkg is
  -- ALU_CLR : bit clear, res = src_b and not src_a (CSRRC / CSRRCI)
  type alu_op_t is (ALU_ADD, ALU_SUB,ALU_SLL, ALU_SLT, ALU_SLTU, ALU_XOR, ALU_SRL, ALU_SRA, ALU_OR, ALU_AND, ALU_CLR
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
    sign_o  : out std_logic; -- signed   (src_a < src_b), every op but ALU_ADD
    carry_o : out std_logic  -- unsigned (src_a < src_b) (borrow), every op but ALU_ADD
  );
end entity WardRV_fsm_alu;

architecture behavioural of WardRV_fsm_alu is
  -- One adder for ADD, SUB, SLT and SLTU : src_a - src_b = src_a + not src_b + 1
  signal sub      : std_logic;                     -- '1' : subtraction
  signal sum      : unsigned(32 downto 0);         -- bit 32 : carry out
  signal lt_u     : std_logic;                     -- unsigned src_a < src_b
  signal lt_s     : std_logic;                     -- signed   src_a < src_b
  signal res      : std_logic_vector(31 downto 0);
begin

  sub  <= '0' when op_i = ALU_ADD else '1';
  sum  <= unsigned('0' & src_a_i) + unsigned('0' & (src_b_i xor sub)) + sub;

  -- No carry out of the subtraction : borrow, unsigned less-than
  lt_u <= not sum(32);
  -- Signed less-than : the sign of src_a when the signs differ (the
  -- difference may overflow), else the sign of the difference
  lt_s <= src_a_i(31) when src_a_i(31) /= src_b_i(31) else sum(31);

  process(all)
  begin
    res <= (others => '0');

    case op_i is
      when ALU_ADD |
           ALU_SUB    => res    <= std_logic_vector(sum(31 downto 0));
      when ALU_SLT    => res(0) <= lt_s;
      when ALU_SLTU   => res(0) <= lt_u;
      when ALU_SLL    => res    <= std_logic_vector(shift_left(unsigned(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_XOR    => res    <= src_a_i xor src_b_i;
      when ALU_SRL    => res    <= std_logic_vector(shift_right(unsigned(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_SRA    => res    <= std_logic_vector(shift_right(  signed(src_a_i), to_integer(unsigned(src_b_i(4 downto 0)))));
      when ALU_OR     => res    <= src_a_i or  src_b_i;
      when ALU_AND    => res    <= src_a_i and src_b_i;
      when ALU_CLR    => res    <= src_b_i and not src_a_i;
      --when ALU_PASS_B => res    <= src_b_i;
    end case;
  end process;

  res_o   <= res;
  zero_o  <= '1' when res = x"00000000" else '0';
  sign_o  <= lt_s;
  carry_o <= lt_u;

end architecture behavioural;
