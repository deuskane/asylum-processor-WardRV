library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;

-- =============================================================================
-- 1. PC Manager (Gestion du Compteur de Programme)
-- =============================================================================
entity WardRV_tiny_pc is
  generic (
    RESET_ADDR : std_logic_vector(31 downto 0)
  );
  port (
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    stall_i      : in  std_logic;
    branch_req_i : in  std_logic;
    branch_tgt_i : in  std_logic_vector(31 downto 0);
    pc_o         : out std_logic_vector(31 downto 0)
  );
end entity;

architecture rtl of WardRV_tiny_pc is
  signal pc_r : std_logic_vector(31 downto 0);
begin
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      pc_r <= RESET_ADDR;
    elsif rising_edge(clk_i) then
      if branch_req_i = '1' then
        pc_r <= branch_tgt_i;
      elsif stall_i = '0' then
        pc_r <= std_logic_vector(unsigned(pc_r) + 4);
      end if;
    end if;
  end process;
  pc_o <= pc_r;
end architecture;