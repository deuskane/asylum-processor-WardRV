library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity WardRV_fsm_regfile is
  port (
    clk_i       : in  std_logic;
    arst_b_i    : in  std_logic;
    -- Read Ports
    rs1_addr_i  : in  std_logic_vector(4 downto 0);
    rs1_re_i    : in  std_logic;
    rs1_rdata_o : out std_logic_vector(31 downto 0);
    rs2_addr_i  : in  std_logic_vector(4 downto 0);
    rs2_re_i    : in  std_logic;
    rs2_rdata_o : out std_logic_vector(31 downto 0);
    -- Write Port
    rd_addr_i   : in  std_logic_vector(4 downto 0);
    rd_wdata_i  : in  std_logic_vector(31 downto 0);
    rd_we_i     : in  std_logic
  );
end entity WardRV_fsm_regfile;

architecture behavioural of WardRV_fsm_regfile is
  type regfile_t is array (0 to 31) of std_logic_vector(31 downto 0);
  signal regs : regfile_t;
begin

  -- Lecture Asynchrone (Combinatoire)
  -- Note: regs(0) est maintenu à 0 par l'initialisation et l'exclusion d'écriture.
  rs1_rdata_o <= regs(to_integer(unsigned(rs1_addr_i))) when rs1_re_i = '1' else (others => '0');
  rs2_rdata_o <= regs(to_integer(unsigned(rs2_addr_i))) when rs2_re_i = '1' else (others => '0');

  -- Écriture Synchrone
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      -- Initialisation de tous les registres à 0
      regs <= (others => (others => '0'));
    elsif rising_edge(clk_i) then
      
      if rd_we_i = '1' then
        regs(to_integer(unsigned(rd_addr_i))) <= rd_wdata_i;
      end if;
    end if;
  end process;

end architecture behavioural;