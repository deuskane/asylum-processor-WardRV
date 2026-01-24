library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

package WardRV_pkg is
-- [COMPONENT_INSERT][BEGIN]
component WardRV is
  -- =====[ Parameters ]==========================
  generic (
    
     RESET_ADDR        : std_logic_vector := (others => '0');
     IT_ADDR           : std_logic_vector := (others => '0')
     );
  -- =====[ Interfaces ]==========================
  port (
    clk_i             : in  std_logic;
    arst_b_i          : in  std_logic;

    -- Instructions
    ics_o            : out std_logic;
    iaddr_o          : out std_logic_vector(32-1 downto 0);
    idata_i          : in  std_logic_vector(32-1 downto 0);
    
    -- Bus
    sbi_ini_o        : out   sbi_ini_t;
    sbi_tgt_i        : in    sbi_tgt_t;

    -- To/From IT Ctrl
    it_val_i         : in    std_logic;
    it_ack_o         : out   std_logic
    );
end component WardRV;

-- [COMPONENT_INSERT][END]

end WardRV_pkg;
