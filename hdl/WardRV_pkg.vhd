library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

package WardRV_pkg is

  type inst_ini_t is record
    valid        : std_logic;        -- Access Valid
    addr         : std_logic_vector; -- Instruction Address
  end record inst_ini_t;

  type inst_tgt_t is record
    ready        : std_logic;        -- Access Ready
    inst         : std_logic_vector; -- Instruction
  end record inst_tgt_t;


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
    inst_ini_o       : out   inst_ini_t;
    inst_tgt_i       : in    inst_tgt_t;
    
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
