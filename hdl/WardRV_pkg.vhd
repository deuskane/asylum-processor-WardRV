library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

package WardRV_pkg is

  type inst_ini_t is record
    valid        : std_logic;        -- Access Valid
    addr         : std_logic_vector(31 downto 0); -- Instruction Address
  end record inst_ini_t;

  type inst_tgt_t is record
    ready        : std_logic;        -- Access Ready
    inst         : std_logic_vector(31 downto 0); -- Instruction
  end record inst_tgt_t;

  type sbi_ini_t is record
    valid        : std_logic;
    addr         : std_logic_vector(31 downto 0);
    wdata        : std_logic_vector(31 downto 0);
    we           : std_logic;
    be           : std_logic_vector(3 downto 0);
  end record sbi_ini_t;

  type sbi_tgt_t is record
    ready        : std_logic;
    rdata        : std_logic_vector(31 downto 0);
    err          : std_logic;
  end record sbi_tgt_t;

  type jtag_ini_t is record
    tck          : std_logic;
    trst_n       : std_logic;
    tms          : std_logic;
    tdi          : std_logic;
  end record jtag_ini_t;

  type jtag_tgt_t is record
    tdo          : std_logic;
    tdo_en       : std_logic;
  end record jtag_tgt_t;

-- [COMPONENT_INSERT][BEGIN]
component WardRV is
  -- =====[ Parameters ]==========================
  generic (
    
     RESET_ADDR        : std_logic_vector(32-1 downto 0) := (others => '0');
     IT_ADDR           : std_logic_vector(32-1 downto 0) := (others => '0');
     BIG_ENDIAN        : boolean          := false;
     DM_ENABLE         : boolean          := true;
     DEBUG             : boolean          := false
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
    it_ack_o         : out   std_logic;

    -- JTAG
    jtag_ini_i       : in    jtag_ini_t;
    jtag_tgt_o       : out   jtag_tgt_t
    );
end component WardRV;

-- [COMPONENT_INSERT][END]

end WardRV_pkg;
