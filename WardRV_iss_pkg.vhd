library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package WardRV_iss_pkg is

    -- Parameters
    generic(
        IMEM_ADDR_WIDTH : natural := 32;
        DMEM_ADDR_WIDTH : natural := 32;
        DMEM_DATA_WIDTH : natural := 32
    );

    -- Memory signals
    signal IMEM : std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0);
    signal DMEM : std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
    signal MEM_DATA : std_logic_vector(DMEM_DATA_WIDTH-1 downto 0);

end WardRV_iss_pkg;

architecture Behavioral of WardRV_iss_pkg is
begin

    -- Memory logic goes here

end Behavioral;