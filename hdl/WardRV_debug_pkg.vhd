library ieee;
use ieee.std_logic_1164.all;
library asylum;
use asylum.WardRV_pkg.all;

package WardRV_debug_pkg is

  -- Debugger commands to Core
  type dbg_req_t is record
    halt     : std_logic; -- Halt request
    resume   : std_logic; -- Resume request
    ndmreset : std_logic; -- System Reset (Non-Debug Module Reset)
  end record;

  -- Core status to Debugger
  type dbg_rsp_t is record
    halted   : std_logic; -- Core is halted
    running  : std_logic; -- Core is running
  end record;

  component WardRV_debug is
    generic (
      IDCODE_VALUE : std_logic_vector(31 downto 0) := x"10000001" -- Default IDCODE
    );
    port (
      -- JTAG Interface (TCK Domain)
      jtag_ini_i : in  jtag_ini_t;
      jtag_tgt_o : out jtag_tgt_t;

      -- Core Interface (CLK Domain)
      clk_i      : in  std_logic;
      arst_b_i   : in  std_logic;
      dbg_req_o  : out dbg_req_t;
      dbg_rsp_i  : in  dbg_rsp_t
    );
  end component;

end package;
