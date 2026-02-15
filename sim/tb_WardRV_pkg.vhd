-------------------------------------------------------------------------------
-- Title      : WardRV Testbench Package
-- Project    : 
-------------------------------------------------------------------------------
-- File       : tb_WardRV_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Shared code for WardRV testbenches
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     std.textio.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

package tb_WardRV_pkg is

  -- Constants
  constant C_CLK_PERIOD  : time := 10 ns;
  constant C_TOHOST_ADDR : std_logic_vector(31 downto 0) := x"80001000";
  constant C_SIGNATURE_ADDR : std_logic_vector(31 downto 0) := x"80001004";
  constant C_MEM_SIZE    : integer := 65536; -- 64KB

  -- Memory Type
  type ram_t is array (0 to C_MEM_SIZE-1) of std_logic_vector(7 downto 0);

  -- Helper to read Hex
  impure function init_ram(file_name : string) return ram_t;

  -- Helper to print instruction
  procedure print_instruction(
    constant addr    : in std_logic_vector;
    constant inst    : in std_logic_vector;
    constant verbose : in boolean
  );

end package tb_WardRV_pkg;

package body tb_WardRV_pkg is

  impure function init_ram(file_name : string) return ram_t is
    file f_in       : text open read_mode is file_name;
    variable l      : line;
    variable word   : std_logic_vector(31 downto 0);
    variable addr   : integer := 0;
    variable good   : boolean;
    variable ram    : ram_t := (others => x"00");
  begin
    while not endfile(f_in) and addr < C_MEM_SIZE loop
      readline(f_in, l);
      hread(l, word, good);
      if good then
        -- Little Endian loading
        ram(addr)   := word(7  downto  0);
        ram(addr+1) := word(15 downto  8);
        ram(addr+2) := word(23 downto 16);
        ram(addr+3) := word(31 downto 24);
        addr := addr + 4;
      end if;
    end loop;
    report "Loaded " & integer'image(addr) & " bytes from " & file_name;
    return ram;
  end function;

  procedure print_instruction(
    constant addr    : in std_logic_vector;
    constant inst    : in std_logic_vector;
    constant verbose : in boolean
  ) is
  begin
    if verbose then
      log(ID_BFM, "Fetch @ 0x" & to_hstring(addr) & " : 0x" & to_hstring(inst));
    end if;
  end procedure;

end package body tb_WardRV_pkg;