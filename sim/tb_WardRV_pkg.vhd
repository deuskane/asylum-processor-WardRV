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
  constant C_CLK_PERIOD     : time := 10 ns;
  constant C_TOHOST_ADDR    : std_logic_vector(31 downto 0) := x"80001000";
  constant C_FROMHOST_ADDR  : std_logic_vector(31 downto 0) := x"80001100";
  constant C_SIGNATURE_ADDR : std_logic_vector(31 downto 0) := x"00010010";
  constant C_MEM_SIZE       : integer := 131072; -- 128KB

  -- Memory Type
  type ram_t is array (0 to C_MEM_SIZE-1) of std_logic_vector(7 downto 0);

  -- Helper to read Hex
  impure function init_ram(file_name : string) return ram_t;

  -- Helper to dump memory signature
  procedure dump_signature (
    constant file_name : in string;
    constant start_addr : in std_logic_vector(31 downto 0);
    constant size       : in integer;
    signal   mem        : in ram_t
  );

  -- Helper to print instruction
  procedure print_instruction(
    constant addr    : in std_logic_vector;
    constant inst    : in std_logic_vector;
    constant verbose : in boolean
  );

  -- Helper to compare signature with golden file
  procedure compare_signature (
    constant sig_file    : in string;
    constant golden_file : in string
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

  -- Dump signature at the end of simulation
  procedure dump_signature (
    constant file_name : in string;
    constant start_addr : in std_logic_vector(31 downto 0);
    constant size       : in integer;
    signal   mem        : in ram_t
  ) is
    file f_sig          : text;
    variable l          : line;
    variable v_sig_addr : integer;
    variable v_wdata    : std_logic_vector(31 downto 0);
  begin
    log(ID_LOG_HDR, "ISS: Dump Signature to " & file_name);

    file_open(f_sig, file_name, write_mode);
    v_sig_addr := to_integer(unsigned(start_addr));
    for i in 0 to (size/4)-1 loop
      exit when v_sig_addr > C_MEM_SIZE - 4;
      v_wdata := mem(v_sig_addr+3) & mem(v_sig_addr+2) & mem(v_sig_addr+1) & mem(v_sig_addr);
      write(l, to_hstring(v_wdata));
      writeline(f_sig, l);
      v_sig_addr := v_sig_addr + 4;
    end loop;
    file_close(f_sig);
  end procedure;

  -- Compare signature against golden reference
  procedure compare_signature (
    constant sig_file    : in string;
    constant golden_file : in string
  ) is
    file f_sig          : text;
    file f_gold         : text;
    variable l_sig      : line;
    variable l_gold     : line;
    variable v_sig      : std_logic_vector(31 downto 0);
    variable v_gold     : std_logic_vector(31 downto 0);
    variable v_good_sig : boolean;
    variable v_good_gold: boolean;
    variable v_line_cnt : integer := 0;
    variable v_errors   : integer := 0;
  begin
    log(ID_LOG_HDR, "Comparing " & sig_file & " with " & golden_file);
    file_open(f_sig,  sig_file,    read_mode);
    file_open(f_gold, golden_file, read_mode);

    while not endfile(f_gold) loop
      v_line_cnt := v_line_cnt + 1;
      readline(f_sig,  l_sig);
      readline(f_gold, l_gold);
      hread(l_sig,  v_sig,  v_good_sig);
      hread(l_gold, v_gold, v_good_gold);

      if v_sig /= v_gold then
        v_errors := v_errors + 1;
        alert(TB_ERROR, "Signature mismatch at line " & integer'image(v_line_cnt) & 
              ": Expected 0x" & to_hstring(v_gold) & ", Got 0x" & to_hstring(v_sig));
      end if;
    end loop;

    if v_errors = 0 then
      log(ID_LOG_HDR, "Signature comparison SUCCESSFUL (" & integer'image(v_line_cnt) & " lines checked)");
    end if;

    file_close(f_sig);
    file_close(f_gold);
  end procedure;

end package body tb_WardRV_pkg;
