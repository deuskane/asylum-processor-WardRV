-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_bfm_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_bfm_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Types, constants and helpers shared by the scenarios of
--              tb_WardRV_dbg_dm : the sequencer is both the debugger (DMI
--              requests) and the hart (accesses to the debug memory, as the
--              debug ROM and the abstract command programs do)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_dbg_dm_pkg.vhd)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

package tb_WardRV_dbg_dm_bfm_pkg is

  -- Driven by the sequencer
  type dm_ini_t is record
    dmi_req    : dmi_req_t;
    mem_ini    : dmem_ini_t;                                      -- Hart : debug memory
    hart_rst_b : std_logic;
  end record dm_ini_t;

  -- Observed by the sequencer
  type dm_tgt_t is record
    dmi_rdata  : std_logic_vector(32-1 downto 0);
    mem_tgt    : dmem_tgt_t;
    hart_req   : dbg_hart_req_t;
    ndmreset   : std_logic;
  end record dm_tgt_t;

  constant DM_INI_IDLE : dm_ini_t := (dmi_req    => (valid => '0', we => '0', addr => (others => '0'), wdata => (others => '0')),
                                      mem_ini    => (valid => '0', addr => (others => '0'), wdata => (others => '0'), we => '0', be => "1111"),
                                      hart_rst_b => '1');

  constant C_TB_SCOPE : string := "tb_WardRV_dbg_dm";

  -- Instructions (GNU as) : data0 is at -1280(zero)
  constant SW_X5_DATA0      : std_logic_vector(32-1 downto 0) := x"B0502023";
  constant LW_X31_DATA0     : std_logic_vector(32-1 downto 0) := x"B0002F83";
  constant CSRW_DSCRATCH0   : std_logic_vector(32-1 downto 0) := x"7B241073";  -- csrw dscratch0, s0
  constant CSRR_MHARTID     : std_logic_vector(32-1 downto 0) := x"F1402473";  -- csrr s0, mhartid
  constant SW_S0_DATA0      : std_logic_vector(32-1 downto 0) := x"B0802023";
  constant CSRR_DSCRATCH0   : std_logic_vector(32-1 downto 0) := x"7B202473";  -- csrr s0, dscratch0
  constant LW_S0_DATA0      : std_logic_vector(32-1 downto 0) := x"B0002403";
  constant CSRW_MSCRATCH    : std_logic_vector(32-1 downto 0) := x"34041073";  -- csrw mscratch, s0
  constant EBREAK           : std_logic_vector(32-1 downto 0) := x"00100073";
  constant NOP              : std_logic_vector(32-1 downto 0) := x"00000013";
  constant LW_S1_S0         : std_logic_vector(32-1 downto 0) := x"00042483";
  constant SW_S1_S0         : std_logic_vector(32-1 downto 0) := x"00942023";

  -- Commands (Access Register, aarsize 2)
  constant CMD_READ         : std_logic_vector(32-1 downto 0) := x"00220000";  -- transfer
  constant CMD_WRITE        : std_logic_vector(32-1 downto 0) := x"00230000";  -- transfer, write
  constant CMD_POSTEXEC     : std_logic_vector(32-1 downto 0) := x"00040000";
  constant DM_PROGBUF1      : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"21";
  constant DM_DATA1         : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"05";

  -- BFM : DMI request and hart access, one clock cycle each
  procedure dmi_access(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                       we : std_logic; addr : std_logic_vector; wdata : std_logic_vector; variable rdata : out std_logic_vector);

  procedure dmi_write(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                      addr : std_logic_vector; data : std_logic_vector);

  -- Read and check
  procedure dmi_check(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                      addr : std_logic_vector; exp : std_logic_vector; msg : string);

  procedure hart_access(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                        we : std_logic; offset : natural; wdata : std_logic_vector; variable rdata : out std_logic_vector);

  procedure hart_write(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                       offset : natural; data : std_logic_vector := x"00000000");

  procedure hart_check(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                       offset : natural; exp : std_logic_vector; msg : string);

  -- dmactive, then the hart halts (HALTED written by the debug ROM)
  procedure activate_and_halt(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

  -- Abstract command program executed by the hart
  type words_t is array (0 to DBG_ABSTRACT_SIZE-1) of std_logic_vector(32-1 downto 0);
  procedure check_program(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                          exp : words_t; msg : string);

  -- Command completed : the hart is back in the ROM
  procedure end_command(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                        cmderr : std_logic_vector(2 downto 0); msg : string);

  procedure check_cmderr(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                         cmderr : std_logic_vector(2 downto 0); msg : string);

end package tb_WardRV_dbg_dm_bfm_pkg;

package body tb_WardRV_dbg_dm_bfm_pkg is

  -----------------------------------------------------------------------------
  -- BFM : DMI request and hart access, one clock cycle each
  -----------------------------------------------------------------------------
  procedure dmi_access(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                       we : std_logic; addr : std_logic_vector; wdata : std_logic_vector; variable rdata : out std_logic_vector) is
  begin
    ini.dmi_req <= (valid => '1', we => we, addr => addr, wdata => wdata);
    wait until rising_edge(clk);
    rdata       := tgt.dmi_rdata;
    ini.dmi_req.valid <= '0';
    wait for 1 ns;                                                -- Registers updated
  end procedure;

  procedure dmi_write(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                      addr : std_logic_vector; data : std_logic_vector) is
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    dmi_access(clk, ini, tgt, '1', addr, data, v_rdata);
  end procedure;

  -- Read and check
  procedure dmi_check(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                      addr : std_logic_vector; exp : std_logic_vector; msg : string) is
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    dmi_access(clk, ini, tgt, '0', addr, x"00000000", v_rdata);
    check_value(v_rdata, exp, ERROR, msg, C_TB_SCOPE);
  end procedure;

  procedure hart_access(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                        we : std_logic; offset : natural; wdata : std_logic_vector; variable rdata : out std_logic_vector) is
  begin
    ini.mem_ini <= (valid => '1', addr => std_logic_vector(unsigned(DBG_MEM_BASE) + offset), wdata => wdata, we => we, be => "1111");
    wait until rising_edge(clk);
    check_value(tgt.mem_tgt.ready, '1', ERROR, "Debug memory without wait state", C_TB_SCOPE);
    rdata       := tgt.mem_tgt.rdata;
    ini.mem_ini.valid <= '0';
    wait for 1 ns;
  end procedure;

  procedure hart_write(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                       offset : natural; data : std_logic_vector := x"00000000") is
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    hart_access(clk, ini, tgt, '1', offset, data, v_rdata);
  end procedure;

  procedure hart_check(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                       offset : natural; exp : std_logic_vector; msg : string) is
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    hart_access(clk, ini, tgt, '0', offset, x"00000000", v_rdata);
    check_value(v_rdata, exp, ERROR, msg, C_TB_SCOPE);
  end procedure;

  -- dmactive, then the hart halts (HALTED written by the debug ROM)
  procedure activate_and_halt(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"00000001");
    hart_write(clk, ini, tgt, DBG_HALTED);
  end procedure;

  procedure check_program(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                          exp : words_t; msg : string) is
  begin
    hart_check(clk, ini, tgt, DBG_GO, x"00000001", "GO " & msg);
    for i in exp'range loop
      hart_check(clk, ini, tgt, DBG_ABSTRACT + 4*i, exp(i), msg & " : word " & integer'image(i));
    end loop;
  end procedure;

  -- Command completed : the hart is back in the ROM
  procedure end_command(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                        cmderr : std_logic_vector(2 downto 0); msg : string) is
    variable v_cs : std_logic_vector(32-1 downto 0);
  begin
    hart_write(clk, ini, tgt, DBG_HALTED);
    hart_check(clk, ini, tgt, DBG_GO, x"00000000", "GO cleared " & msg);
    dmi_access(clk, ini, tgt, '0', DM_ABSTRACTCS, x"00000000", v_cs);
    check_value(v_cs(12)         , '0'   , ERROR, "abstractcs.busy "   & msg, C_TB_SCOPE);
    check_value(v_cs(10 downto 8), cmderr, ERROR, "abstractcs.cmderr " & msg, C_TB_SCOPE);
  end procedure;

  procedure check_cmderr(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t;
                         cmderr : std_logic_vector(2 downto 0); msg : string) is
    variable v_cs : std_logic_vector(32-1 downto 0);
  begin
    dmi_access(clk, ini, tgt, '0', DM_ABSTRACTCS, x"00000000", v_cs);
    check_value(v_cs(10 downto 8), cmderr, ERROR, "abstractcs.cmderr " & msg, C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_ABSTRACTCS, x"00000700");         -- W1C
  end procedure;

end package body tb_WardRV_dbg_dm_bfm_pkg;
