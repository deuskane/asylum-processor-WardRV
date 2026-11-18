-------------------------------------------------------------------------------
-- Title      : WardRV Debug - Debug Module unit test scenarios
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_WardRV_dbg_dm (one per run, TEST generic).
--              The sequencer is both the debugger (DMI requests) and the hart
--              (accesses to the debug memory, as the debug ROM and the
--              abstract command programs do).
--  reset        : after reset : dmcontrol, dmstatus, abstractcs, sbcs,
--                 hartinfo, haltsum0
--  dmcontrol    : dmactive, ackhavereset, ndmreset, dmactive = 0 resets
--                 the DM
--  halt_resume  : haltreq, HALTED, resumereq, RESUMING, resumeack
--  abstract     : programs of GPR / CSR reads and writes, postexec and the
--                 program buffer with its implicit ebreak, data registers
--  cmderr       : not supported, exception, halt / resume, busy, W1C
--  autoexec     : abstractauto on data and program buffer accesses
--  resethaltreq : set / clrresethaltreq, hart reset (havereset, unavail)
--  sba          : System Bus Access 8 / 16 / 32 bits, autoincrement,
--                 readonaddr, readondata, sberror, sbbusyerror
--  progbuf0     : Debug Module without program buffer (PROGBUF_SIZE = 0)
--  postincrement: aarpostincrement (AARPOSTINCREMENT_ENABLE), with
--                 abstractauto
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

package tb_WardRV_dbg_dm_pkg is

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

  procedure test_reset       (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_dmcontrol   (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_halt_resume (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_abstract    (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_cmderr      (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_autoexec    (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_resethaltreq(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_sba         (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_progbuf0    (signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);
  procedure test_postincrement(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_pkg;

package body tb_WardRV_dbg_dm_pkg is

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

  -- Abstract command program executed by the hart
  type words_t is array (0 to DBG_ABSTRACT_SIZE-1) of std_logic_vector(32-1 downto 0);
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

  -----------------------------------------------------------------------------
  -- reset
  -----------------------------------------------------------------------------
  procedure test_reset(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_check(clk, ini, tgt, DM_DMCONTROL , x"00000000", "dmcontrol : dmactive = 0 after reset");
    dmi_write(clk, ini, tgt, DM_DMCONTROL , x"00000001");          -- dmactive
    dmi_check(clk, ini, tgt, DM_DMSTATUS  , x"004C0CA3", "dmstatus : impebreak, havereset, running, authenticated, hasresethaltreq, version 3");
    dmi_check(clk, ini, tgt, DM_ABSTRACTCS, x"02000002", "abstractcs : progbufsize 2, not busy, cmderr 0, datacount 2");
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20040407", "sbcs : sbversion 1, sbaccess 32, sbasize 32, sbaccess 8 / 16 / 32");
    dmi_check(clk, ini, tgt, DM_HARTINFO  , x"00112B00", "hartinfo : nscratch 1, dataaccess, datasize 2, dataaddr 0xB00");
    dmi_check(clk, ini, tgt, DM_HALTSUM0  , x"00000000", "haltsum0 : running");
    check_value(tgt.hart_req.haltreq, '0', ERROR, "no haltreq to the hart", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- dmcontrol
  -----------------------------------------------------------------------------
  procedure test_dmcontrol(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    log(ID_LOG_HDR, "dmactive = 0 : DM in reset", C_TB_SCOPE);
    dmi_check(clk, ini, tgt, DM_DMCONTROL, x"00000000", "dmcontrol after reset");
    dmi_write(clk, ini, tgt, DM_DATA0    , x"0000CAFE");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000003");          -- ndmreset ignored
    dmi_check(clk, ini, tgt, DM_DMCONTROL, x"00000001", "dmactive only");
    dmi_check(clk, ini, tgt, DM_DATA0    , x"00000000", "data0 not written while dmactive = 0");

    log(ID_LOG_HDR, "ackhavereset", C_TB_SCOPE);
    dmi_check(clk, ini, tgt, DM_DMSTATUS  , x"004C0CA3", "dmstatus : havereset");
    dmi_write(clk, ini, tgt, DM_DMCONTROL , x"10000001");         -- ackhavereset
    dmi_check(clk, ini, tgt, DM_DMSTATUS  , x"00400CA3", "dmstatus : havereset acknowledged");

    log(ID_LOG_HDR, "ndmreset", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000003");
    dmi_check(clk, ini, tgt, DM_DMCONTROL, x"00000003", "dmcontrol.ndmreset");
    check_value(tgt.ndmreset, '1', ERROR, "ndmreset_o", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    check_value(tgt.ndmreset, '0', ERROR, "ndmreset_o released", C_TB_SCOPE);

    log(ID_LOG_HDR, "dmactive = 0 resets the DM", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DATA0    , x"0000CAFE");
    dmi_check(clk, ini, tgt, DM_DATA0    , x"0000CAFE", "data0");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"80000001");          -- haltreq
    check_value(tgt.hart_req.haltreq, '1', ERROR, "haltreq", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000000");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    dmi_check(clk, ini, tgt, DM_DATA0    , x"00000000", "data0 reset by dmactive = 0");
    check_value(tgt.hart_req.haltreq, '0', ERROR, "haltreq reset by dmactive = 0", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- halt_resume
  -----------------------------------------------------------------------------
  procedure test_halt_resume(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");          -- dmactive
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"10000001");          -- ackhavereset

    log(ID_LOG_HDR, "Halt", C_TB_SCOPE);
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"80000001");
    check_value(tgt.hart_req.haltreq, '1', ERROR, "haltreq to the hart", C_TB_SCOPE);
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"00400CA3", "running until HALTED");
    hart_write(clk, ini, tgt, DBG_HALTED);
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"004003A3", "allhalted");
    dmi_check (clk, ini, tgt, DM_HALTSUM0 , x"00000001", "haltsum0");
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"00000001");
    check_value(tgt.hart_req.haltreq, '0', ERROR, "haltreq cleared", C_TB_SCOPE);
    hart_check(clk, ini, tgt, DBG_GO    , x"00000000", "GO");
    hart_check(clk, ini, tgt, DBG_RESUME, x"00000000", "RESUME");

    log(ID_LOG_HDR, "Resume", C_TB_SCOPE);
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"C0000001");          -- resumereq with haltreq : ignored
    hart_check(clk, ini, tgt, DBG_RESUME  , x"00000000", "resumereq ignored with haltreq");
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"40000001");
    hart_check(clk, ini, tgt, DBG_RESUME  , x"00000001", "RESUME");
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"004003A3", "halted until RESUMING, no resumeack");
    hart_write(clk, ini, tgt, DBG_RESUMING);
    hart_check(clk, ini, tgt, DBG_RESUME  , x"00000000", "RESUME cleared");
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"00430CA3", "running, allresumeack");
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"40000001");          -- running : ignored
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"00430CA3", "resumereq ignored while running");
  end procedure;

  -----------------------------------------------------------------------------
  -- abstract
  -----------------------------------------------------------------------------
  procedure test_abstract(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    activate_and_halt(clk, ini, tgt);

    log(ID_LOG_HDR, "GPR read : x5", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001005");
    dmi_check    (clk, ini, tgt, DM_ABSTRACTCS, x"02001002", "abstractcs.busy");
    check_program(clk, ini, tgt, (SW_X5_DATA0, NOP, NOP, NOP, EBREAK), "read x5");
    hart_write   (clk, ini, tgt, DBG_DATA, x"55555555");
    end_command  (clk, ini, tgt, CMDERR_NONE, "read x5");
    dmi_check    (clk, ini, tgt, DM_DATA0, x"55555555", "data0 written by the hart");

    log(ID_LOG_HDR, "GPR write : x31", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_DATA0  , x"12345678");
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"0000101F");
    check_program(clk, ini, tgt, (LW_X31_DATA0, NOP, NOP, NOP, EBREAK), "write x31");
    hart_check   (clk, ini, tgt, DBG_DATA, x"12345678", "data0 read by the hart");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x31");

    log(ID_LOG_HDR, "CSR read : mhartid, CSR write : mscratch", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_READ  or x"00000F14");
    check_program(clk, ini, tgt, (CSRW_DSCRATCH0, CSRR_MHARTID, SW_S0_DATA0, CSRR_DSCRATCH0, EBREAK), "read mhartid");
    end_command  (clk, ini, tgt, CMDERR_NONE, "read mhartid");
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"00000340");
    check_program(clk, ini, tgt, (CSRW_DSCRATCH0, LW_S0_DATA0, CSRW_MSCRATCH, CSRR_DSCRATCH0, EBREAK), "write mscratch");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write mscratch");

    log(ID_LOG_HDR, "postexec : program buffer and implicit ebreak, data1", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_PROGBUF0, LW_S1_S0);
    dmi_write    (clk, ini, tgt, DM_PROGBUF1, SW_S1_S0);
    dmi_check    (clk, ini, tgt, DM_PROGBUF1, SW_S1_S0, "progbuf1");
    dmi_write    (clk, ini, tgt, DM_COMMAND , CMD_POSTEXEC);
    check_program(clk, ini, tgt, (NOP, NOP, NOP, NOP, NOP), "postexec");
    hart_check   (clk, ini, tgt, DBG_PROGBUF    , LW_S1_S0, "progbuf0 after the program");
    hart_check   (clk, ini, tgt, DBG_PROGBUF + 4, SW_S1_S0, "progbuf1");
    hart_check   (clk, ini, tgt, DBG_PROGBUF + 8, EBREAK  , "implicit ebreak");
    hart_write   (clk, ini, tgt, DBG_DATA + 4, x"0000DA7A");
    end_command  (clk, ini, tgt, CMDERR_NONE, "postexec");
    dmi_check    (clk, ini, tgt, DM_DATA1, x"0000DA7A", "data1 written by the hart");
  end procedure;

  -----------------------------------------------------------------------------
  -- cmderr
  -----------------------------------------------------------------------------
  procedure test_cmderr(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    activate_and_halt(clk, ini, tgt);

    log(ID_LOG_HDR, "Not supported, exception", C_TB_SCOPE);
    dmi_write   (clk, ini, tgt, DM_COMMAND, x"01000000");                  -- Quick Access
    dmi_write   (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001005");      -- Ignored : cmderr /= 0
    hart_check  (clk, ini, tgt, DBG_GO, x"00000000", "No command while cmderr /= 0");
    check_cmderr(clk, ini, tgt, CMDERR_NOTSUP   , "cmdtype 1");
    dmi_write   (clk, ini, tgt, DM_COMMAND, x"00321005");                  -- aarsize 3
    check_cmderr(clk, ini, tgt, CMDERR_NOTSUP   , "aarsize 3");
    dmi_write   (clk, ini, tgt, DM_COMMAND, x"002A1005");                  -- aarpostincrement
    check_cmderr(clk, ini, tgt, CMDERR_NOTSUP   , "aarpostincrement");
    dmi_write   (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001020");      -- f0
    check_cmderr(clk, ini, tgt, CMDERR_EXCEPTION, "regno 0x1020");
    check_cmderr(clk, ini, tgt, CMDERR_NONE     , "cleared");
    hart_check  (clk, ini, tgt, DBG_GO, x"00000000", "No command started");

    log(ID_LOG_HDR, "Busy", C_TB_SCOPE);
    dmi_write   (clk, ini, tgt, DM_DATA0  , x"11111111");
    dmi_write   (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"00001005");
    dmi_write   (clk, ini, tgt, DM_DATA0  , x"22222222");                  -- During the command
    dmi_check   (clk, ini, tgt, DM_DATA0  , x"11111111", "data0 not written during the command");
    check_program(clk, ini, tgt, (x"B0002283", NOP, NOP, NOP, EBREAK), "write x5"); -- lw x5, data0(zero)
    end_command (clk, ini, tgt, CMDERR_BUSY, "busy");
    check_cmderr(clk, ini, tgt, CMDERR_BUSY, "busy");

    log(ID_LOG_HDR, "Hart not halted", C_TB_SCOPE);
    dmi_write   (clk, ini, tgt, DM_DMCONTROL, x"40000001");
    hart_write  (clk, ini, tgt, DBG_RESUMING);
    dmi_write   (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001005");
    check_cmderr(clk, ini, tgt, CMDERR_HALTRESUME, "running");
  end procedure;

  -----------------------------------------------------------------------------
  -- autoexec
  -----------------------------------------------------------------------------
  procedure test_autoexec(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    activate_and_halt(clk, ini, tgt);
    dmi_write  (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001005");
    hart_write (clk, ini, tgt, DBG_DATA, x"00000001");
    end_command(clk, ini, tgt, CMDERR_NONE, "read x5");

    dmi_write(clk, ini, tgt, DM_ABSTRACTAUTO, x"FFFFFFFF");
    dmi_check(clk, ini, tgt, DM_ABSTRACTAUTO, x"00030003", "abstractauto : 2 progbuf, 2 data");
    dmi_write(clk, ini, tgt, DM_ABSTRACTAUTO, x"00010001");

    for i in 0 to 2 loop
      log(ID_LOG_HDR, "autoexec " & integer'image(i), C_TB_SCOPE);
      case i is
        when 0      => dmi_access(clk, ini, tgt, '0', DM_DATA0   , x"00000000", v_data);
                       check_value(v_data, x"00000001", ERROR, "data0 read before the command", C_TB_SCOPE);
        when 1      => dmi_write (clk, ini, tgt, DM_DATA0   , x"00000002");
        when others => dmi_write (clk, ini, tgt, DM_PROGBUF0, NOP);
      end case;
      check_program(clk, ini, tgt, (SW_X5_DATA0, NOP, NOP, NOP, EBREAK), "autoexec read x5");
      hart_write   (clk, ini, tgt, DBG_DATA, std_logic_vector(to_unsigned(16+i, 32)));
      end_command  (clk, ini, tgt, CMDERR_NONE, "autoexec");
    end loop;

    log(ID_LOG_HDR, "No autoexec", C_TB_SCOPE);
    dmi_write (clk, ini, tgt, DM_DATA1       , x"00000003");
    dmi_write (clk, ini, tgt, DM_PROGBUF1    , NOP);
    hart_check(clk, ini, tgt, DBG_GO, x"00000000", "no command started by data1 / progbuf1");
    dmi_write (clk, ini, tgt, DM_ABSTRACTAUTO, x"00000000");
    dmi_check (clk, ini, tgt, DM_DATA0       , x"00000012", "data0 of the last command");
    dmi_write (clk, ini, tgt, DM_DATA0       , x"00000004");
    hart_check(clk, ini, tgt, DBG_GO, x"00000000", "no command started without abstractauto");
  end procedure;

  -----------------------------------------------------------------------------
  -- resethaltreq
  -----------------------------------------------------------------------------
  procedure test_resethaltreq(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"10000001");

    log(ID_LOG_HDR, "setresethaltreq / clrresethaltreq", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000009");
    check_value(tgt.hart_req.resethaltreq, '1', ERROR, "resethaltreq", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    check_value(tgt.hart_req.resethaltreq, '1', ERROR, "resethaltreq kept", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000005");
    check_value(tgt.hart_req.resethaltreq, '0', ERROR, "resethaltreq cleared", C_TB_SCOPE);

    log(ID_LOG_HDR, "Hart reset : unavail, havereset, leaves debug mode", C_TB_SCOPE);
    hart_write(clk, ini, tgt, DBG_HALTED);
    dmi_check (clk, ini, tgt, DM_DMSTATUS, x"004003A3", "halted");
    ini.hart_rst_b <= '0';
    for i in 1 to 4 loop
      wait until rising_edge(clk);
    end loop;
    dmi_check (clk, ini, tgt, DM_DMSTATUS, x"004C30A3", "unavail, havereset");
    ini.hart_rst_b <= '1';
    for i in 1 to 4 loop
      wait until rising_edge(clk);
    end loop;
    dmi_check (clk, ini, tgt, DM_DMSTATUS, x"004C0CA3", "running after the reset");
  end procedure;

  -----------------------------------------------------------------------------
  -- sba
  -----------------------------------------------------------------------------
  procedure test_sba(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is

    procedure sba_wait is
      variable v_sbcs : std_logic_vector(32-1 downto 0);
    begin
      for i in 1 to 20 loop
        dmi_access(clk, ini, tgt, '0', DM_SBCS, x"00000000", v_sbcs);
        exit when v_sbcs(21) = '0';
      end loop;
      check_value(v_sbcs(21), '0', ERROR, "sbcs.sbbusy", C_TB_SCOPE);
    end procedure;

  begin
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");

    log(ID_LOG_HDR, "32-bit write, read on address", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");
    dmi_write(clk, ini, tgt, DM_SBDATA0   , x"A1B2C3D4");
    sba_wait;
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00140000");          -- sbreadonaddr, 32 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBDATA0   , x"A1B2C3D4", "32-bit read");

    log(ID_LOG_HDR, "8 and 16-bit accesses", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00000000");          -- 8 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000011");
    dmi_write(clk, ini, tgt, DM_SBDATA0   , x"0000005A");
    sba_wait;
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00120000");          -- sbreadonaddr, 16 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000012");
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBDATA0   , x"0000A1B2", "16-bit read");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00140000");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBDATA0   , x"A1B25AD4", "8-bit write");

    log(ID_LOG_HDR, "Autoincrement, read on data", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00050000");          -- autoincrement, 32 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000020");
    for i in 0 to 3 loop
      dmi_write(clk, ini, tgt, DM_SBDATA0 , std_logic_vector(to_unsigned(16#100# + i, 32)));
      sba_wait;
    end loop;
    dmi_check(clk, ini, tgt, DM_SBADDRESS0, x"00000030", "sbaddress0 incremented");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00158000");          -- readonaddr, autoincrement, readondata
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000020");
    for i in 0 to 3 loop
      sba_wait;
      dmi_check(clk, ini, tgt, DM_SBDATA0 , std_logic_vector(to_unsigned(16#100# + i, 32)), "read on data " & integer'image(i));
    end loop;

    log(ID_LOG_HDR, "sberror, sbbusyerror", C_TB_SCOPE);
    sba_wait;
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00140000");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000012");          -- Misaligned
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20143407", "sberror 3");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");          -- No access while sberror
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20143407", "no access while sberror is set");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00067000");          -- W1C, sbaccess 64
    dmi_write(clk, ini, tgt, DM_SBDATA0   , x"00000000");
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20064407", "sberror 4");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00147000");          -- W1C, readonaddr, 32 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000020");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000024");          -- During the access
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20540407", "sbbusyerror");
    dmi_check(clk, ini, tgt, DM_SBADDRESS0, x"00000020", "sbaddress0 not written during the access");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00440000");          -- W1C, 32 bits
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20040407", "sbbusyerror cleared");
  end procedure;

  -----------------------------------------------------------------------------
  -- progbuf0 (PROGBUF_SIZE = 0)
  -----------------------------------------------------------------------------
  procedure test_progbuf0(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    activate_and_halt(clk, ini, tgt);
    dmi_check(clk, ini, tgt, DM_ABSTRACTCS  , x"00000002", "abstractcs : progbufsize 0");
    dmi_check(clk, ini, tgt, DM_DMSTATUS    , x"004C03A3", "dmstatus : impebreak also without program buffer");
    dmi_write(clk, ini, tgt, DM_PROGBUF0    , NOP);
    dmi_check(clk, ini, tgt, DM_PROGBUF0    , x"00000000", "no progbuf0");
    dmi_write(clk, ini, tgt, DM_ABSTRACTAUTO, x"FFFFFFFF");
    dmi_check(clk, ini, tgt, DM_ABSTRACTAUTO, x"00000003", "no autoexecprogbuf");
    dmi_write(clk, ini, tgt, DM_ABSTRACTAUTO, x"00000000");

    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_READ or CMD_POSTEXEC or x"00001005");
    check_cmderr (clk, ini, tgt, CMDERR_NOTSUP, "postexec without program buffer");
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00000F14");
    check_program(clk, ini, tgt, (CSRW_DSCRATCH0, CSRR_MHARTID, SW_S0_DATA0, CSRR_DSCRATCH0, EBREAK), "read mhartid");
    hart_write   (clk, ini, tgt, DBG_DATA, x"900DC0DE");
    end_command  (clk, ini, tgt, CMDERR_NONE, "read mhartid");
    dmi_check    (clk, ini, tgt, DM_DATA0, x"900DC0DE", "data0");
  end procedure;

  -----------------------------------------------------------------------------
  -- postincrement (AARPOSTINCREMENT_ENABLE)
  -----------------------------------------------------------------------------
  procedure test_postincrement(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
    constant LW_X10_DATA0 : std_logic_vector(32-1 downto 0) := x"B0002503";
    constant LW_X11_DATA0 : std_logic_vector(32-1 downto 0) := x"B0002583";
    constant LW_X12_DATA0 : std_logic_vector(32-1 downto 0) := x"B0002603";
  begin
    activate_and_halt(clk, ini, tgt);

    log(ID_LOG_HDR, "Write x10, x11, x12 : aarpostincrement and abstractauto", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_DATA0  , x"0000000A");
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"0008100A");  -- aarpostincrement
    check_program(clk, ini, tgt, (LW_X10_DATA0, NOP, NOP, NOP, EBREAK), "write x10");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x10");
    dmi_write    (clk, ini, tgt, DM_ABSTRACTAUTO, x"00000001");
    dmi_write    (clk, ini, tgt, DM_DATA0  , x"0000000B");
    check_program(clk, ini, tgt, (LW_X11_DATA0, NOP, NOP, NOP, EBREAK), "autoexec : x11");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x11");
    dmi_write    (clk, ini, tgt, DM_DATA0  , x"0000000C");
    check_program(clk, ini, tgt, (LW_X12_DATA0, NOP, NOP, NOP, EBREAK), "autoexec : x12");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x12");
    dmi_write    (clk, ini, tgt, DM_ABSTRACTAUTO, x"00000000");

    log(ID_LOG_HDR, "Without aarpostincrement : same register", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"0000100A");
    check_program(clk, ini, tgt, (LW_X10_DATA0, NOP, NOP, NOP, EBREAK), "write x10");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x10");
    dmi_write    (clk, ini, tgt, DM_ABSTRACTAUTO, x"00000001");
    dmi_write    (clk, ini, tgt, DM_DATA0  , x"0000000D");
    check_program(clk, ini, tgt, (LW_X10_DATA0, NOP, NOP, NOP, EBREAK), "autoexec : x10 again");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x10");
  end procedure;

end package body tb_WardRV_dbg_dm_pkg;
