-------------------------------------------------------------------------------
-- Title      : WardRV Debug - System test scenarios
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_WardRV_debug (one per run, TEST generic) :
--              the sequencer is a debugger on the JTAG port, the core runs
--              esw/debug/debug_fw.S. Debugger procedures (halt, resume,
--              register and memory access) follow the OpenOCD flow.
--  reset           : after reset, the core runs its firmware (allrunning,
--                    counter in memory), Debug Module idle
--  halt_resume     : haltreq (cause 3, dpc in the loop), resume
--  registers       : GPR / CSR access with abstract commands, dpc write
--  cmderr          : register that does not exist, command while running
--  progbuf         : memory read / write through the program buffer
--  step            : dcsr.step (cause 4, dpc of each instruction)
--  ebreak          : ebreak with dcsr.ebreakm (cause 1), NOP without
--  trigger_execute : tselect / tinfo / tdata1 WARL (no range, no chain),
--                    execute trigger
--  trigger_store   : store trigger (cause 2, dpc of the store)
--  trigger_load    : load trigger (cause 2, dpc of the load)
--  resethaltreq    : ndmreset with resethaltreq (cause 5) and haltreq (3)
--  interrupt       : interrupt pending in debug mode, step with and
--                    without dcsr.stepie
-- TRIGGER_RANGE_ENABLE = true :
--  trigger_ge          : execute trigger, match >= (fires on pc = tdata2)
--  trigger_lt          : execute trigger, match < (not on pc = tdata2)
--  trigger_range_execute : chain >= A, < B on the pc, chain WARL, chain
--                    of M-mode triggers (tdata2 written, dmode not set)
--  trigger_range_store : chain >= A, < B on the store address
-- EBREAK_EXCEPTION = true :
--  exc_ebreak      : ebreak without ebreakm traps (mcause 3), with ebreakm
--                    enters debug mode
--  exc_trigger     : M-mode trigger (dmode 0) : breakpoint exception
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
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

package tb_WardRV_debug_pkg is

  constant C_IDCODE     : std_logic_vector(32-1 downto 0) := x"1DEB0001";
  constant C_NB_TRIGGERS: natural := 2;

  -- Firmware : esw/debug/debug_fw.lst
  constant C_LOOP       : std_logic_vector(32-1 downto 0) := x"80000034";  -- addi t0, t0, 1
  constant C_SW_COUNTER : std_logic_vector(32-1 downto 0) := x"80000038";  -- sw   t0, 0(a0)
  constant C_LW_FLAG    : std_logic_vector(32-1 downto 0) := x"8000003C";  -- lw   s0, 4(a0)
  constant C_BEQZ       : std_logic_vector(32-1 downto 0) := x"80000040";  -- beqz s0, loop
  constant C_EBREAK     : std_logic_vector(32-1 downto 0) := x"80000044";
  constant C_SW_FLAG    : std_logic_vector(32-1 downto 0) := x"8000004C";  -- sw   zero, 4(a0)
  constant C_HANDLER    : std_logic_vector(32-1 downto 0) := x"80000054";
  constant C_COUNTER    : std_logic_vector(32-1 downto 0) := x"80001000";
  constant C_FLAG       : std_logic_vector(32-1 downto 0) := x"80001004";

  procedure test_reset          (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; signal mem : in ram_t);
  procedure test_halt_resume    (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_registers      (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_cmderr         (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_progbuf        (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_step           (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_ebreak         (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_execute(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_store  (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_load   (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_ge     (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_lt     (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_range_execute(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_trigger_range_store  (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_resethaltreq   (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_interrupt      (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_exc_ebreak     (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);
  procedure test_exc_trigger    (signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_pkg;

package body tb_WardRV_debug_pkg is

  constant C_TB_SCOPE   : string := "tb_WardRV_debug";

  -- Registers
  constant X0           : natural := 16#1000#;
  constant T0           : natural := 16#1005#;
  constant S0           : natural := 16#1008#;
  constant S1           : natural := 16#1009#;
  constant S2           : natural := 16#1012#;
  constant S3           : natural := 16#1013#;
  constant A4           : natural := 16#100E#;
  constant A5           : natural := 16#100F#;
  constant MSTATUS      : natural := 16#300#;
  constant MSCRATCH     : natural := 16#340#;
  constant MEPC         : natural := 16#341#;
  constant MCAUSE       : natural := 16#342#;
  constant MHARTID      : natural := 16#F14#;
  constant TSELECT      : natural := 16#7A0#;
  constant TDATA1       : natural := 16#7A1#;
  constant TDATA2       : natural := 16#7A2#;
  constant TINFO        : natural := 16#7A4#;
  constant DCSR         : natural := 16#7B0#;
  constant DPC          : natural := 16#7B1#;

  -- dcsr written by the debugger : prv = 3
  constant DCSR_NONE    : std_logic_vector(32-1 downto 0) := x"00000003";
  constant DCSR_STEP    : std_logic_vector(32-1 downto 0) := x"00000007";
  constant DCSR_STEPIE  : std_logic_vector(32-1 downto 0) := x"00000807";
  constant DCSR_EBREAKM : std_logic_vector(32-1 downto 0) := x"00008003";

  -- Program buffer : memory access through a4 (address) and a5 (data),
  -- registers not used by the firmware
  constant LW_A5_A4     : std_logic_vector(32-1 downto 0) := x"00072783";  -- lw a5, 0(a4)
  constant SW_A5_A4     : std_logic_vector(32-1 downto 0) := x"00F72023";  -- sw a5, 0(a4)
  constant DM_PROGBUF1  : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"21";

  -----------------------------------------------------------------------------
  -- Debugger
  -----------------------------------------------------------------------------
  procedure dmi_write(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      addr : std_logic_vector; data : std_logic_vector) is
  begin
    vip_jtag_dmi_write(jtag_ini, jtag_tgt, addr, data);
  end procedure;

  procedure dmi_read(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                     addr : std_logic_vector; variable data : out std_logic_vector) is
  begin
    vip_jtag_dmi_read(jtag_ini, jtag_tgt, addr, data);
  end procedure;

  -- dmactive, IDCODE, havereset acknowledged
  procedure connect(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    vip_jtag_reset(jtag_ini);
    vip_jtag_scan (jtag_ini, jtag_tgt, false, x"00000000", v_data);
    check_value(v_data, C_IDCODE, ERROR, "IDCODE", C_TB_SCOPE);
    dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000001");
    dmi_read (jtag_ini, jtag_tgt, DM_DMSTATUS , v_data);
    check_value(v_data(19 downto 18), "11", ERROR, "dmstatus.havereset after the power-on reset", C_TB_SCOPE);
    check_value(v_data(11 downto  8), "1100", ERROR, "dmstatus : running", C_TB_SCOPE);
    dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"10000001");     -- ackhavereset
  end procedure;

  procedure wait_halted(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; msg : string) is
    variable v_status : std_logic_vector(32-1 downto 0);
  begin
    for i in 1 to 50 loop
      dmi_read(jtag_ini, jtag_tgt, DM_DMSTATUS, v_status);
      exit when v_status(9) = '1';
    end loop;
    check_value(v_status(11 downto 8), "0011", ERROR, "dmstatus : halted " & msg, C_TB_SCOPE);
  end procedure;

  procedure halt(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; msg : string := "") is
  begin
    dmi_write  (jtag_ini, jtag_tgt, DM_DMCONTROL, x"80000001");   -- haltreq
    wait_halted(jtag_ini, jtag_tgt, msg);
    dmi_write  (jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000001");
  end procedure;

  procedure resume(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; msg : string := "") is
    variable v_status : std_logic_vector(32-1 downto 0);
  begin
    dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"40000001");     -- resumereq
    for i in 1 to 50 loop
      dmi_read(jtag_ini, jtag_tgt, DM_DMSTATUS, v_status);
      exit when v_status(17) = '1';
    end loop;
    check_value(v_status(17), '1', ERROR, "dmstatus.allresumeack " & msg, C_TB_SCOPE);
  end procedure;

  -- End of the abstract command, cmderr checked and cleared
  procedure wait_command(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                         cmderr : std_logic_vector(2 downto 0); msg : string) is
    variable v_cs : std_logic_vector(32-1 downto 0);
  begin
    for i in 1 to 50 loop
      dmi_read(jtag_ini, jtag_tgt, DM_ABSTRACTCS, v_cs);
      exit when v_cs(12) = '0';
    end loop;
    check_value(v_cs(12)         , '0'   , ERROR, "abstractcs.busy "   & msg, C_TB_SCOPE);
    check_value(v_cs(10 downto 8), cmderr, ERROR, "abstractcs.cmderr " & msg, C_TB_SCOPE);
    if v_cs(10 downto 8) /= CMDERR_NONE
    then
      dmi_write(jtag_ini, jtag_tgt, DM_ABSTRACTCS, x"00000700");
    end if;
  end procedure;

  procedure reg_read(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                     regno : natural; variable data : out std_logic_vector) is
  begin
    dmi_write   (jtag_ini, jtag_tgt, DM_COMMAND, std_logic_vector(to_unsigned(16#00220000# + regno, 32)));
    wait_command(jtag_ini, jtag_tgt, CMDERR_NONE, "read 0x" & to_hstring(to_unsigned(regno, 16)));
    dmi_read    (jtag_ini, jtag_tgt, DM_DATA0, data);
  end procedure;

  procedure reg_check(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      regno : natural; exp : std_logic_vector; msg : string) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    reg_read(jtag_ini, jtag_tgt, regno, v_data);
    check_value(v_data, exp, ERROR, msg, C_TB_SCOPE);
  end procedure;

  procedure reg_write(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      regno : natural; data : std_logic_vector) is
  begin
    dmi_write   (jtag_ini, jtag_tgt, DM_DATA0  , data);
    dmi_write   (jtag_ini, jtag_tgt, DM_COMMAND, std_logic_vector(to_unsigned(16#00230000# + regno, 32)));
    wait_command(jtag_ini, jtag_tgt, CMDERR_NONE, "write 0x" & to_hstring(to_unsigned(regno, 16)));
  end procedure;

  -- Memory : a4 written by the abstract command, then the program buffer
  procedure mem_access(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                       inst : std_logic_vector; addr : std_logic_vector) is
  begin
    dmi_write   (jtag_ini, jtag_tgt, DM_PROGBUF0, inst);
    dmi_write   (jtag_ini, jtag_tgt, DM_PROGBUF1, rv_i_type(1, 0, F3_PRIV, 0, OPC_SYSTEM));   -- ebreak
    dmi_write   (jtag_ini, jtag_tgt, DM_DATA0   , addr);
    dmi_write   (jtag_ini, jtag_tgt, DM_COMMAND , std_logic_vector(to_unsigned(16#00270000# + A4, 32))); -- write a4, postexec
    wait_command(jtag_ini, jtag_tgt, CMDERR_NONE, "program buffer");
  end procedure;

  procedure mem_read(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                     addr : std_logic_vector; variable data : out std_logic_vector) is
  begin
    mem_access(jtag_ini, jtag_tgt, LW_A5_A4, addr);
    reg_read  (jtag_ini, jtag_tgt, A5, data);
  end procedure;

  procedure mem_write(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      addr : std_logic_vector; data : std_logic_vector) is
  begin
    reg_write (jtag_ini, jtag_tgt, A5, data);
    mem_access(jtag_ini, jtag_tgt, SW_A5_A4, addr);
  end procedure;

  procedure check_cause(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                        cause : std_logic_vector(2 downto 0); msg : string) is
    variable v_dcsr : std_logic_vector(32-1 downto 0);
  begin
    reg_read(jtag_ini, jtag_tgt, DCSR, v_dcsr);
    check_value(v_dcsr(31 downto 28), "0100", ERROR, "dcsr.debugver", C_TB_SCOPE);
    check_value(v_dcsr( 1 downto  0), "11"  , ERROR, "dcsr.prv"     , C_TB_SCOPE);
    check_value(v_dcsr( 8 downto  6), cause , ERROR, "dcsr.cause " & msg, C_TB_SCOPE);
  end procedure;

  -- Running for a while, then halted (cause haltreq)
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                duration : time; msg : string) is
  begin
    resume(jtag_ini, jtag_tgt, msg);
    wait for duration;
    halt  (jtag_ini, jtag_tgt, msg);
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_HALTREQ, msg);
  end procedure;

  -- Address of the next instruction of the loop (flag = 0)
  function next_pc(pc : std_logic_vector(32-1 downto 0)) return std_logic_vector is
  begin
    if pc = C_BEQZ
    then
      return C_LOOP;
    end if;
    return std_logic_vector(unsigned(pc) + 4);
  end function;

  -----------------------------------------------------------------------------
  -- reset
  -----------------------------------------------------------------------------
  procedure test_reset(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; signal mem : in ram_t) is
    variable v_data   : std_logic_vector(32-1 downto 0);
    variable v_count  : std_logic_vector(32-1 downto 0);
    variable v_count2 : std_logic_vector(32-1 downto 0);
  begin
    connect    (jtag_ini, jtag_tgt);
    dmi_read   (jtag_ini, jtag_tgt, DM_DMSTATUS  , v_data);
    check_value(v_data, x"00400CA3", ERROR, "dmstatus : impebreak, allrunning, authenticated, hasresethaltreq, version 3", C_TB_SCOPE);
    dmi_read   (jtag_ini, jtag_tgt, DM_ABSTRACTCS, v_data);
    check_value(v_data, x"02000002", ERROR, "abstractcs : progbufsize 2, not busy, cmderr 0, datacount 2", C_TB_SCOPE);
    dmi_read   (jtag_ini, jtag_tgt, DM_HALTSUM0  , v_data);
    check_value(v_data, x"00000000", ERROR, "haltsum0 : running", C_TB_SCOPE);
    -- The firmware loop stores its counter
    read_mem   (mem, C_COUNTER, v_count , false);
    wait for 5 us;
    read_mem   (mem, C_COUNTER, v_count2, false);
    check_value(unsigned(v_count2) > unsigned(v_count), ERROR,
                "the firmware runs : counter 0x" & to_hstring(v_count) & " then 0x" & to_hstring(v_count2), C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- halt_resume
  -----------------------------------------------------------------------------
  procedure test_halt_resume(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_dpc : std_logic_vector(32-1 downto 0);
    variable v_t0  : std_logic_vector(32-1 downto 0);
    variable v_t0b : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    wait for 20 us;
    log(ID_LOG_HDR, "Halt", C_TB_SCOPE);
    halt       (jtag_ini, jtag_tgt);
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_HALTREQ, "haltreq");
    reg_read   (jtag_ini, jtag_tgt, DPC, v_dpc);
    check_value(unsigned(v_dpc) >= unsigned(C_LOOP) and unsigned(v_dpc) <= unsigned(C_BEQZ), ERROR,
                "dpc in the loop : 0x" & to_hstring(v_dpc), C_TB_SCOPE);
    reg_read   (jtag_ini, jtag_tgt, T0, v_t0);
    check_value(unsigned(v_t0) > 10, ERROR, "the counter runs : " & to_hstring(v_t0), C_TB_SCOPE);
    wait for 5 us;
    reg_check  (jtag_ini, jtag_tgt, T0, v_t0, "halted : the counter does not change");

    log(ID_LOG_HDR, "Resume", C_TB_SCOPE);
    run        (jtag_ini, jtag_tgt, 10 us, "resume");
    reg_read   (jtag_ini, jtag_tgt, T0, v_t0b);
    check_value(unsigned(v_t0b) > unsigned(v_t0) + 10, ERROR, "the counter ran after resume", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- registers
  -----------------------------------------------------------------------------
  procedure test_registers(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_t0 : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "GPR", C_TB_SCOPE);
    reg_write(jtag_ini, jtag_tgt, A5, x"A5A5A5A5");
    reg_check(jtag_ini, jtag_tgt, A5, x"A5A5A5A5", "a5");
    reg_write(jtag_ini, jtag_tgt, X0, x"0000FFFF");
    reg_check(jtag_ini, jtag_tgt, X0, x"00000000", "x0 stays 0");
    reg_check(jtag_ini, jtag_tgt, 16#100A#, C_COUNTER, "a0 = address of the counter");

    log(ID_LOG_HDR, "CSR", C_TB_SCOPE);
    reg_check(jtag_ini, jtag_tgt, MHARTID , C_HARTID, "mhartid");
    reg_write(jtag_ini, jtag_tgt, MSCRATCH, x"12345678");
    reg_check(jtag_ini, jtag_tgt, MSCRATCH, x"12345678", "mscratch");
    reg_write(jtag_ini, jtag_tgt, S0      , x"55AA55AA");
    reg_check(jtag_ini, jtag_tgt, DCSR    , x"400000C3", "dcsr : debugver 4, cause haltreq, prv 3");
    reg_check(jtag_ini, jtag_tgt, S0      , x"55AA55AA", "s0 kept by the CSR access");
    reg_write(jtag_ini, jtag_tgt, S0      , x"00000000");

    log(ID_LOG_HDR, "dpc and t0 written, then resume", C_TB_SCOPE);
    reg_write(jtag_ini, jtag_tgt, DPC     , C_LOOP);
    reg_check(jtag_ini, jtag_tgt, DPC     , C_LOOP, "dpc");
    reg_write(jtag_ini, jtag_tgt, T0      , x"00010000");
    run      (jtag_ini, jtag_tgt, 5 us, "after the register writes");
    reg_read (jtag_ini, jtag_tgt, T0, v_t0);
    check_value(unsigned(v_t0) > 16#10000#, ERROR, "the loop goes on from t0 = 0x10000 : 0x" & to_hstring(v_t0), C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- cmderr
  -----------------------------------------------------------------------------
  procedure test_cmderr(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);
    dmi_write   (jtag_ini, jtag_tgt, DM_COMMAND, x"00221020");   -- f0
    wait_command(jtag_ini, jtag_tgt, CMDERR_EXCEPTION, "register that does not exist");
    resume      (jtag_ini, jtag_tgt);
    dmi_write   (jtag_ini, jtag_tgt, DM_COMMAND, x"00221005");
    wait_command(jtag_ini, jtag_tgt, CMDERR_HALTRESUME, "command while running");
  end procedure;

  -----------------------------------------------------------------------------
  -- progbuf
  -----------------------------------------------------------------------------
  procedure test_progbuf(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
    variable v_t0   : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_ini, jtag_tgt);
    halt     (jtag_ini, jtag_tgt);
    mem_write(jtag_ini, jtag_tgt, x"80001100", x"DEADBEEF");
    mem_read (jtag_ini, jtag_tgt, x"80001100", v_data);
    check_value(v_data, x"DEADBEEF", ERROR, "memory written and read through the program buffer", C_TB_SCOPE);
    mem_read (jtag_ini, jtag_tgt, C_FIRMWARE_ADDR, v_data);
    check_value(v_data, x"00000317", ERROR, "first instruction of the firmware", C_TB_SCOPE);
    reg_read (jtag_ini, jtag_tgt, T0, v_t0);
    mem_read (jtag_ini, jtag_tgt, C_COUNTER, v_data);
    check_value(v_data = v_t0 or unsigned(v_data) + 1 = unsigned(v_t0), ERROR,
                "counter in memory = t0 or t0 - 1", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- step
  -----------------------------------------------------------------------------
  procedure test_step(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_dpc : std_logic_vector(32-1 downto 0) := C_LOOP;
    variable v_t0  : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_ini, jtag_tgt);
    halt     (jtag_ini, jtag_tgt);
    reg_write(jtag_ini, jtag_tgt, DPC , C_LOOP);
    reg_write(jtag_ini, jtag_tgt, DCSR, DCSR_STEP);
    for i in 1 to 6 loop
      reg_read   (jtag_ini, jtag_tgt, T0, v_t0);
      resume     (jtag_ini, jtag_tgt, "step");
      wait_halted(jtag_ini, jtag_tgt, "step");
      check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_STEP, "step");
      reg_check  (jtag_ini, jtag_tgt, DPC, next_pc(v_dpc), "dpc after the step of 0x" & to_hstring(v_dpc));
      if v_dpc = C_LOOP
      then
        reg_check(jtag_ini, jtag_tgt, T0, std_logic_vector(unsigned(v_t0) + 1), "addi t0 executed once");
      end if;
      v_dpc := next_pc(v_dpc);
    end loop;
    reg_write(jtag_ini, jtag_tgt, DCSR, DCSR_NONE);
    run      (jtag_ini, jtag_tgt, 5 us, "without step");
  end procedure;

  -----------------------------------------------------------------------------
  -- ebreak
  -----------------------------------------------------------------------------
  procedure test_ebreak(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_ini, jtag_tgt);
    halt     (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "ebreak with dcsr.ebreakm : debug mode", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_EBREAKM);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "ebreak");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_EBREAK, "ebreak");
    reg_check  (jtag_ini, jtag_tgt, DPC, C_EBREAK, "dpc = address of the ebreak");
    reg_check  (jtag_ini, jtag_tgt, S1 , x"00000000", "ebreak not executed");

    log(ID_LOG_HDR, "ebreak without dcsr.ebreakm : NOP", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_NONE);
    run        (jtag_ini, jtag_tgt, 5 us, "ebreak NOP");
    reg_check  (jtag_ini, jtag_tgt, S1, x"00000001", "the firmware went on after the ebreak");
    mem_read   (jtag_ini, jtag_tgt, C_FLAG, v_data);
    check_value(v_data, x"00000000", ERROR, "flag cleared by the firmware", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- trigger_execute
  -----------------------------------------------------------------------------
  procedure test_trigger_execute(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_mem : std_logic_vector(32-1 downto 0);
    variable v_t0  : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "tselect, tinfo, tdata1", C_TB_SCOPE);
    reg_check(jtag_ini, jtag_tgt, TSELECT, x"00000000", "tselect after reset");
    reg_write(jtag_ini, jtag_tgt, TSELECT, std_logic_vector(to_unsigned(C_NB_TRIGGERS, 32)));
    reg_check(jtag_ini, jtag_tgt, TSELECT, x"00000000", "tselect : index out of range ignored");
    reg_write(jtag_ini, jtag_tgt, TSELECT, std_logic_vector(to_unsigned(C_NB_TRIGGERS-1, 32)));
    reg_check(jtag_ini, jtag_tgt, TSELECT, std_logic_vector(to_unsigned(C_NB_TRIGGERS-1, 32)), "last trigger");
    reg_check(jtag_ini, jtag_tgt, TINFO  , x"01000040", "tinfo : version 1, mcontrol6");
    reg_check(jtag_ini, jtag_tgt, TDATA1 , x"60000000", "tdata1 after reset");
    reg_write(jtag_ini, jtag_tgt, TSELECT, x"00000000");

    log(ID_LOG_HDR, "dmode = 0 without EBREAK_EXCEPTION : the trigger cannot fire", C_TB_SCOPE);
    reg_write(jtag_ini, jtag_tgt, TDATA2 , C_SW_COUNTER);
    reg_write(jtag_ini, jtag_tgt, TDATA1 , x"60001044");          -- dmode 0, action 1, m, execute
    reg_check(jtag_ini, jtag_tgt, TDATA1 , x"60000004", "tdata1 : action = dmode = 0, m = 0");
    run      (jtag_ini, jtag_tgt, 5 us, "trigger of M-mode");

    log(ID_LOG_HDR, "Execute trigger", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"68001944");         -- chain, match >= : not implemented
    reg_check  (jtag_ini, jtag_tgt, TDATA1, x"68001044", "tdata1 : match equal, no chain");
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"68001044");         -- dmode, action 1, m, execute
    reg_check  (jtag_ini, jtag_tgt, TDATA1, x"68001044", "tdata1");
    mem_read   (jtag_ini, jtag_tgt, C_COUNTER, v_mem);
    reg_write  (jtag_ini, jtag_tgt, DPC   , C_LOOP);
    reg_read   (jtag_ini, jtag_tgt, T0    , v_t0);
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "execute trigger");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_TRIGGER, "execute trigger");
    reg_check  (jtag_ini, jtag_tgt, DPC, C_SW_COUNTER, "dpc = address of the instruction");
    reg_check  (jtag_ini, jtag_tgt, T0 , std_logic_vector(unsigned(v_t0) + 1), "addi before the trigger executed");
    mem_read   (jtag_ini, jtag_tgt, C_COUNTER, v_t0);
    check_value(v_t0, v_mem, ERROR, "store not executed (timing before)", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- trigger_store / trigger_load
  -----------------------------------------------------------------------------
  procedure trigger_memory(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                           tdata1_val : std_logic_vector; tdata2_val : std_logic_vector;
                           pc : std_logic_vector; msg : string) is
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);
    reg_write  (jtag_ini, jtag_tgt, TSELECT, x"00000001");
    reg_write  (jtag_ini, jtag_tgt, TDATA2 , tdata2_val);
    reg_write  (jtag_ini, jtag_tgt, TDATA1 , tdata1_val);
    reg_check  (jtag_ini, jtag_tgt, TDATA1 , tdata1_val, "tdata1");
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, msg);
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_TRIGGER, msg);
    reg_check  (jtag_ini, jtag_tgt, DPC, pc, "dpc " & msg);
  end procedure;

  procedure test_trigger_store(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    trigger_memory(jtag_ini, jtag_tgt, x"68001042", C_COUNTER, C_SW_COUNTER, "store trigger");
  end procedure;

  procedure test_trigger_load(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    trigger_memory(jtag_ini, jtag_tgt, x"68001041", C_FLAG, C_LW_FLAG, "load trigger");
  end procedure;

  -----------------------------------------------------------------------------
  -- trigger_ge / trigger_lt / trigger_range_execute / trigger_range_store
  -- (TRIGGER_RANGE_ENABLE) : tdata1 = dmode, action 1, m, match, chain, type
  -----------------------------------------------------------------------------
  procedure trigger_set(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                        index : natural; tdata1_val : std_logic_vector; tdata2_val : std_logic_vector) is
  begin
    reg_write(jtag_ini, jtag_tgt, TSELECT, std_logic_vector(to_unsigned(index, 32)));
    reg_write(jtag_ini, jtag_tgt, TDATA2 , tdata2_val);
    reg_write(jtag_ini, jtag_tgt, TDATA1 , tdata1_val);
    reg_check(jtag_ini, jtag_tgt, TDATA1 , tdata1_val, "tdata1 of trigger " & integer'image(index));
  end procedure;

  -- Resume from start_pc, the trigger stops the core at the instruction stop_pc
  procedure trigger_run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                        start_pc : std_logic_vector; stop_pc : std_logic_vector; msg : string) is
  begin
    reg_write  (jtag_ini, jtag_tgt, DPC, start_pc);
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, msg);
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_TRIGGER, msg);
    reg_check  (jtag_ini, jtag_tgt, DPC, stop_pc, "dpc " & msg);
  end procedure;

  procedure test_trigger_ge(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);
    -- pc >= beqz : the first instruction of the loop that matches is beqz
    trigger_set(jtag_ini, jtag_tgt, 0, x"68001144", C_BEQZ);
    trigger_run(jtag_ini, jtag_tgt, C_LOOP, C_BEQZ, "trigger >=");
  end procedure;

  procedure test_trigger_lt(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);
    -- pc < sw : resumed on the sw, the trigger fires on the addi of the next loop
    trigger_set(jtag_ini, jtag_tgt, 0, x"680011C4", C_SW_COUNTER);
    trigger_run(jtag_ini, jtag_tgt, C_SW_COUNTER, C_LOOP, "trigger <");
  end procedure;

  procedure test_trigger_range_execute(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "chain of M-mode triggers : tdata2 writable, dmode not set", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, TSELECT, x"00000000");
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"60000804");          -- dmode 0, chain
    reg_check  (jtag_ini, jtag_tgt, TDATA1, x"60000804", "tdata1 : chain of M-mode triggers");
    reg_write  (jtag_ini, jtag_tgt, TSELECT, x"00000001");
    reg_write  (jtag_ini, jtag_tgt, TDATA2, x"08000000");          -- bit 27 : dmode in tdata1
    reg_check  (jtag_ini, jtag_tgt, TDATA2, x"08000000", "tdata2 : written after a chain of M-mode");
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"680011C4");
    reg_check  (jtag_ini, jtag_tgt, TDATA1, x"60000000", "tdata1 : dmode not set after a chain of M-mode");
    reg_write  (jtag_ini, jtag_tgt, TSELECT, x"00000000");
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"60000000");          -- No chain

    log(ID_LOG_HDR, "chain : not on the last trigger, not from M-mode into the debugger", C_TB_SCOPE);
    trigger_set(jtag_ini, jtag_tgt, 1, x"680011C4", C_BEQZ);      -- pc < beqz
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"680019C4");
    reg_check  (jtag_ini, jtag_tgt, TDATA1, x"680011C4", "tdata1 : no chain on the last trigger");
    reg_write  (jtag_ini, jtag_tgt, TSELECT, x"00000000");
    reg_write  (jtag_ini, jtag_tgt, TDATA1, x"60000844");          -- dmode 0, chain
    reg_check  (jtag_ini, jtag_tgt, TDATA1, x"60000004", "tdata1 : no chain from M-mode into the debugger");

    log(ID_LOG_HDR, "Range [lw, beqz) : addi and sw match the < trigger only", C_TB_SCOPE);
    trigger_set(jtag_ini, jtag_tgt, 0, x"68001944", C_LW_FLAG);   -- pc >= lw, chain
    trigger_run(jtag_ini, jtag_tgt, C_LOOP, C_LW_FLAG, "range trigger on the pc");
  end procedure;

  procedure test_trigger_range_store(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect    (jtag_ini, jtag_tgt);
    halt       (jtag_ini, jtag_tgt);
    -- Range [flag, flag + 4) of stores : the store of the counter matches
    -- the < trigger only, the store that clears the flag both
    trigger_set(jtag_ini, jtag_tgt, 1, x"680011C2", std_logic_vector(unsigned(C_FLAG) + 4));
    trigger_set(jtag_ini, jtag_tgt, 0, x"68001942", C_FLAG);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");          -- ebreak (NOP), then flag cleared
    trigger_run(jtag_ini, jtag_tgt, C_LOOP, C_SW_FLAG, "range trigger on the store address");
    mem_read   (jtag_ini, jtag_tgt, C_FLAG, v_data);
    check_value(v_data, x"00000001", ERROR, "store not executed (timing before)", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- resethaltreq
  -----------------------------------------------------------------------------
  procedure test_resethaltreq(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);
    for i in 0 to 1 loop
      -- t0 is not reset : it shows whether an instruction was executed
      reg_write(jtag_ini, jtag_tgt, T0, x"12345678");
      if i = 0
      then
        log(ID_LOG_HDR, "ndmreset with resethaltreq", C_TB_SCOPE);
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000009");  -- setresethaltreq
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000003");  -- ndmreset
      else
        log(ID_LOG_HDR, "ndmreset with haltreq", C_TB_SCOPE);
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"80000003");  -- haltreq, ndmreset
      end if;
      dmi_read   (jtag_ini, jtag_tgt, DM_DMSTATUS , v_data);
      check_value(v_data(13 downto 12), "11", ERROR, "dmstatus.unavail during ndmreset", C_TB_SCOPE);
      -- ndmreset released, haltreq kept
      if i = 0
      then
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000001");
      else
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"80000001");
      end if;
      wait_halted(jtag_ini, jtag_tgt, "after the reset");
      dmi_write  (jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000001");
      dmi_read   (jtag_ini, jtag_tgt, DM_DMSTATUS , v_data);
      check_value(v_data(19 downto 18), "11", ERROR, "dmstatus.havereset", C_TB_SCOPE);
      if i = 0
      then
        check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_RESETHALTREQ, "resethaltreq");
      else
        check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_HALTREQ     , "haltreq at reset");
      end if;
      reg_check  (jtag_ini, jtag_tgt, DPC, C_FIRMWARE_ADDR, "dpc = reset address");
      reg_check  (jtag_ini, jtag_tgt, T0 , x"12345678", "no instruction executed");
      dmi_write  (jtag_ini, jtag_tgt, DM_DMCONTROL, x"10000005");  -- ackhavereset, clrresethaltreq
      run        (jtag_ini, jtag_tgt, 5 us, "after the reset");
      reg_read   (jtag_ini, jtag_tgt, T0, v_data);
      check_value(unsigned(v_data) > 0 and unsigned(v_data) < 16#12345678#, ERROR, "the firmware restarted", C_TB_SCOPE);
    end loop;
  end procedure;

  -----------------------------------------------------------------------------
  -- interrupt
  -----------------------------------------------------------------------------
  procedure test_interrupt(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_dpc : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "Interrupt pending in debug mode : masked", C_TB_SCOPE);
    reg_read   (jtag_ini, jtag_tgt, DPC, v_dpc);
    mem_write  (jtag_ini, jtag_tgt, C_MEIP_ADDR, x"00000001");     -- meip_i = 1
    reg_write  (jtag_ini, jtag_tgt, MSTATUS, x"00001808");         -- MIE written in debug mode
    wait for 5 us;
    wait_halted(jtag_ini, jtag_tgt, "interrupt pending");
    reg_check  (jtag_ini, jtag_tgt, S2 , x"00000000", "no interrupt taken in debug mode");
    reg_check  (jtag_ini, jtag_tgt, DPC, v_dpc     , "dpc unchanged");

    log(ID_LOG_HDR, "Step without dcsr.stepie : interrupt masked", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_STEP);
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "step");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_STEP, "step");
    reg_check  (jtag_ini, jtag_tgt, DPC, next_pc(v_dpc), "dpc of the next instruction");
    reg_check  (jtag_ini, jtag_tgt, S2 , x"00000000", "no interrupt taken during the step");

    log(ID_LOG_HDR, "Step with dcsr.stepie : halt in the handler", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_STEPIE);
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "step");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_STEP, "step");
    reg_check  (jtag_ini, jtag_tgt, DPC   , C_HANDLER  , "dpc = mtvec");
    reg_check  (jtag_ini, jtag_tgt, MCAUSE, x"8000000B", "mcause : external interrupt");

    log(ID_LOG_HDR, "Resume : the handler runs", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_NONE);
    run        (jtag_ini, jtag_tgt, 5 us, "handler");
    reg_check  (jtag_ini, jtag_tgt, S2, x"00000001", "one interrupt taken");
  end procedure;

  -----------------------------------------------------------------------------
  -- exc_ebreak (EBREAK_EXCEPTION)
  -----------------------------------------------------------------------------
  procedure test_exc_ebreak(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "ebreak without dcsr.ebreakm : breakpoint exception", C_TB_SCOPE);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");
    run        (jtag_ini, jtag_tgt, 5 us, "breakpoint exception");
    reg_check  (jtag_ini, jtag_tgt, S3    , x"00000001", "one breakpoint exception");
    reg_check  (jtag_ini, jtag_tgt, S1    , x"00000001", "the firmware went on after the ebreak");
    reg_check  (jtag_ini, jtag_tgt, MCAUSE, x"00000003", "mcause : breakpoint");
    reg_check  (jtag_ini, jtag_tgt, MEPC  , std_logic_vector(unsigned(C_EBREAK) + 4), "mepc = ebreak + 4 (handler)");

    log(ID_LOG_HDR, "ebreak with dcsr.ebreakm : debug mode", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR  , DCSR_EBREAKM);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "ebreak");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_EBREAK, "ebreak");
    reg_check  (jtag_ini, jtag_tgt, DPC, C_EBREAK  , "dpc = address of the ebreak");
    reg_check  (jtag_ini, jtag_tgt, S3 , x"00000001", "no breakpoint exception");
    mem_read   (jtag_ini, jtag_tgt, C_FLAG, v_data);
    check_value(v_data, x"00000001", ERROR, "flag not cleared yet", C_TB_SCOPE);
  end procedure;

  -----------------------------------------------------------------------------
  -- exc_trigger (EBREAK_EXCEPTION)
  -----------------------------------------------------------------------------
  procedure test_exc_trigger(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_mem  : std_logic_vector(32-1 downto 0);
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_ini, jtag_tgt);
    halt     (jtag_ini, jtag_tgt);
    reg_write(jtag_ini, jtag_tgt, TDATA2, C_SW_COUNTER);
    reg_write(jtag_ini, jtag_tgt, TDATA1, x"60000044");           -- dmode 0, m, execute
    reg_check(jtag_ini, jtag_tgt, TDATA1, x"60000044", "tdata1 : trigger of M-mode, action 0");
    mem_read (jtag_ini, jtag_tgt, C_COUNTER, v_mem);
    run      (jtag_ini, jtag_tgt, 5 us, "trigger of M-mode");
    reg_read (jtag_ini, jtag_tgt, S3, v_data);
    check_value(unsigned(v_data) > 1, ERROR, "breakpoint exception at each loop", C_TB_SCOPE);
    reg_check(jtag_ini, jtag_tgt, MCAUSE, x"00000003", "mcause : breakpoint");
    mem_read (jtag_ini, jtag_tgt, C_COUNTER, v_data);
    check_value(v_data, v_mem, ERROR, "store never executed (skipped by the handler)", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_pkg;
