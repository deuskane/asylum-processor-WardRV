-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_bfm_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_bfm_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Constants and debugger procedures shared by the scenarios
--              of tb_WardRV_debug : the sequencer is a debugger on the JTAG
--              port, the core runs esw/debug/debug_fw.S. The debugger
--              procedures (halt, resume, register and memory access) follow
--              the OpenOCD flow.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_debug_pkg.vhd)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

package tb_WardRV_debug_bfm_pkg is

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

  -- Debugger
  procedure dmi_write(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      addr : std_logic_vector; data : std_logic_vector);

  procedure dmi_read(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                     addr : std_logic_vector; variable data : out std_logic_vector);

  -- dmactive, IDCODE, havereset acknowledged
  procedure connect(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

  procedure wait_halted(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; msg : string);

  procedure halt(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; msg : string := "");

  procedure resume(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t; msg : string := "");

  -- End of the abstract command, cmderr checked and cleared
  procedure wait_command(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                         cmderr : std_logic_vector(2 downto 0); msg : string);

  procedure reg_read(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                     regno : natural; variable data : out std_logic_vector);

  procedure reg_check(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      regno : natural; exp : std_logic_vector; msg : string);

  procedure reg_write(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      regno : natural; data : std_logic_vector);

  -- Memory : a4 written by the abstract command, then the program buffer
  procedure mem_access(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                       inst : std_logic_vector; addr : std_logic_vector);

  procedure mem_read(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                     addr : std_logic_vector; variable data : out std_logic_vector);

  procedure mem_write(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                      addr : std_logic_vector; data : std_logic_vector);

  procedure check_cause(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                        cause : std_logic_vector(2 downto 0); msg : string);

  -- Running for a while, then halted (cause haltreq)
  procedure run_for(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                    duration : time; msg : string);

  -- Address of the next instruction of the loop (flag = 0)
  function next_pc(pc : std_logic_vector(32-1 downto 0)) return std_logic_vector;

  -- trigger_store / trigger_load
  procedure trigger_memory(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                           tdata1_val : std_logic_vector; tdata2_val : std_logic_vector;
                           pc : std_logic_vector; msg : string);

  -- trigger_ge / trigger_lt / trigger_range_execute / trigger_range_store
  -- (TRIGGER_RANGE_ENABLE) : tdata1 = dmode, action 1, m, match, chain, type
  procedure trigger_set(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                        index : natural; tdata1_val : std_logic_vector; tdata2_val : std_logic_vector);

  -- Resume from start_pc, the trigger stops the core at the instruction stop_pc
  procedure trigger_run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
                        start_pc : std_logic_vector; stop_pc : std_logic_vector; msg : string);

end package tb_WardRV_debug_bfm_pkg;

package body tb_WardRV_debug_bfm_pkg is

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
  procedure run_for(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t;
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

end package body tb_WardRV_debug_bfm_pkg;
