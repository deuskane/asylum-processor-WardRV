-------------------------------------------------------------------------------
-- Title      : WardRV Debug - System Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: System testbench of the debug unit : WardRV_fsm
--              (DEBUG_ENABLE=true) + WardRV_debug (JTAG + DM), driven by the
--              JTAG VIP like a debugger (OpenOCD flow). Firmware :
--              esw/debug/debug_fw.hex (gen_debug_fw.py).
--              Scenario :
--              - IDCODE, dtmcs, dmactive, dmstatus / hartinfo / abstractcs,
--                ackhavereset
--              - halt (haltreq), dcsr (cause 3) and dpc
--              - GPR and CSR read / write (abstract commands), cmderr,
--                abstractauto and aarpostincrement
--              - memory read / write through the program buffer
--              - single step (dcsr.step, cause 4)
--              - software breakpoint (ebreak with dcsr.ebreakm, cause 1)
--              - hardware triggers (execute, store, load : cause 2)
--              - resume, command while running (cmderr 4)
--              - resethaltreq with ndmreset (cause 5, dpc = reset vector)
--              Review (Debug Spec 1.0) : regno that does not exist ->
--              cmderr 3 ; tdata1 never holds dmode=0 with action=1 (reset
--              value, write from debug mode with dmode=0 : the trigger
--              does not fire)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere Review checks (see description)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

use work.WardRV_vips.all;
use work.tb_WardRV_pkg.all;

entity tb_WardRV_debug is
  generic (
    FIRMWARE_FILE : string  := "debug/debug_fw.hex";
    VERBOSE       : boolean := false;
    NB_TRIGGERS   : natural := 2
  );
end entity tb_WardRV_debug;

architecture tb of tb_WardRV_debug is

  constant C_TCK_PERIOD  : time := 50 ns;
  constant C_IDCODE      : std_logic_vector(31 downto 0) := x"1DEB0001";

  -- Firmware (esw/debug/debug_fw.lst)
  constant C_LOOP        : unsigned(31 downto 0) := x"8000000C";
  constant C_SW_COUNTER  : unsigned(31 downto 0) := x"80000010";
  constant C_LW_FLAG     : unsigned(31 downto 0) := x"80000014";
  constant C_EBREAK      : unsigned(31 downto 0) := x"8000001C";
  constant C_END         : unsigned(31 downto 0) := x"8000002C";
  constant C_COUNTER     : std_logic_vector(31 downto 0) := x"80001000";
  constant C_FLAG        : std_logic_vector(31 downto 0) := x"80001004";

  -- Abstract command (Access Register, aarsize = 32 bits)
  constant CMD_READ      : std_logic_vector(31 downto 0) := x"00220000"; -- transfer
  constant CMD_WRITE     : std_logic_vector(31 downto 0) := x"00230000"; -- transfer, write
  constant CMD_POSTEXEC  : std_logic_vector(31 downto 0) := x"00040000";
  constant CMD_POSTINCR  : std_logic_vector(31 downto 0) := x"00080000";
  constant REG_GPR       : natural := 16#1000#;
  constant DM_PROGBUF1   : std_logic_vector(6 downto 0) := "0100001";

  -- Program buffer instructions
  constant PB_LW_X9_X8   : std_logic_vector(31 downto 0) := x"00042483"; -- lw x9, 0(x8)
  constant PB_SW_X9_X8   : std_logic_vector(31 downto 0) := x"00942023"; -- sw x9, 0(x8)

  signal clk             : std_logic := '0';
  signal arst_b          : std_logic := '0';  -- Power-on reset (debug unit)
  signal core_rst_b      : std_logic;         -- Hart reset (power-on and ndmreset)
  signal ndmreset        : std_logic;

  signal mem             : ram_t(0 to C_MEM_SIZE-1);
  signal imem_ini        : imem_ini_t;
  signal imem_tgt        : imem_tgt_t;
  signal dmem_ini        : dmem_ini_t;
  signal dmem_tgt        : dmem_tgt_t;

  signal jtag_ini        : jtag_ini_t := (tck => '0', trst_n => '0', tms => '1', tdi => '0');
  signal jtag_tgt        : jtag_tgt_t;
  signal dbg_hart_req    : dbg_hart_req_t;
  signal dbg_sbi_ini     : dbg_sbi_ini_t;
  signal dbg_sbi_tgt     : dbg_sbi_tgt_t;

begin

  clock_generator(clk, 10 ns);

  arst_b     <= '0', '1' after 50 ns;
  core_rst_b <= arst_b and not ndmreset;

  ------------------------------------------------------------------
  -- DUT
  ------------------------------------------------------------------
  core : entity asylum.WardRV_fsm
    generic map (
      HARTID              => C_HARTID,
      RESET_ADDR          => C_FIRMWARE_ADDR,
      VERBOSE             => VERBOSE,
      DEBUG_ENABLE        => true,
      NB_TRIGGERS         => NB_TRIGGERS,
      EBREAK_EXCEPTION    => false,
      RESETHALTREQ_ENABLE => true
    )
    port map (
      clk_i          => clk,
      arst_b_i       => core_rst_b,
      imem_ini_o     => imem_ini,
      imem_tgt_i     => imem_tgt,
      dmem_ini_o     => dmem_ini,
      dmem_tgt_i     => dmem_tgt,
      meip_i         => '0',
      dbg_hart_req_i => dbg_hart_req,
      dbg_sbi_ini_o  => dbg_sbi_ini,
      dbg_sbi_tgt_i  => dbg_sbi_tgt
    );

  debug : entity asylum.WardRV_debug
    generic map (
      IDCODE_VALUE        => C_IDCODE,
      TRST_ENABLE         => true,
      DTM_IDLE_CYCLES     => 5,
      PROGBUF_SIZE        => 2,
      DATA_COUNT          => 2,
      IMPEBREAK           => true,
      SBA_ENABLE          => false,
      RESETHALTREQ_ENABLE => true,
      HARTID              => C_HARTID
    )
    port map (
      jtag_ini_i     => jtag_ini,
      jtag_tgt_o     => jtag_tgt,
      clk_i          => clk,
      arst_b_i       => arst_b,
      hart_rst_b_i   => core_rst_b,
      dbg_hart_req_o => dbg_hart_req,
      dbg_sbi_ini_i  => dbg_sbi_ini,
      dbg_sbi_tgt_o  => dbg_sbi_tgt,
      ndmreset_o     => ndmreset,
      sba_sbi_ini_o  => open
    );

  ------------------------------------------------------------------
  -- Memory (same responder as tb_WardRV)
  ------------------------------------------------------------------
  p_mem : process
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    init_ram(FIRMWARE_FILE, mem);
    imem_tgt.ready <= '0';
    imem_tgt.inst  <= (others => '0');
    dmem_tgt.ready <= '0';
    dmem_tgt.rdata <= (others => '0');
    dmem_tgt.err   <= '0';
    loop
      wait until rising_edge(clk);
      imem_tgt.ready <= '0';
      dmem_tgt.ready <= '0';
      if imem_ini.valid = '1' then
        check_value(imem_ini.addr(31 downto 11) /= DBG_MEM_BASE(31 downto 11), ERROR,
                    "No fetch from the debug memory on the system bus", C_TB_SCOPE_DEFAULT, ID_NEVER);
        read_mem(mem, imem_ini.addr, v_rdata, VERBOSE, C_FIRMWARE_ADDR);
        imem_tgt.inst  <= v_rdata;
        imem_tgt.ready <= '1';
      end if;
      if dmem_ini.valid = '1' then
        check_value(dmem_ini.addr(31 downto 11) /= DBG_MEM_BASE(31 downto 11), ERROR,
                    "No debug memory access on the system bus", C_TB_SCOPE_DEFAULT, ID_NEVER);
        if dmem_ini.we = '1' then
          write_mem(mem, dmem_ini.addr, dmem_ini.wdata, dmem_ini.be, VERBOSE, C_FIRMWARE_ADDR);
        else
          read_mem(mem, dmem_ini.addr, v_rdata, VERBOSE, C_FIRMWARE_ADDR);
          dmem_tgt.rdata <= v_rdata;
        end if;
        dmem_tgt.ready <= '1';
      end if;
    end loop;
  end process;

  ------------------------------------------------------------------
  -- Debugger
  ------------------------------------------------------------------
  p_main : process

    variable v_data   : std_logic_vector(31 downto 0);
    variable v_data2  : std_logic_vector(31 downto 0);
    variable v_dpc    : std_logic_vector(31 downto 0);
    variable v_x5     : std_logic_vector(31 downto 0);
    variable v_mem    : std_logic_vector(31 downto 0);

    procedure dmi_write(addr : std_logic_vector(6 downto 0); data : std_logic_vector(31 downto 0)) is
    begin
      vip_jtag_dmi_write(jtag_ini, jtag_tgt, addr, data, "", C_TCK_PERIOD);
    end procedure;

    procedure dmi_read(addr : std_logic_vector(6 downto 0); variable data : out std_logic_vector(31 downto 0)) is
    begin
      vip_jtag_dmi_read(jtag_ini, jtag_tgt, addr, data, "", C_TCK_PERIOD);
    end procedure;

    -- Wait the end of the abstract command, check and clear cmderr
    procedure wait_cmd(exp_cmderr : std_logic_vector(2 downto 0) := CMDERR_NONE; msg : string := "") is
      variable v_cs : std_logic_vector(31 downto 0);
    begin
      for i in 1 to 100 loop
        dmi_read(DM_ABSTRACTCS, v_cs);
        exit when v_cs(12) = '0';
      end loop;
      check_value(v_cs(12), '0', ERROR, "abstractcs.busy " & msg);
      check_value(v_cs(10 downto 8), exp_cmderr, ERROR, "abstractcs.cmderr " & msg);
      if v_cs(10 downto 8) /= CMDERR_NONE then
        dmi_write(DM_ABSTRACTCS, x"00000700");      -- W1C
      end if;
    end procedure;

    procedure reg_read(regno : natural; variable data : out std_logic_vector(31 downto 0); msg : string := "") is
    begin
      dmi_write(DM_COMMAND, CMD_READ or std_logic_vector(to_unsigned(regno, 32)));
      wait_cmd(CMDERR_NONE, "read 0x" & to_hstring(to_unsigned(regno, 16)) & " " & msg);
      dmi_read(DM_DATA0, data);
    end procedure;

    procedure reg_write(regno : natural; data : std_logic_vector(31 downto 0); msg : string := "") is
    begin
      dmi_write(DM_DATA0, data);
      dmi_write(DM_COMMAND, CMD_WRITE or std_logic_vector(to_unsigned(regno, 32)));
      wait_cmd(CMDERR_NONE, "write 0x" & to_hstring(to_unsigned(regno, 16)) & " " & msg);
    end procedure;

    -- Memory access through the program buffer (x8 = address, x9 = data)
    procedure mem_read(addr : std_logic_vector(31 downto 0); variable data : out std_logic_vector(31 downto 0)) is
    begin
      dmi_write(DM_PROGBUF0  , PB_LW_X9_X8);
      dmi_write(DM_PROGBUF1, RV_EBREAK);
      dmi_write(DM_DATA0     , addr);
      dmi_write(DM_COMMAND   , CMD_WRITE or CMD_POSTEXEC or std_logic_vector(to_unsigned(REG_GPR+8, 32)));
      wait_cmd(CMDERR_NONE, "progbuf lw");
      reg_read(REG_GPR+9, data);
      log(ID_SEQUENCER, "Memory @0x" & to_hstring(addr) & " = 0x" & to_hstring(data));
    end procedure;

    procedure mem_write(addr : std_logic_vector(31 downto 0); data : std_logic_vector(31 downto 0)) is
    begin
      dmi_write(DM_PROGBUF0  , PB_SW_X9_X8);
      dmi_write(DM_PROGBUF1, RV_EBREAK);
      reg_write(REG_GPR+9, data);
      dmi_write(DM_DATA0     , addr);
      dmi_write(DM_COMMAND   , CMD_WRITE or CMD_POSTEXEC or std_logic_vector(to_unsigned(REG_GPR+8, 32)));
      wait_cmd(CMDERR_NONE, "progbuf sw");
    end procedure;

    procedure wait_halted(msg : string := "") is
      variable v_st : std_logic_vector(31 downto 0);
    begin
      for i in 1 to 100 loop
        dmi_read(DM_DMSTATUS, v_st);
        exit when v_st(9) = '1';
      end loop;
      check_value(v_st(9), '1', ERROR, "dmstatus.allhalted " & msg);
      check_value(v_st(11), '0', ERROR, "dmstatus.allrunning " & msg);
    end procedure;

    procedure halt(msg : string := "") is
    begin
      log(ID_SEQUENCER, "Halt " & msg);
      dmi_write(DM_DMCONTROL, x"80000001");  -- haltreq, dmactive
      wait_halted(msg);
      dmi_write(DM_DMCONTROL, x"00000001");
    end procedure;

    procedure resume(msg : string := "") is
      variable v_st : std_logic_vector(31 downto 0);
    begin
      log(ID_SEQUENCER, "Resume " & msg);
      dmi_write(DM_DMCONTROL, x"40000001");  -- resumereq, dmactive
      for i in 1 to 100 loop
        dmi_read(DM_DMSTATUS, v_st);
        exit when v_st(17) = '1';
      end loop;
      check_value(v_st(17), '1', ERROR, "dmstatus.allresumeack " & msg);
      dmi_write(DM_DMCONTROL, x"00000001");
    end procedure;

    procedure check_cause(cause : std_logic_vector(2 downto 0); msg : string) is
      variable v_dcsr : std_logic_vector(31 downto 0);
    begin
      reg_read(16#7B0#, v_dcsr, "dcsr");
      check_value(v_dcsr(31 downto 28), std_logic_vector'("0100"), ERROR, "dcsr.debugver");
      check_value(v_dcsr(1 downto 0)  , std_logic_vector'("11")  , ERROR, "dcsr.prv");
      check_value(v_dcsr(8 downto 6)  , cause                    , ERROR, "dcsr.cause " & msg);
    end procedure;

    -- Next PC of the firmware (flag = 0, ebreakm = 0)
    function next_pc(pc : unsigned(31 downto 0)) return unsigned is
    begin
      if pc = x"80000018" or pc = x"80000028" then
        return C_LOOP;
      end if;
      return pc + 4;
    end function;

  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    disable_log_msg(ID_POS_ACK);
    if not VERBOSE then
      disable_log_msg(ID_BFM);
    end if;

    wait until arst_b = '1';
    vip_jtag_init(jtag_ini, C_TCK_PERIOD);
    vip_jtag_reset(jtag_ini, "", C_TCK_PERIOD);

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "JTAG : IDCODE, dtmcs");
    vip_jtag_read_idcode(jtag_ini, jtag_tgt, C_IDCODE, "", C_TCK_PERIOD);
    vip_jtag_dtmcs(jtag_ini, jtag_tgt, x"00000000", v_data, "", C_TCK_PERIOD);
    check_value(v_data(9 downto 0), std_logic_vector'("0001110001"), ERROR, "dtmcs abits / version");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "DM : dmactive, dmstatus, hartinfo, abstractcs");
    dmi_read(DM_DMCONTROL, v_data);
    check_value(v_data(0), '0', ERROR, "dmcontrol.dmactive at reset");
    dmi_write(DM_DMCONTROL, x"00000001");
    dmi_read(DM_DMCONTROL, v_data);
    check_value(v_data(0), '1', ERROR, "dmcontrol.dmactive");
    dmi_read(DM_DMSTATUS, v_data);
    check_value(v_data(3 downto 0), std_logic_vector'("0011"), ERROR, "dmstatus.version");
    check_value(v_data(7), '1', ERROR, "dmstatus.authenticated");
    check_value(v_data(22), '1', ERROR, "dmstatus.impebreak");
    check_value(v_data(5), '1', ERROR, "dmstatus.hasresethaltreq");
    check_value(v_data(11 downto 10), std_logic_vector'("11"), ERROR, "dmstatus.all/anyrunning");
    check_value(v_data(9 downto 8), std_logic_vector'("00"), ERROR, "dmstatus.all/anyhalted");
    check_value(v_data(15 downto 12), std_logic_vector'("0000"), ERROR, "dmstatus nonexistent / unavail");
    check_value(v_data(19 downto 18), std_logic_vector'("11"), ERROR, "dmstatus.all/anyhavereset");
    dmi_write(DM_DMCONTROL, x"10000001");  -- ackhavereset
    dmi_read(DM_DMSTATUS, v_data);
    check_value(v_data(19 downto 18), std_logic_vector'("00"), ERROR, "dmstatus.havereset after ackhavereset");
    dmi_read(DM_HARTINFO, v_data);
    check_value(v_data, std_logic_vector'(x"00212B80"), ERROR, "hartinfo (nscratch 2, dataaccess, datasize 2, dataaddr 0xB80)");
    dmi_read(DM_ABSTRACTCS, v_data);
    check_value(v_data, std_logic_vector'(x"02000002"), ERROR, "abstractcs (progbufsize 2, datacount 2)");
    dmi_read(DM_HALTSUM0, v_data);
    check_value(v_data, std_logic_vector'(x"00000000"), ERROR, "haltsum0 running");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Halt request");
    wait for 20 us;
    halt;
    dmi_read(DM_HALTSUM0, v_data);
    check_value(v_data, std_logic_vector'(x"00000001"), ERROR, "haltsum0 halted");
    check_cause(DCSR_CAUSE_HALTREQ, "haltreq");
    reg_read(16#7B1#, v_dpc, "dpc");
    check_value(unsigned(v_dpc) >= C_LOOP and unsigned(v_dpc) < C_END, ERROR, "dpc in the firmware loop : 0x" & to_hstring(v_dpc));

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "GPR / CSR access");
    reg_read(REG_GPR+5, v_x5, "x5");
    check_value(unsigned(v_x5) > 10, ERROR, "x5 counter is running : 0x" & to_hstring(v_x5));
    mem_read(C_COUNTER, v_mem);
    check_value(v_mem = v_x5 or unsigned(v_mem) + 1 = unsigned(v_x5), ERROR, "counter in memory = x5 or x5 - 1");
    reg_read(REG_GPR+10, v_data, "x10");
    check_value(v_data, C_COUNTER, ERROR, "x10 = data pointer");
    reg_write(REG_GPR+5, x"00000100", "x5");
    reg_read (REG_GPR+5, v_data, "x5");
    check_value(v_data, std_logic_vector'(x"00000100"), ERROR, "x5 written");
    reg_write(REG_GPR+0, x"0000FFFF", "x0");
    reg_read (REG_GPR+0, v_data, "x0");
    check_value(v_data, std_logic_vector'(x"00000000"), ERROR, "x0 stays 0");
    reg_read (16#F14#, v_data, "mhartid");
    check_value(v_data, C_HARTID, ERROR, "mhartid");
    reg_write(16#340#, x"12345678", "mscratch");
    reg_read (16#340#, v_data, "mscratch");
    check_value(v_data, std_logic_vector'(x"12345678"), ERROR, "mscratch");
    -- dscratch0 / dscratch1 are used by the debug ROM and the abstract
    -- commands (s0 save) : not accessed by abstract commands here
    reg_write(REG_GPR+8, x"55AA55AA", "s0");
    reg_read (16#7B0#, v_data, "dcsr");                -- CSR access through s0
    reg_read (REG_GPR+8, v_data, "s0");
    check_value(v_data, std_logic_vector'(x"55AA55AA"), ERROR, "s0 preserved by CSR access");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "cmderr");
    dmi_write(DM_COMMAND, x"01000000");                         -- quick access
    wait_cmd(CMDERR_NOTSUP, "cmdtype 1");
    dmi_write(DM_COMMAND, x"00321005");                         -- aarsize 64 bits
    wait_cmd(CMDERR_NOTSUP, "aarsize 3");
    dmi_write(DM_COMMAND, x"00221020");                         -- FPR f0 : does not exist
    wait_cmd(CMDERR_EXCEPTION, "regno 0x1020");
    dmi_write(DM_COMMAND, x"00221020");
    dmi_write(DM_COMMAND, CMD_READ or x"00001005");             -- ignored, cmderr /= 0
    dmi_read (DM_ABSTRACTCS, v_data);
    check_value(v_data(10 downto 8), CMDERR_EXCEPTION, ERROR, "command ignored while cmderr /= 0");
    dmi_write(DM_ABSTRACTCS, x"00000700");
    dmi_read (DM_ABSTRACTCS, v_data);
    check_value(v_data(10 downto 8), CMDERR_NONE, ERROR, "cmderr cleared (W1C)");
    dmi_write(DM_COMMAND, x"00000000");                         -- no transfer, no postexec
    wait_cmd(CMDERR_NONE, "empty command");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "abstractauto and aarpostincrement");
    dmi_write(DM_DATA0, x"0000000A");
    dmi_write(DM_COMMAND, CMD_WRITE or CMD_POSTINCR or x"0000100B");  -- x11, then regno = x12
    wait_cmd(CMDERR_NONE, "postincrement x11");
    dmi_write(DM_ABSTRACTAUTO, x"00000001");                    -- autoexecdata[0]
    dmi_read (DM_ABSTRACTAUTO, v_data);
    check_value(v_data, std_logic_vector'(x"00000001"), ERROR, "abstractauto");
    dmi_write(DM_DATA0, x"0000000B");                           -- x12
    wait_cmd(CMDERR_NONE, "autoexec x12");
    dmi_write(DM_DATA0, x"0000000C");                           -- x13
    wait_cmd(CMDERR_NONE, "autoexec x13");
    dmi_write(DM_ABSTRACTAUTO, x"00000000");
    reg_read (REG_GPR+11, v_data, "x11");
    check_value(v_data, std_logic_vector'(x"0000000A"), ERROR, "x11 (postincrement)");
    reg_read (REG_GPR+12, v_data, "x12");
    check_value(v_data, std_logic_vector'(x"0000000B"), ERROR, "x12 (autoexec)");
    reg_read (REG_GPR+13, v_data, "x13");
    check_value(v_data, std_logic_vector'(x"0000000C"), ERROR, "x13 (autoexec)");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Memory through the program buffer");
    mem_write(x"80001100", x"DEADBEEF");
    mem_read (x"80001100", v_data);
    check_value(v_data, std_logic_vector'(x"DEADBEEF"), ERROR, "memory write / read");
    mem_read (C_FIRMWARE_ADDR, v_data);
    check_value(v_data, std_logic_vector'(x"80001537"), ERROR, "first instruction of the firmware");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Single step");
    reg_write(16#7B1#, std_logic_vector(C_LOOP), "dpc");
    reg_write(16#7B0#, x"40000007", "dcsr step");                -- step, prv = 3
    v_dpc := std_logic_vector(C_LOOP);
    for i in 1 to 6 loop
      reg_read(REG_GPR+5, v_x5, "x5");
      resume("step");
      wait_halted("step");
      check_cause(DCSR_CAUSE_STEP, "step");
      reg_read(16#7B1#, v_data, "dpc");
      check_value(v_data, std_logic_vector(next_pc(unsigned(v_dpc))), ERROR, "dpc after a step");
      if unsigned(v_dpc) = C_LOOP then
        reg_read(REG_GPR+5, v_data2, "x5");
        check_value(unsigned(v_data2), unsigned(v_x5) + 1, ERROR, "addi x5 stepped once");
      end if;
      v_dpc := v_data;
    end loop;
    reg_write(16#7B0#, x"40000003", "dcsr no step");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Software breakpoint (ebreak, dcsr.ebreakm)");
    reg_write(16#7B0#, x"40008003", "dcsr ebreakm");
    mem_write(C_FLAG, x"00000001");
    reg_read (REG_GPR+9, v_data2, "x9");
    resume("to ebreak");
    wait_halted("ebreak");
    check_cause(DCSR_CAUSE_EBREAK, "ebreak");
    reg_read(16#7B1#, v_data, "dpc");
    check_value(v_data, std_logic_vector(C_EBREAK), ERROR, "dpc = address of the ebreak");
    reg_write(16#7B1#, std_logic_vector(C_EBREAK + 4), "dpc after the ebreak");
    reg_write(16#7B0#, x"40000003", "dcsr no ebreakm");
    resume("after ebreak");
    wait for 5 us;
    halt("after ebreak");
    reg_read(REG_GPR+9, v_data, "x9");
    check_value(unsigned(v_data), unsigned(v_data2) + 1, ERROR, "one breakpoint counted by the firmware");
    mem_read(C_FLAG, v_data);
    check_value(v_data, std_logic_vector'(x"00000000"), ERROR, "flag cleared by the firmware");

    ----------------------------------------------------------------
    if NB_TRIGGERS > 0 then
      log(ID_LOG_HDR, "Hardware triggers");
      reg_write(16#7A0#, x"00000000", "tselect");
      reg_read (16#7A0#, v_data, "tselect");
      check_value(v_data, std_logic_vector'(x"00000000"), ERROR, "tselect 0");
      reg_write(16#7A0#, std_logic_vector(to_unsigned(NB_TRIGGERS, 32)), "tselect out of range");
      reg_read (16#7A0#, v_data, "tselect");
      check_value(v_data, std_logic_vector'(x"00000000"), ERROR, "tselect WARL");
      reg_read (16#7A4#, v_data, "tinfo");
      check_value(v_data, std_logic_vector'(x"01000040"), ERROR, "tinfo : mcontrol6");
      reg_read (16#7A1#, v_data, "tdata1 reset");
      check_value(v_data, std_logic_vector'(x"60000000"), ERROR, "tdata1 reset : mcontrol6, dmode 0, action 0");

      -- dmode = 0 with action = 1 is not a legal value : action 0, and m
      -- forced to 0 without EBREAK_EXCEPTION, the trigger does not fire
      reg_write(16#7A2#, std_logic_vector(C_SW_COUNTER), "tdata2");
      reg_write(16#7A1#, x"60001044", "tdata1 dmode 0, action 1");
      reg_read (16#7A1#, v_data, "tdata1");
      check_value(v_data, std_logic_vector'(x"60000004"), ERROR, "tdata1 : no dmode 0 with action 1");
      resume("dmode 0 trigger");
      wait for 5 us;
      halt("dmode 0 trigger");
      check_cause(DCSR_CAUSE_HALTREQ, "dmode 0 trigger did not fire");

      -- Execute trigger on the store of the counter
      reg_write(16#7A2#, std_logic_vector(C_SW_COUNTER), "tdata2");
      reg_write(16#7A1#, x"68001044", "tdata1 execute");        -- type 6, dmode, action 1, m, execute
      reg_read (16#7A1#, v_data, "tdata1");
      check_value(v_data, std_logic_vector'(x"68001044"), ERROR, "tdata1 read back");
      mem_read (C_COUNTER, v_mem);
      reg_read (REG_GPR+5, v_x5, "x5");
      reg_write(16#7B1#, std_logic_vector(C_LOOP), "dpc");
      resume("to execute trigger");
      wait_halted("execute trigger");
      check_cause(DCSR_CAUSE_TRIGGER, "execute trigger");
      reg_read(16#7B1#, v_data, "dpc");
      check_value(v_data, std_logic_vector(C_SW_COUNTER), ERROR, "dpc = address of the triggering instruction");
      reg_read(REG_GPR+5, v_data, "x5");
      check_value(unsigned(v_data), unsigned(v_x5) + 1, ERROR, "addi executed before the trigger");
      mem_read(C_COUNTER, v_data);
      check_value(v_data, v_mem, ERROR, "store not executed (trigger before)");

      -- Store trigger on the counter address (trigger 1)
      reg_write(16#7A1#, x"00000000", "tdata1 disable");
      reg_read (16#7A1#, v_data, "tdata1");
      check_value(v_data, std_logic_vector'(x"60000000"), ERROR, "tdata1 disabled (dmode 0, action 0)");
      if NB_TRIGGERS > 1 then
        reg_write(16#7A0#, x"00000001", "tselect 1");
      end if;
      reg_write(16#7A2#, C_COUNTER, "tdata2");
      reg_write(16#7A1#, x"68001042", "tdata1 store");
      reg_write(16#7B1#, std_logic_vector(C_LOOP), "dpc");
      resume("to store trigger");
      wait_halted("store trigger");
      check_cause(DCSR_CAUSE_TRIGGER, "store trigger");
      reg_read(16#7B1#, v_data, "dpc");
      check_value(v_data, std_logic_vector(C_SW_COUNTER), ERROR, "dpc = address of the store");

      -- Load trigger (match >=) on the flag address
      reg_write(16#7A2#, C_FLAG, "tdata2");
      reg_write(16#7A1#, x"68001141", "tdata1 load >=");         -- match 2
      reg_read (16#7A1#, v_data, "tdata1");
      check_value(v_data, std_logic_vector'(x"68001141"), ERROR, "tdata1 load >= read back");
      reg_write(16#7B1#, std_logic_vector(C_LOOP), "dpc");
      resume("to load trigger");
      wait_halted("load trigger");
      check_cause(DCSR_CAUSE_TRIGGER, "load trigger");
      reg_read(16#7B1#, v_data, "dpc");
      check_value(v_data, std_logic_vector(C_LW_FLAG), ERROR, "dpc = address of the load");
      reg_write(16#7A1#, x"00000000", "tdata1 disable");
    end if;

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Resume, command while running");
    resume("run");
    dmi_read(DM_DMSTATUS, v_data);
    check_value(v_data(11 downto 8), std_logic_vector'("1100"), ERROR, "dmstatus running after resume");
    dmi_write(DM_COMMAND, CMD_READ or x"00001005");
    wait_cmd(CMDERR_HALTRESUME, "command while running");
    wait for 10 us;
    halt("counter");
    reg_read(REG_GPR+5, v_data, "x5");
    check_value(unsigned(v_data) > unsigned(v_x5) + 10, ERROR, "the firmware ran after resume");
    resume("run");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "resethaltreq with ndmreset");
    dmi_write(DM_DMCONTROL, x"00000009");                       -- setresethaltreq
    dmi_write(DM_DMCONTROL, x"00000003");                       -- ndmreset
    dmi_read (DM_DMSTATUS, v_data);
    check_value(v_data(13 downto 12), std_logic_vector'("11"), ERROR, "dmstatus.unavail during ndmreset");
    dmi_read (DM_DMCONTROL, v_data);
    check_value(v_data(1 downto 0), std_logic_vector'("11"), ERROR, "dmcontrol.ndmreset");
    dmi_write(DM_DMCONTROL, x"00000001");                       -- release ndmreset
    wait_halted("resethaltreq");
    dmi_read (DM_DMSTATUS, v_data);
    check_value(v_data(19 downto 18), std_logic_vector'("11"), ERROR, "dmstatus.havereset after ndmreset");
    check_cause(DCSR_CAUSE_RESETHALTREQ, "resethaltreq");
    reg_read(16#7B1#, v_data, "dpc");
    check_value(v_data, C_FIRMWARE_ADDR, ERROR, "dpc = reset vector");
    dmi_write(DM_DMCONTROL, x"10000005");                       -- ackhavereset, clrresethaltreq
    dmi_read (DM_DMSTATUS, v_data);
    check_value(v_data(19 downto 18), std_logic_vector'("00"), ERROR, "havereset acknowledged");
    resume("after reset");
    wait for 5 us;
    halt("after reset");
    reg_read(REG_GPR+5, v_data, "x5");
    check_value(unsigned(v_data) > 0, ERROR, "the firmware restarted");
    resume("end");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "dmactive = 0");
    dmi_write(DM_DMCONTROL, x"00000000");
    dmi_read (DM_DMCONTROL, v_data);
    check_value(v_data, std_logic_vector'(x"00000000"), ERROR, "dmcontrol after dmactive = 0");

    wait for 1 us;
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture tb;
