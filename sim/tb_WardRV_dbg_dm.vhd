-------------------------------------------------------------------------------
-- Title      : WardRV Debug - Debug Module Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Unit testbench of WardRV_dbg_dm, driven directly with the DMI
--              record, with a behavioural hart model on the debug memory bus
--              (SBI) and an SBI RAM on the System Bus Access.
--              The hart model mimics the debug ROM (HALTED / GOING /
--              RESUMING / EXCEPTION, FLAGS polling) and interprets the
--              abstract command program (sw/lw on data0, CSR access through
--              s0 and dscratch1, jal to the program buffer, ebreak).
--              Checked :
--              - dmcontrol / dmstatus fields, dmactive reset, ndmreset,
--                haltreq, resumereq / resumeack, havereset, resethaltreq
--              - abstract command program, cmderr (busy, not supported,
--                exception, halt/resume), abstractauto, postincrement
--              - SBA (SBA_ENABLE) : 8/16/32-bit accesses, autoincrement,
--                readonaddr, readondata, sberror, sbbusyerror
--              Review (Debug Spec 1.0) : regno that does not exist ->
--              cmderr 3, busy error (data / progbuf reads included)
--              reported when the command completes (exception and
--              aarpostincrement kept), dmcontrol hart requests ignored
--              while busy, postexec without program buffer -> cmderr 2,
--              SBA alignment checked on the new sbaddress0, sbdata0 not
--              written while sberror /= 0
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
use     asylum.sbi_pkg.all;
use     asylum.WardRV_debug_pkg.all;
use     asylum.WardRV_dbg_rom_pkg.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

entity tb_WardRV_dbg_dm is
  generic (
    SBA_ENABLE   : boolean := true;
    PROGBUF_SIZE : natural := 2;
    DATA_COUNT   : natural := 2
  );
end entity tb_WardRV_dbg_dm;

architecture tb of tb_WardRV_dbg_dm is

  constant C_CLK_PERIOD   : time    := 10 ns;
  constant C_HART_DELAY   : natural := 20;     -- Cycles to execute a command (busy window)
  constant C_SBA_AWIDTH   : natural := 8;
  constant C_ILLEGAL      : std_logic_vector(31 downto 0) := x"FFFFFFFF"; -- Hart model : exception

  signal clk              : std_logic := '0';
  signal arst_b           : std_logic := '0';
  signal hart_rst_b       : std_logic := '0';

  signal dmi_req          : dmi_req_t := DMI_REQ_NULL;
  signal dmi_rsp          : dmi_rsp_t;
  signal dbg_hart_req     : dbg_hart_req_t;
  signal dbg_sbi_ini      : dbg_sbi_ini_t := DBG_SBI_INI_NULL;
  signal dbg_sbi_tgt      : dbg_sbi_tgt_t;
  signal ndmreset         : std_logic;
  signal sba_sbi_ini      : sbi_ini_t(addr(C_SBA_AWIDTH-1 downto 0), wdata(7 downto 0));
  signal sba_sbi_tgt      : sbi_tgt_t(rdata(7 downto 0));

  -- Hart model state
  type regs_t is array (natural range <>) of std_logic_vector(31 downto 0);
  shared variable hart_gpr : regs_t(0 to 31)   := (others => (others => '0'));
  shared variable hart_csr : regs_t(0 to 4095) := (others => (others => '0'));
  signal hart_halted      : boolean := false;
  signal hart_nb_progbuf  : natural := 0;       -- program buffer executions

begin

  clock_generator(clk, C_CLK_PERIOD);
  arst_b <= '0', '1' after 5 * C_CLK_PERIOD;

  dut : entity asylum.WardRV_dbg_dm
    generic map (
      PROGBUF_SIZE        => PROGBUF_SIZE,
      DATA_COUNT          => DATA_COUNT,
      IMPEBREAK           => true,
      SBA_ENABLE          => SBA_ENABLE,
      SBA_ADDR_WIDTH      => C_SBA_AWIDTH,
      RESETHALTREQ_ENABLE => true,
      HARTID              => x"00000000"
    )
    port map (
      clk_i          => clk,
      arst_b_i       => arst_b,
      dmi_req_i      => dmi_req,
      dmi_rsp_o      => dmi_rsp,
      hart_rst_b_i   => hart_rst_b,
      dbg_hart_req_o => dbg_hart_req,
      dbg_sbi_ini_i  => dbg_sbi_ini,
      dbg_sbi_tgt_o  => dbg_sbi_tgt,
      ndmreset_o     => ndmreset,
      sba_sbi_ini_o  => sba_sbi_ini,
      sba_sbi_tgt_i  => sba_sbi_tgt
    );

  sba_ram : entity asylum.sbi_ram
    generic map (
      NAME      => "sba_ram",
      DEPTH     => 2**C_SBA_AWIDTH,
      SYNC_READ => false
    )
    port map (
      clk_i     => clk,
      arst_b_i  => arst_b,
      sbi_ini_i => sba_sbi_ini,
      sbi_tgt_o => sba_sbi_tgt
    );

  ------------------------------------------------------------------
  -- Hart model (debug ROM behaviour on the debug memory bus)
  ------------------------------------------------------------------
  p_hart : process

    procedure sbi_write(offset : natural; data : std_logic_vector(31 downto 0)) is
    begin
      dbg_sbi_ini.cs    <= '1';
      dbg_sbi_ini.we    <= '1';
      dbg_sbi_ini.re    <= '0';
      dbg_sbi_ini.addr  <= std_logic_vector(to_unsigned(offset, DBG_MEM_AWIDTH));
      dbg_sbi_ini.wdata <= data;
      wait until rising_edge(clk);
      check_value(dbg_sbi_tgt.ready, '1', ERROR, "debug memory : no wait state", C_TB_SCOPE_DEFAULT, ID_NEVER);
      dbg_sbi_ini       <= DBG_SBI_INI_NULL;
    end procedure;

    procedure sbi_read(offset : natural; variable data : out std_logic_vector(31 downto 0)) is
    begin
      dbg_sbi_ini.cs    <= '1';
      dbg_sbi_ini.we    <= '0';
      dbg_sbi_ini.re    <= '1';
      dbg_sbi_ini.addr  <= std_logic_vector(to_unsigned(offset, DBG_MEM_AWIDTH));
      wait until rising_edge(clk);
      check_value(dbg_sbi_tgt.ready, '1', ERROR, "debug memory : no wait state", C_TB_SCOPE_DEFAULT, ID_NEVER);
      data              := dbg_sbi_tgt.rdata;
      dbg_sbi_ini       <= DBG_SBI_INI_NULL;
    end procedure;

    variable v_flags  : std_logic_vector(31 downto 0);
    variable v_inst   : std_logic_vector(31 downto 0);
    variable v_data   : std_logic_vector(31 downto 0);
    variable v_pc     : natural;
    variable v_rd     : natural;
    variable v_rs1    : natural;
    variable v_rs2    : natural;
    variable v_csr    : natural;
    variable v_imm    : integer;
    variable v_exc    : boolean;
    variable v_reset  : boolean;
  begin
    hart_halted <= false;
    wait until arst_b = '1';
    loop
      wait until rising_edge(clk);
      if hart_rst_b = '0' then
        hart_halted <= false;
        v_reset     := true;
      elsif not hart_halted then
        -- Running : halt on request (and at the first instruction after reset,
        -- the core needs some cycles to reach the HALTED write of the ROM)
        if v_reset then
          for i in 1 to 10 loop
            wait until rising_edge(clk);
          end loop;
        end if;
        if dbg_hart_req.haltreq = '1' or (v_reset and dbg_hart_req.resethaltreq = '1') then
          sbi_write(DBG_OFFSET_HALTED, x"00000000");
          hart_halted <= true;
        end if;
        v_reset := false;
      else
        -- Parking loop
        sbi_read(DBG_OFFSET_FLAGS, v_flags);
        if v_flags(0) = '1' then
          sbi_write(DBG_OFFSET_GOING, x"00000000");
          for i in 1 to C_HART_DELAY loop
            wait until rising_edge(clk);
          end loop;
          -- Execute from WHERETO
          sbi_read(DBG_OFFSET_WHERETO, v_inst);
          check_value(v_inst, rv_jal(RV_X0, DBG_OFFSET_ABSTRACT - DBG_OFFSET_WHERETO), ERROR, "WHERETO");
          v_pc  := DBG_OFFSET_ABSTRACT;
          v_exc := false;
          loop
            sbi_read(v_pc, v_inst);
            v_rd  := to_integer(unsigned(v_inst(11 downto 7)));
            v_rs1 := to_integer(unsigned(v_inst(19 downto 15)));
            v_rs2 := to_integer(unsigned(v_inst(24 downto 20)));
            v_csr := to_integer(unsigned(v_inst(31 downto 20)));
            v_pc  := v_pc + 4;
            if v_inst = RV_EBREAK then
              exit;
            elsif v_inst = C_ILLEGAL then
              v_exc := true;
              exit;
            elsif v_inst = RV_NOP then
              null;
            elsif v_inst(6 downto 0) = "0100011" then                 -- sw rs2, imm(x0)
              v_imm := to_integer(signed(std_logic_vector'(v_inst(31 downto 25) & v_inst(11 downto 7))));
              check_value(v_rs1, 0, ERROR, "sw : x0 relative");
              sbi_write(v_imm + 2048, hart_gpr(v_rs2));
            elsif v_inst(6 downto 0) = "0000011" then                 -- lw rd, imm(x0)
              v_imm := to_integer(signed(v_inst(31 downto 20)));
              check_value(v_rs1, 0, ERROR, "lw : x0 relative");
              sbi_read(v_imm + 2048, v_data);
              if v_rd /= 0 then hart_gpr(v_rd) := v_data; end if;
            elsif v_inst(6 downto 0) = "1110011" and v_inst(14 downto 12) = "001" then -- csrrw
              v_data := hart_csr(v_csr);
              hart_csr(v_csr) := hart_gpr(v_rs1);
              if v_rd /= 0 then hart_gpr(v_rd) := v_data; end if;
            elsif v_inst(6 downto 0) = "1110011" and v_inst(14 downto 12) = "010" then -- csrrs (rs1 = x0)
              if v_rd /= 0 then hart_gpr(v_rd) := hart_csr(v_csr); end if;
            elsif v_inst(6 downto 0) = "1101111" then                 -- jal x0, progbuf
              v_imm := to_integer(signed(std_logic_vector'(v_inst(31) & v_inst(19 downto 12) & v_inst(20) & v_inst(30 downto 21) & '0')));
              v_pc  := v_pc - 4 + v_imm;
              check_value(v_pc, DBG_OFFSET_PROGBUF, ERROR, "jal to the program buffer");
              hart_nb_progbuf <= hart_nb_progbuf + 1;
            else
              -- Other program buffer instructions : not executed by the model
              null;
            end if;
          end loop;
          if v_exc then
            sbi_write(DBG_OFFSET_EXCEPTION, x"00000000");
          end if;
          sbi_write(DBG_OFFSET_HALTED, x"00000000");
        elsif v_flags(1) = '1' then
          sbi_write(DBG_OFFSET_RESUMING, x"00000000");
          hart_halted <= false;
        end if;
      end if;
    end loop;
  end process;

  ------------------------------------------------------------------
  -- Sequencer
  ------------------------------------------------------------------
  p_main : process

    variable v_data : std_logic_vector(31 downto 0);
    variable v_nb   : natural;

    procedure dmi(op : std_logic_vector(1 downto 0); addr : std_logic_vector(6 downto 0);
                  wdata : std_logic_vector(31 downto 0); variable rdata : out std_logic_vector(31 downto 0)) is
    begin
      wait until falling_edge(clk);
      dmi_req.valid <= '1';
      dmi_req.op    <= op;
      dmi_req.addr  <= addr;
      dmi_req.data  <= wdata;
      wait until rising_edge(clk);
      check_value(dmi_rsp.ready, '1', ERROR, "DMI ready",  C_TB_SCOPE_DEFAULT, ID_NEVER);
      check_value(dmi_rsp.valid, '1', ERROR, "DMI valid",  C_TB_SCOPE_DEFAULT, ID_NEVER);
      check_value(dmi_rsp.op, DMI_RSP_SUCCESS, ERROR, "DMI op");
      rdata         := dmi_rsp.data;
      dmi_req       <= DMI_REQ_NULL;
      wait until falling_edge(clk);   -- DM registers updated
    end procedure;

    procedure dmi_write(addr : std_logic_vector(6 downto 0); data : std_logic_vector(31 downto 0)) is
      variable v : std_logic_vector(31 downto 0);
    begin
      dmi(DMI_OP_WRITE, addr, data, v);
    end procedure;

    procedure dmi_read(addr : std_logic_vector(6 downto 0); variable data : out std_logic_vector(31 downto 0)) is
    begin
      dmi(DMI_OP_READ, addr, x"00000000", data);
    end procedure;

    procedure dmi_check(addr : std_logic_vector(6 downto 0); exp : std_logic_vector(31 downto 0); msg : string) is
      variable v : std_logic_vector(31 downto 0);
    begin
      dmi_read(addr, v);
      check_value(v, exp, ERROR, msg);
    end procedure;

    procedure dmi_check(addr : std_logic_vector(6 downto 0); msb, lsb : natural; exp : std_logic_vector; msg : string) is
      variable v : std_logic_vector(31 downto 0);
    begin
      dmi_read(addr, v);
      check_value(v(msb downto lsb), exp, ERROR, msg);
    end procedure;

    procedure wait_idle(exp_cmderr : std_logic_vector(2 downto 0); msg : string) is
      variable v : std_logic_vector(31 downto 0);
    begin
      for i in 1 to 1000 loop
        dmi_read(DM_ABSTRACTCS, v);
        exit when v(12) = '0';
      end loop;
      check_value(v(12), '0', ERROR, "abstractcs.busy " & msg);
      check_value(v(10 downto 8), exp_cmderr, ERROR, "abstractcs.cmderr " & msg);
      dmi_write(DM_ABSTRACTCS, x"00000700");
    end procedure;

    procedure wait_status(bit_idx : natural; msg : string) is
      variable v : std_logic_vector(31 downto 0);
    begin
      for i in 1 to 1000 loop
        dmi_read(DM_DMSTATUS, v);
        exit when v(bit_idx) = '1';
      end loop;
      check_value(v(bit_idx), '1', ERROR, "dmstatus(" & integer'image(bit_idx) & ") " & msg);
    end procedure;

    procedure sba_wait(msg : string) is
      variable v : std_logic_vector(31 downto 0);
    begin
      for i in 1 to 1000 loop
        dmi_read(DM_SBCS, v);
        exit when v(21) = '0';
      end loop;
      check_value(v(21), '0', ERROR, "sbcs.sbbusy " & msg);
    end procedure;

    constant DM_DATA1    : std_logic_vector(6 downto 0) := "0000101";
    constant DM_PROGBUF1 : std_logic_vector(6 downto 0) := "0100001";

  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    disable_log_msg(ID_POS_ACK);

    wait until arst_b = '1';
    wait until rising_edge(clk);

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "dmactive = 0 : DM in reset, hart in reset (unavail)");
    dmi_check(DM_DMCONTROL, x"00000000", "dmcontrol at reset");
    dmi_write(DM_DMCONTROL, x"80000003");                       -- ignored fields while dmactive = 0 ? no : dmactive set too
    dmi_check(DM_DMCONTROL, x"00000003", "dmcontrol : haltreq is WARZ, ndmreset, dmactive");
    check_value(ndmreset, '1', ERROR, "ndmreset_o");
    check_value(dbg_hart_req.haltreq, '1', ERROR, "haltreq to the hart");
    dmi_check(DM_DMSTATUS, 13, 12, std_logic_vector'("11"), "dmstatus.unavail : hart in reset");
    dmi_write(DM_DMCONTROL, x"00000000");                       -- dmactive = 0 : reset
    dmi_check(DM_DMCONTROL, x"00000000", "dmcontrol after dmactive = 0");
    check_value(ndmreset, '0', ERROR, "ndmreset_o cleared by dmactive = 0");
    check_value(dbg_hart_req.haltreq, '0', ERROR, "haltreq cleared by dmactive = 0");
    dmi_write(DM_DMCONTROL, x"00000001");
    dmi_check(DM_DMSTATUS, 31, 0, std_logic_vector'(x"004C30A3"),
              "dmstatus : impebreak, havereset, unavail, authenticated, hasresethaltreq, version 3");
    dmi_check(DM_NEXTDM, x"00000000", "nextdm");
    dmi_check("1111111", x"00000000", "unimplemented register reads 0");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Hart out of reset : running, ackhavereset");
    hart_rst_b <= '1';
    wait for 100 ns;
    dmi_check(DM_DMSTATUS, 19, 8, std_logic_vector'(x"C0C"), "havereset, running");
    dmi_write(DM_DMCONTROL, x"10000001");
    dmi_check(DM_DMSTATUS, 19, 8, std_logic_vector'(x"00C"), "havereset acknowledged");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Command while running : cmderr 4");
    dmi_write(DM_COMMAND, x"00221005");
    wait_idle(CMDERR_HALTRESUME, "running");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Halt / resume");
    dmi_write(DM_DMCONTROL, x"80000001");
    wait_status(9, "allhalted");
    dmi_write(DM_DMCONTROL, x"00000001");
    dmi_check(DM_HALTSUM0, x"00000001", "haltsum0");
    dmi_write(DM_DMCONTROL, x"C0000001");                       -- resumereq with haltreq : ignored
    wait for 1 us;
    dmi_check(DM_DMSTATUS, 17, 8, std_logic_vector'("0000000011"), "resumereq ignored with haltreq");
    dmi_write(DM_DMCONTROL, x"40000001");
    wait_status(17, "allresumeack");
    dmi_check(DM_DMSTATUS, 17, 8, std_logic_vector'("1100001100"), "resumed : resumeack, running");
    dmi_write(DM_DMCONTROL, x"80000001");
    wait_status(9, "allhalted");
    dmi_write(DM_DMCONTROL, x"00000001");
    dmi_check(DM_DMSTATUS, 17, 16, std_logic_vector'("11"), "resumeack kept until next resumereq");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "Abstract commands");
    hart_gpr(5) := x"12345678";
    dmi_write(DM_COMMAND, x"00221005");                         -- read x5
    wait_idle(CMDERR_NONE, "read x5");
    dmi_check(DM_DATA0, x"12345678", "read x5");
    dmi_write(DM_DATA0, x"CAFEDECA");
    dmi_write(DM_COMMAND, x"00231006");                         -- write x6
    wait_idle(CMDERR_NONE, "write x6");
    check_value(hart_gpr(6), std_logic_vector'(x"CAFEDECA"), ERROR, "x6 written");
    hart_gpr(8)          := x"5A5A5A5A";
    hart_csr(16#340#)    := x"00C0FFEE";
    dmi_write(DM_COMMAND, x"00220340");                         -- read mscratch
    wait_idle(CMDERR_NONE, "read mscratch");
    dmi_check(DM_DATA0, x"00C0FFEE", "read mscratch");
    check_value(hart_gpr(8), std_logic_vector'(x"5A5A5A5A"), ERROR, "s0 restored");
    dmi_write(DM_DATA0, x"0BADF00D");
    dmi_write(DM_COMMAND, x"00230341");                         -- write mepc
    wait_idle(CMDERR_NONE, "write mepc");
    check_value(hart_csr(16#341#), std_logic_vector'(x"0BADF00D"), ERROR, "mepc written");
    check_value(hart_gpr(8), std_logic_vector'(x"5A5A5A5A"), ERROR, "s0 restored");

    log(ID_LOG_HDR, "cmderr");
    dmi_write(DM_COMMAND, x"02000000");
    wait_idle(CMDERR_NOTSUP, "cmdtype 2");
    dmi_write(DM_COMMAND, x"00421005");
    wait_idle(CMDERR_NOTSUP, "aarsize 128");
    dmi_write(DM_COMMAND, x"0022C000");
    wait_idle(CMDERR_EXCEPTION, "regno 0xC000 does not exist : cmderr 3");
    dmi_write(DM_COMMAND, x"00221020");
    wait_idle(CMDERR_EXCEPTION, "regno 0x1020 (no FPR) does not exist : cmderr 3");
    -- busy : command, data, progbuf, abstractauto and abstractcs accesses
    -- (cmderr is only valid once busy = 0 : the busy error is reported when
    -- the command completes)
    dmi_write(DM_COMMAND, x"00221005");
    dmi_check(DM_ABSTRACTCS, 12, 12, "1", "busy");
    dmi_write(DM_COMMAND, x"00221006");
    dmi_check(DM_ABSTRACTCS, 12, 12, "1", "command while busy : still busy");
    wait_idle(CMDERR_BUSY, "command while busy");
    dmi_check(DM_DATA0, x"12345678", "first command done, second ignored");
    dmi_write(DM_COMMAND, x"00221005");
    dmi_write(DM_DATA0, x"FFFFFFFF");
    wait_idle(CMDERR_BUSY, "data write while busy");
    dmi_check(DM_DATA0, x"12345678", "data write ignored while busy");
    dmi_write(DM_COMMAND, x"00221005");
    dmi_read (DM_DATA0, v_data);
    wait_idle(CMDERR_BUSY, "data read while busy");
    if PROGBUF_SIZE > 0 then
      dmi_write(DM_COMMAND, x"00221005");
      dmi_write(DM_PROGBUF0, x"FFFFFFFF");
      wait_idle(CMDERR_BUSY, "progbuf write while busy");
      dmi_write(DM_COMMAND, x"00221005");
      dmi_read (DM_PROGBUF0, v_data);
      wait_idle(CMDERR_BUSY, "progbuf read while busy");
    end if;
    dmi_write(DM_COMMAND, x"00221005");
    dmi_write(DM_ABSTRACTAUTO, x"00000001");
    wait_idle(CMDERR_BUSY, "abstractauto write while busy");
    dmi_check(DM_ABSTRACTAUTO, x"00000000", "abstractauto write ignored while busy");
    dmi_write(DM_COMMAND, x"00221005");
    dmi_write(DM_ABSTRACTCS, x"00000700");
    wait_idle(CMDERR_BUSY, "abstractcs write while busy");

    -- aarpostincrement : a busy error does not cancel a successful access
    hart_gpr(20) := x"00000020";
    hart_gpr(21) := x"00000021";
    dmi_write(DM_COMMAND, x"002A1014");                         -- read x20, postincrement
    dmi_read (DM_DATA0, v_data);                                -- access while busy
    wait_idle(CMDERR_BUSY, "read x20 with an access while busy");
    dmi_check(DM_DATA0, x"00000020", "x20 read despite the busy error");
    dmi_write(DM_ABSTRACTAUTO, x"00000001");                    -- autoexecdata[0]
    dmi_read (DM_DATA0, v_data);                                -- command again : x21
    wait_idle(CMDERR_NONE, "autoexec after aarpostincrement");
    dmi_write(DM_ABSTRACTAUTO, x"00000000");
    dmi_check(DM_DATA0, x"00000021", "regno incremented despite the busy error");

    -- dmcontrol hart requests ignored while an abstract command is executing
    log(ID_LOG_HDR, "dmcontrol haltreq / resumereq ignored while busy");
    dmi_check(DM_DMSTATUS, 17, 8, std_logic_vector'("1100000011"), "halted, resumeack before the test");
    dmi_write(DM_COMMAND, x"00221005");
    dmi_write(DM_DMCONTROL, x"40000001");                       -- resumereq while busy
    dmi_check(DM_ABSTRACTCS, 12, 12, "1", "resumereq written while busy");
    wait_idle(CMDERR_NONE, "dmcontrol write while busy is not a busy error");
    wait for 1 us;
    dmi_check(DM_DMSTATUS, 17, 8, std_logic_vector'("1100000011"), "resumereq ignored while busy : still halted");
    dmi_write(DM_COMMAND, x"00221005");
    dmi_write(DM_DMCONTROL, x"80000001");                       -- haltreq while busy
    dmi_check(DM_ABSTRACTCS, 12, 12, "1", "haltreq written while busy");
    check_value(dbg_hart_req.haltreq, '0', ERROR, "haltreq ignored while busy");
    wait_idle(CMDERR_NONE, "dmcontrol write while busy is not a busy error");

    if PROGBUF_SIZE > 0 then
      -- exception in the program buffer
      dmi_write(DM_PROGBUF0, C_ILLEGAL);
      dmi_write(DM_COMMAND, x"00040000");                       -- postexec
      wait_idle(CMDERR_EXCEPTION, "exception");
      -- exception after an access while busy : cmderr 3 (busy only written
      -- if cmderr is still 0 when the command completes)
      dmi_write(DM_COMMAND, x"00040000");
      dmi_write(DM_DATA0, x"FFFFFFFF");
      wait_idle(CMDERR_EXCEPTION, "exception and access while busy");
      -- program buffer, impebreak
      dmi_write(DM_PROGBUF0, RV_NOP);
      if PROGBUF_SIZE > 1 then
        dmi_write(DM_PROGBUF1, RV_NOP);
        dmi_check(DM_PROGBUF1, RV_NOP, "progbuf1 read back");
      end if;
      v_nb := hart_nb_progbuf;
      dmi_write(DM_COMMAND, x"00261007");                       -- read x7, postexec
      wait_idle(CMDERR_NONE, "postexec");
      check_value(hart_nb_progbuf, v_nb + 1, ERROR, "program buffer executed");
    else
      dmi_write(DM_COMMAND, x"00040000");                       -- postexec
      wait_idle(CMDERR_NOTSUP, "postexec without program buffer");
    end if;

    log(ID_LOG_HDR, "abstractauto, aarpostincrement");
    dmi_write(DM_DATA0, x"00000011");
    dmi_write(DM_COMMAND, x"002B1010");                         -- write x16, postincrement
    wait_idle(CMDERR_NONE, "write x16");
    dmi_write(DM_ABSTRACTAUTO, x"00010001");                    -- autoexecprogbuf[0], autoexecdata[0]
    if PROGBUF_SIZE > 0 then
      dmi_check(DM_ABSTRACTAUTO, x"00010001", "abstractauto");
    else
      dmi_check(DM_ABSTRACTAUTO, x"00000001", "abstractauto (no program buffer)");
    end if;
    dmi_write(DM_DATA0, x"00000012");                           -- x17
    wait_idle(CMDERR_NONE, "autoexec data0 write");
    dmi_read (DM_DATA0, v_data);                                -- x18 <- 0x12 (data0 read triggers)
    wait_idle(CMDERR_NONE, "autoexec data0 read");
    if PROGBUF_SIZE > 0 then
      dmi_write(DM_PROGBUF0, RV_NOP);                           -- x19 <- 0x12
      wait_idle(CMDERR_NONE, "autoexec progbuf0 write");
    end if;
    dmi_write(DM_ABSTRACTAUTO, x"00000000");
    check_value(hart_gpr(16), std_logic_vector'(x"00000011"), ERROR, "x16");
    check_value(hart_gpr(17), std_logic_vector'(x"00000012"), ERROR, "x17 (autoexecdata write)");
    check_value(hart_gpr(18), std_logic_vector'(x"00000012"), ERROR, "x18 (autoexecdata read)");
    if PROGBUF_SIZE > 0 then
      check_value(hart_gpr(19), std_logic_vector'(x"00000012"), ERROR, "x19 (autoexecprogbuf)");
    end if;
    dmi_write(DM_DATA1, x"00000099");
    dmi_check(DM_DATA1, x"00000099", "data1");

    ----------------------------------------------------------------
    log(ID_LOG_HDR, "resethaltreq");
    dmi_write(DM_DMCONTROL, x"00000009");                       -- setresethaltreq
    check_value(dbg_hart_req.resethaltreq, '1', ERROR, "resethaltreq to the hart");
    dmi_write(DM_DMCONTROL, x"40000001");
    wait_status(11, "allrunning");
    hart_rst_b <= '0';
    wait for 100 ns;
    hart_rst_b <= '1';
    wait_status(9, "halted after reset");
    dmi_check(DM_DMSTATUS, 19, 18, std_logic_vector'("11"), "havereset after hart reset");
    dmi_write(DM_DMCONTROL, x"10000005");                       -- ackhavereset, clrresethaltreq
    check_value(dbg_hart_req.resethaltreq, '0', ERROR, "resethaltreq cleared");

    ----------------------------------------------------------------
    if SBA_ENABLE then
      log(ID_LOG_HDR, "System Bus Access");
      dmi_check(DM_SBCS, x"20040107", "sbcs reset : version 1, sbaccess 32, sbasize 8, 8/16/32");
      dmi_write(DM_SBADDRESS0, x"00000010");
      dmi_write(DM_SBDATA0, x"44332211");                       -- 32-bit write
      sba_wait("write 32");
      dmi_write(DM_SBCS, x"00140000");                          -- sbreadonaddr, sbaccess 32
      dmi_write(DM_SBADDRESS0, x"00000010");
      sba_wait("read 32");
      dmi_check(DM_SBDATA0, x"44332211", "read 32");
      dmi_write(DM_SBCS, x"00100000");                          -- sbreadonaddr, sbaccess 8
      dmi_write(DM_SBADDRESS0, x"00000012");
      sba_wait("read 8");
      dmi_check(DM_SBDATA0, x"00000033", "read 8");
      dmi_write(DM_SBCS, x"00120000");                          -- sbreadonaddr, sbaccess 16
      dmi_write(DM_SBADDRESS0, x"00000012");
      sba_wait("read 16");
      dmi_check(DM_SBDATA0, x"00004433", "read 16");
      dmi_write(DM_SBCS, x"00010000");                          -- sbaccess 8, autoincrement
      dmi_write(DM_SBADDRESS0, x"00000020");
      dmi_write(DM_SBDATA0, x"000000A0"); sba_wait("write 8");
      dmi_write(DM_SBDATA0, x"000000A1"); sba_wait("write 8");
      dmi_write(DM_SBDATA0, x"000000A2"); sba_wait("write 8");
      dmi_write(DM_SBDATA0, x"000000A3"); sba_wait("write 8");
      dmi_check(DM_SBADDRESS0, x"00000024", "autoincrement");
      dmi_write(DM_SBCS, x"00040000");                          -- sbaccess 32
      dmi_write(DM_SBADDRESS0, x"00000014");
      dmi_write(DM_SBDATA0, x"55667788"); sba_wait("write 32");
      dmi_write(DM_SBCS, x"00158000");                          -- readonaddr, sbaccess 32, autoincrement, readondata
      dmi_write(DM_SBADDRESS0, x"00000010");
      sba_wait("read 32");
      dmi_check(DM_SBDATA0, x"44332211", "readondata 0x10");
      sba_wait("read 32");
      dmi_check(DM_SBDATA0, x"55667788", "readondata 0x14");
      sba_wait("read 32");
      dmi_check(DM_SBADDRESS0, x"0000001C", "autoincrement after reads");
      dmi_write(DM_SBCS, x"00140000");                          -- readonaddr, sbaccess 32
      dmi_write(DM_SBADDRESS0, x"00000020");
      sba_wait("read 32");
      dmi_check(DM_SBDATA0, x"A3A2A1A0", "bytes written with autoincrement");
      -- sberror : misaligned, then unsupported size
      dmi_write(DM_SBCS, x"00040000");                          -- sbaccess 32
      dmi_write(DM_SBADDRESS0, x"00000011");
      dmi_write(DM_SBDATA0, x"00000000");
      dmi_check(DM_SBCS, 14, 12, std_logic_vector'("011"), "sberror misaligned");
      dmi_write(DM_SBDATA0, x"DEADBEEF");                       -- sberror /= 0 : does nothing
      dmi_check(DM_SBDATA0, x"00000000", "sbdata0 not written while sberror /= 0");
      dmi_write(DM_SBCS, x"00047000");                          -- clear sberror
      dmi_check(DM_SBCS, 14, 12, std_logic_vector'("000"), "sberror cleared");
      dmi_write(DM_SBCS, x"00060000");                          -- sbaccess 64
      dmi_write(DM_SBADDRESS0, x"00000010");
      dmi_write(DM_SBDATA0, x"00000000");
      dmi_check(DM_SBCS, 14, 12, std_logic_vector'("100"), "sberror size");
      dmi_write(DM_SBCS, x"00047000");
      -- sbreadonaddr : alignment checked on the new value of sbaddress0
      dmi_write(DM_SBADDRESS0, x"00000011");                    -- misaligned, no access
      dmi_write(DM_SBCS, x"00140000");                          -- sbreadonaddr, sbaccess 32
      dmi_write(DM_SBADDRESS0, x"00000010");                    -- aligned new address
      sba_wait("read 32 after a misaligned address");
      dmi_check(DM_SBCS, 14, 12, std_logic_vector'("000"), "no sberror : aligned new address");
      dmi_check(DM_SBDATA0, x"44332211", "read at the new address");
      dmi_write(DM_SBADDRESS0, x"00000012");                    -- misaligned new address
      sba_wait("misaligned readonaddr");
      dmi_check(DM_SBCS, 14, 12, std_logic_vector'("011"), "sberror : misaligned new address");
      dmi_check(DM_SBDATA0, x"44332211", "no access at a misaligned address");
      dmi_write(DM_SBCS, x"00047000");                          -- clear sberror, sbaccess 32
      -- sbbusyerror : access while busy
      dmi_write(DM_SBADDRESS0, x"00000030");
      dmi_write(DM_SBDATA0, x"11111111");
      dmi_write(DM_SBDATA0, x"22222222");
      dmi_check(DM_SBCS, 22, 22, "1", "sbbusyerror");
      sba_wait("write 32");
      dmi_write(DM_SBCS, x"00440000");                          -- clear sbbusyerror
      dmi_check(DM_SBCS, 22, 22, "0", "sbbusyerror cleared");
      dmi_write(DM_SBCS, x"00140000");
      dmi_write(DM_SBADDRESS0, x"00000030");
      sba_wait("read 32");
      dmi_check(DM_SBDATA0, x"11111111", "second write ignored (sbbusyerror)");
    else
      dmi_check(DM_SBCS, x"00000000", "sbcs without SBA");
    end if;

    wait for 100 ns;
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture tb;
