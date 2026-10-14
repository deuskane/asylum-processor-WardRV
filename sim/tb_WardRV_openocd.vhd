-------------------------------------------------------------------------------
-- Title      : WardRV Debug - OpenOCD + GDB co-simulation testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_openocd.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: WardRV_fsm (DEBUG_ENABLE, NB_TRIGGERS = 2, RESETHALTREQ) +
--              WardRV_debug (JTAG + DM, SBA_ENABLE with a 32-bit address)
--              driven by a real debugger : OpenOCD (remote_bitbang adapter)
--              connects to the TCP server of sim/openocd/remote_bitbang.c,
--              called every TCK_DIV clock cycles through VHPIDIRECT
--              (rbb_tick, sim/openocd/rbb_pkg.vhd). GDB runs the scenario
--              sim/openocd/scenario.py on the gdb server of OpenOCD (target
--              cosim_openocd, hooks sim/openocd/launch.sh and check.sh).
--              - RAM of C_RAM_SIZE bytes at C_FIRMWARE_ADDR (RESET_ADDR),
--                loaded with FIRMWARE_FILE (esw/openocd/fw.hex), accessed by
--                the core (imem / dmem) and by the System Bus Access (8-bit
--                SBI, one byte per cycle without wait state)
--              - ndmreset_o (and the SRST* of OpenOCD) reset the core, never
--                the debug unit
--              - end of the simulation when OpenOCD quits (Q or connection
--                closed), error after TIMEOUT_US (watchdog, simulated time)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;
use     asylum.WardRV_debug_pkg.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

use work.tb_WardRV_pkg.all;
use work.rbb_pkg.all;

entity tb_WardRV_openocd is
  generic (
    FIRMWARE_FILE : string   := "openocd/fw.hex";
    VERBOSE       : boolean  := false;
    TCK_DIV       : positive := 1;       -- clk cycles between two rbb_tick (TCK period = 2 x TCK_DIV clk)
    TIMEOUT_US    : positive := 250000   -- Watchdog in us of simulated time (a full scenario takes ~34 ms)
  );
end entity tb_WardRV_openocd;

architecture tb of tb_WardRV_openocd is

  -- IDCODE : same value in sim/openocd/wardrv_sim.cfg (-expected-id)
  constant C_IDCODE      : std_logic_vector(31 downto 0) := x"1DEB0001";
  -- RAM : esw/openocd/link.ld
  constant C_RAM_SIZE    : natural := 16#10000#;
  constant C_SBA_WIDTH   : positive := 32;

  signal clk             : std_logic := '0';
  signal arst_b          : std_logic := '0';  -- Power-on reset (debug unit and core)
  signal core_rst_b      : std_logic;         -- Hart reset (power-on, ndmreset, SRST*)
  signal ndmreset        : std_logic;
  signal srst_n          : std_logic := '1';

  signal mem             : ram_t(0 to C_RAM_SIZE-1);
  signal imem_ini        : imem_ini_t;
  signal imem_tgt        : imem_tgt_t;
  signal dmem_ini        : dmem_ini_t;
  signal dmem_tgt        : dmem_tgt_t;

  signal jtag_ini        : jtag_ini_t := (tck => '0', trst_n => '1', tms => '1', tdi => '0');
  signal jtag_tgt        : jtag_tgt_t;
  signal dbg_hart_req    : dbg_hart_req_t;
  signal dbg_sbi_ini     : dbg_sbi_ini_t;
  signal dbg_sbi_tgt     : dbg_sbi_tgt_t;
  signal sba_sbi_ini     : sbi_ini_t(addr (C_SBA_WIDTH-1 downto 0), wdata(7 downto 0));
  signal sba_sbi_tgt     : sbi_tgt_t(rdata(7 downto 0));

  signal quit            : boolean := false;

begin

  clock_generator(clk, C_CLK_PERIOD);

  arst_b     <= '0', '1' after 5*C_CLK_PERIOD;
  core_rst_b <= arst_b and srst_n and not ndmreset;

  ------------------------------------------------------------------
  -- DUT
  ------------------------------------------------------------------
  core : entity asylum.WardRV_fsm
    generic map (
      HARTID              => C_HARTID,
      RESET_ADDR          => C_FIRMWARE_ADDR,
      VERBOSE             => VERBOSE,
      DEBUG_ENABLE        => true,
      NB_TRIGGERS         => 2,
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
      SBA_ENABLE          => true,
      SBA_ADDR_WIDTH      => C_SBA_WIDTH,
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
      sba_sbi_ini_o  => sba_sbi_ini,
      sba_sbi_tgt_i  => sba_sbi_tgt
    );

  ------------------------------------------------------------------
  -- RAM : core (imem / dmem, answer in one cycle) and SBA writes
  ------------------------------------------------------------------
  p_mem : process
    variable v_rdata : std_logic_vector(31 downto 0);
    variable v_be    : std_logic_vector(3 downto 0);
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
        if imem_ini.addr(31 downto 11) = DBG_MEM_BASE(31 downto 11) then
          alert(TB_WARNING, "Fetch from the debug memory window on the system bus @0x" & to_hstring(imem_ini.addr));
        end if;
        read_mem(mem, imem_ini.addr, v_rdata, VERBOSE, C_FIRMWARE_ADDR);
        imem_tgt.inst  <= v_rdata;
        imem_tgt.ready <= '1';
      end if;
      if dmem_ini.valid = '1' then
        if dmem_ini.addr(31 downto 11) = DBG_MEM_BASE(31 downto 11) then
          alert(TB_WARNING, "Access to the debug memory window on the system bus @0x" & to_hstring(dmem_ini.addr));
        end if;
        if dmem_ini.we = '1' then
          write_mem(mem, dmem_ini.addr, dmem_ini.wdata, dmem_ini.be, VERBOSE, C_FIRMWARE_ADDR);
        else
          read_mem(mem, dmem_ini.addr, v_rdata, VERBOSE, C_FIRMWARE_ADDR);
          dmem_tgt.rdata <= v_rdata;
        end if;
        dmem_tgt.ready <= '1';
      end if;
      -- System Bus Access : byte write
      if sba_sbi_ini.cs = '1' and sba_sbi_ini.we = '1' then
        v_be := (others => '0');
        v_be(to_integer(unsigned(sba_sbi_ini.addr(1 downto 0)))) := '1';
        write_mem(mem, sba_sbi_ini.addr, sba_sbi_ini.wdata & sba_sbi_ini.wdata & sba_sbi_ini.wdata & sba_sbi_ini.wdata,
                  v_be, VERBOSE, C_FIRMWARE_ADDR);
      end if;
    end loop;
  end process;

  -- System Bus Access : byte read, no wait state
  p_sba_read : process (sba_sbi_ini, mem)
    variable v_rdata : std_logic_vector(31 downto 0);
    variable v_byte  : natural range 0 to 3;
  begin
    if sba_sbi_ini.cs = '1' and sba_sbi_ini.re = '1' then
      read_mem(mem, sba_sbi_ini.addr, v_rdata, false, C_FIRMWARE_ADDR);
      v_byte := to_integer(unsigned(sba_sbi_ini.addr(1 downto 0)));
      sba_sbi_tgt.rdata <= v_rdata(8*v_byte+7 downto 8*v_byte);
    else
      sba_sbi_tgt.rdata <= (others => '0');
    end if;
  end process;

  sba_sbi_tgt.ready     <= sba_sbi_ini.cs;
  sba_sbi_tgt.info.name <= to_sbi_name("tb_ram");

  ------------------------------------------------------------------
  -- JTAG : OpenOCD remote_bitbang (TCK, TMS, TDI, TRST*, SRST*)
  ------------------------------------------------------------------
  p_rbb : process
    variable v_tdo    : integer;
    variable v_pins   : std_logic_vector(RBB_WIDTH-1 downto 0);
    variable v_writes : natural := 0;
  begin
    wait until arst_b = '1';
    loop
      for i in 1 to TCK_DIV loop
        wait until rising_edge(clk);
      end loop;
      -- TDO driven only in Shift-IR / Shift-DR (pull-up otherwise)
      v_tdo  := 0 when jtag_tgt.tdo_en = '1' and jtag_tgt.tdo = '0' else 1;
      v_pins := std_logic_vector(to_unsigned(rbb_tick(v_tdo), RBB_WIDTH));
      jtag_ini.tck    <= v_pins(RBB_TCK);
      jtag_ini.tms    <= v_pins(RBB_TMS);
      jtag_ini.tdi    <= v_pins(RBB_TDI);
      jtag_ini.trst_n <= v_pins(RBB_TRST_N);
      srst_n          <= v_pins(RBB_SRST_N);
      if v_pins(RBB_VALID) = '1' then
        v_writes := v_writes + 1;
      end if;
      if v_pins(RBB_QUIT) = '1' then
        log(ID_SEQUENCER, "remote_bitbang : " & integer'image(v_writes) & " pin writes (" &
            integer'image(v_writes/2) & " TCK cycles)");
        quit <= true;
        wait;
      end if;
    end loop;
  end process;

  ------------------------------------------------------------------
  -- End of simulation
  ------------------------------------------------------------------
  p_main : process
  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    disable_log_msg(ID_POS_ACK);
    if not VERBOSE then
      disable_log_msg(ID_BFM);
    end if;
    -- Warnings (debug memory window on the system bus) do not stop the simulation
    set_alert_stop_limit(TB_WARNING, 0);

    log(ID_LOG_HDR, "OpenOCD / GDB co-simulation : TCK_DIV = " & integer'image(TCK_DIV) &
        ", watchdog " & integer'image(TIMEOUT_US) & " us");
    wait until quit for TIMEOUT_US * 1 us;
    if quit then
      log(ID_LOG_HDR, "OpenOCD quit at " & integer'image(now / 1 ns) & " ns");
    else
      alert(TB_ERROR, "Watchdog : OpenOCD did not quit before " & integer'image(TIMEOUT_US) & " us");
    end if;

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture tb;
