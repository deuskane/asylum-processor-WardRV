-------------------------------------------------------------------------------
-- Title      : WardRV Debug - OpenOCD + GDB co-simulation testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_openocd.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: WardRV_fsm (DEBUG_ENABLE, RESETHALTREQ_ENABLE, NB_TRIGGERS) +
--              WardRV_debug (PROGBUF_SIZE, DATA_COUNT, SBA_ENABLE) driven
--              by a real debugger : OpenOCD (remote_bitbang adapter) talks to
--              sim/openocd/remote_bitbang.c, called at each clk cycle
--              through VHPIDIRECT (TCK = clk / 2), and GDB runs
--              sim/openocd/scenario.py (hooks in sim/openocd/cosim.sh).
--              - The debug options are written in cosim.cfg for the
--                scenario, which checks them against OpenOCD.
--              - RAM of 64 KB at C_FIRMWARE_ADDR loaded with FIRMWARE_FILE,
--                shared by the core and the System Bus Access, answer in one
--                cycle. ndmreset resets the core, not the debug unit.
--              - End when OpenOCD quits, error after C_TIMEOUT.
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
use     std.textio.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.tb_WardRV_pkg.all;
use     work.rbb_pkg.all;

entity tb_WardRV_openocd is
  generic (
    PROGBUF_SIZE            : natural  := 2
   ;DATA_COUNT              : positive := 2
   ;SBA_ENABLE              : boolean  := true
   ;AARPOSTINCREMENT_ENABLE : boolean  := false
   ;TRIGGER_RANGE_ENABLE    : boolean  := false
   ;NB_TRIGGERS             : natural  := 2
   ;FIRMWARE_FILE           : string   := "openocd/fw/fw.hex"
   ;VERBOSE                 : boolean  := false
  );
end entity tb_WardRV_openocd;

architecture sim of tb_WardRV_openocd is

  constant C_SCOPE     : string  := "tb_WardRV_openocd";
  constant C_IDCODE    : std_logic_vector(32-1 downto 0) := x"1DEB0001";  -- sim/openocd/wardrv.cfg
  constant C_RAM_SIZE  : natural := 16#10000#;                             -- esw/openocd/link.ld
  constant C_TIMEOUT   : time    := 500 ms;

  signal   clk_i       : std_logic;
  signal   clk_ena     : boolean := true;
  signal   arst_b_i    : std_logic;
  signal   hart_rst_b  : std_logic;
  signal   ndmreset    : std_logic;
  signal   quit        : boolean := false;

  signal   mem         : ram_t(0 to C_RAM_SIZE-1);
  signal   imem_ini    : imem_ini_t;
  signal   imem_tgt    : imem_tgt_t;
  signal   dmem_ini    : dmem_ini_t;
  signal   dmem_tgt    : dmem_tgt_t;
  signal   sba_ini     : dmem_ini_t;
  signal   sba_tgt     : dmem_tgt_t;

  signal   jtag_ini    : jtag_ini_t := (tck => '0', trst_n => '1', tms => '1', tdi => '0');
  signal   jtag_tgt    : jtag_tgt_t;
  signal   dbg_hart_req: dbg_hart_req_t;
  signal   dbg_mem_ini : dmem_ini_t;
  signal   dbg_mem_tgt : dmem_tgt_t;

begin  -- architecture sim

  clock_generator(clk_i, clk_ena, C_CLK_PERIOD, "TB Clock");
  arst_b_i   <= '0', '1' after 100 ns;
  hart_rst_b <= arst_b_i and not ndmreset;

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  ins_core : entity asylum.WardRV_fsm
    generic map
    (HARTID              => C_HARTID
    ,RESET_ADDR          => C_FIRMWARE_ADDR
    ,VERBOSE             => VERBOSE
    ,DEBUG_ENABLE        => true
    ,NB_TRIGGERS         => NB_TRIGGERS
    ,RESETHALTREQ_ENABLE => true
    ,TRIGGER_RANGE_ENABLE => TRIGGER_RANGE_ENABLE
    )
    port map
    (clk_i               => clk_i
    ,arst_b_i            => hart_rst_b
    ,imem_ini_o          => imem_ini
    ,imem_tgt_i          => imem_tgt
    ,dmem_ini_o          => dmem_ini
    ,dmem_tgt_i          => dmem_tgt
    ,meip_i              => '0'
    ,dbg_hart_req_i      => dbg_hart_req
    ,dbg_mem_ini_o       => dbg_mem_ini
    ,dbg_mem_tgt_i       => dbg_mem_tgt
    );

  ins_debug : entity asylum.WardRV_debug
    generic map
    (IDCODE_VALUE            => C_IDCODE
    ,PROGBUF_SIZE            => PROGBUF_SIZE
    ,DATA_COUNT              => DATA_COUNT
    ,SBA_ENABLE              => SBA_ENABLE
    ,RESETHALTREQ_ENABLE     => true
    ,AARPOSTINCREMENT_ENABLE => AARPOSTINCREMENT_ENABLE
    )
    port map
    (jtag_ini_i              => jtag_ini
    ,jtag_tgt_o              => jtag_tgt
    ,clk_i                   => clk_i
    ,arst_b_i                => arst_b_i
    ,hart_rst_b_i            => hart_rst_b
    ,dbg_hart_req_o          => dbg_hart_req
    ,dbg_mem_ini_i           => dbg_mem_ini
    ,dbg_mem_tgt_o           => dbg_mem_tgt
    ,ndmreset_o              => ndmreset
    ,sba_dmem_ini_o          => sba_ini
    ,sba_dmem_tgt_i          => sba_tgt
    );

  -----------------------------------------------------------------------------
  -- RAM : core (imem, dmem) and System Bus Access, answer in one cycle
  -----------------------------------------------------------------------------
  p_mem : process
    variable v_rdata : std_logic_vector(32-1 downto 0);
  begin
    init_ram(FIRMWARE_FILE, mem);
    loop
      wait until rising_edge(clk_i);
      imem_tgt <= (ready => '0', inst  => (others => '0'));
      dmem_tgt <= DMEM_TGT_NULL;
      sba_tgt  <= DMEM_TGT_NULL;
      if imem_ini.valid = '1'
      then
        read_mem(mem, imem_ini.addr, v_rdata, VERBOSE);
        imem_tgt <= (ready => '1', inst => v_rdata);
      end if;
      if dmem_ini.valid = '1' and dmem_ini.we = '1'
      then
        write_mem(mem, dmem_ini.addr, dmem_ini.wdata, dmem_ini.be, VERBOSE);
        dmem_tgt.ready <= '1';
      elsif dmem_ini.valid = '1'
      then
        read_mem(mem, dmem_ini.addr, v_rdata, VERBOSE);
        dmem_tgt <= (ready => '1', rdata => v_rdata, err => '0');
      end if;
      if sba_ini.valid = '1' and sba_tgt.ready = '0' and sba_ini.we = '1'
      then
        write_mem(mem, sba_ini.addr, sba_ini.wdata, sba_ini.be, VERBOSE);
        sba_tgt.ready <= '1';
      elsif sba_ini.valid = '1' and sba_tgt.ready = '0'
      then
        read_mem(mem, sba_ini.addr, v_rdata, VERBOSE);
        sba_tgt <= (ready => '1', rdata => v_rdata, err => '0');
      end if;
    end loop;
  end process;

  -----------------------------------------------------------------------------
  -- JTAG : OpenOCD remote_bitbang, one call per clk cycle
  -----------------------------------------------------------------------------
  p_rbb : process
    variable v_pins : std_logic_vector(RBB_QUIT downto 0);
    variable v_tdo  : integer;
  begin
    wait until arst_b_i = '1';
    while not quit loop
      wait until rising_edge(clk_i);
      -- TDO is driven in Shift-IR / Shift-DR only (pull-up otherwise)
      v_tdo  := 0 when jtag_tgt.tdo_en = '1' and jtag_tgt.tdo = '0' else 1;
      v_pins := std_logic_vector(to_unsigned(rbb_tick(v_tdo), RBB_QUIT+1));
      jtag_ini.tck <= v_pins(RBB_TCK);
      jtag_ini.tms <= v_pins(RBB_TMS);
      jtag_ini.tdi <= v_pins(RBB_TDI);
      quit         <= v_pins(RBB_QUIT) = '1';
    end loop;
    wait;
  end process;

  -----------------------------------------------------------------------------
  -- Debug options for the scenario, end of the simulation
  -----------------------------------------------------------------------------
  p_main : process
    file     f_cfg  : text;
    variable v_line : line;
  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);

    file_open(f_cfg, "cosim.cfg", write_mode);
    write(v_line, "PROGBUF_SIZE=" & integer'image(PROGBUF_SIZE));               writeline(f_cfg, v_line);
    write(v_line, "DATA_COUNT="   & integer'image(DATA_COUNT));                 writeline(f_cfg, v_line);
    write(v_line, "SBA_ENABLE="   & integer'image(boolean'pos(SBA_ENABLE)));    writeline(f_cfg, v_line);
    write(v_line, "NB_TRIGGERS="  & integer'image(NB_TRIGGERS));                writeline(f_cfg, v_line);
    file_close(f_cfg);
    log(ID_LOG_HDR, "Co-simulation : PROGBUF_SIZE " & integer'image(PROGBUF_SIZE) &
        ", DATA_COUNT " & integer'image(DATA_COUNT) & ", SBA_ENABLE " & boolean'image(SBA_ENABLE) &
        ", NB_TRIGGERS " & integer'image(NB_TRIGGERS) & ", AARPOSTINCREMENT_ENABLE " & boolean'image(AARPOSTINCREMENT_ENABLE) &
        ", TRIGGER_RANGE_ENABLE " & boolean'image(TRIGGER_RANGE_ENABLE), C_SCOPE);

    wait until quit for C_TIMEOUT;
    if quit
    then
      log(ID_LOG_HDR, "OpenOCD quit at " & integer'image(now / 1 us) & " us", C_SCOPE);
    else
      alert(TB_ERROR, "Timeout : OpenOCD still connected", C_SCOPE);
    end if;
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture sim;
