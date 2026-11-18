-------------------------------------------------------------------------------
-- Title      : WardRV Debug - Debug Module unit testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: WardRV_dbg_dm (DATA_COUNT 2, SBA, resethaltreq) driven on its
--              DMI and debug memory ports, with a memory on the System Bus
--              Access (64 words, one wait state). Scenarios in
--              tb_WardRV_dbg_dm_pkg (TEST). PROGBUF_SIZE : 2, or 0 for the
--              scenario progbuf0.
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

use     work.tb_WardRV_dbg_dm_pkg.all;

entity tb_WardRV_dbg_dm is
  generic (
    TEST           : string  := "dmcontrol"
   ;PROGBUF_SIZE   : natural := 2
   ;VERBOSE        : boolean := false
  );
end entity tb_WardRV_dbg_dm;

architecture sim of tb_WardRV_dbg_dm is

  constant C_SCOPE     : string := "tb_WardRV_dbg_dm";

  signal   clk_i       : std_logic;
  signal   clk_ena     : boolean := true;
  signal   arst_b_i    : std_logic;
  signal   ini         : dm_ini_t := DM_INI_IDLE;
  signal   tgt         : dm_tgt_t;
  signal   sba_ini     : dmem_ini_t;
  signal   sba_tgt     : dmem_tgt_t;
  signal   sba_mem     : dbg_words_t(0 to 63) := (others => (others => '0'));

begin  -- architecture sim

  clock_generator(clk_i, clk_ena, 10 ns, "TB Clock");
  arst_b_i <= '0', '1' after 100 ns;

  -----------------------------------------------------------------------------
  -- DUT
  -----------------------------------------------------------------------------
  dut : entity asylum.WardRV_dbg_dm
    generic map
    (PROGBUF_SIZE        => PROGBUF_SIZE
    ,DATA_COUNT          => 2
    ,SBA_ENABLE          => true
    ,RESETHALTREQ_ENABLE => true
    )
    port map
    (clk_i               => clk_i
    ,arst_b_i            => arst_b_i
    ,dmi_req_i           => ini.dmi_req
    ,dmi_rdata_o         => tgt.dmi_rdata
    ,hart_rst_b_i        => ini.hart_rst_b
    ,dbg_hart_req_o      => tgt.hart_req
    ,dbg_mem_ini_i       => ini.mem_ini
    ,dbg_mem_tgt_o       => tgt.mem_tgt
    ,ndmreset_o          => tgt.ndmreset
    ,sba_dmem_ini_o      => sba_ini
    ,sba_dmem_tgt_i      => sba_tgt
    );

  -----------------------------------------------------------------------------
  -- System bus memory : ready one cycle after valid
  -----------------------------------------------------------------------------
  p_sba_mem : process(clk_i)
    variable v_word : natural range 0 to 63;
  begin
    if rising_edge(clk_i)
    then
      sba_tgt <= DMEM_TGT_NULL;
      if sba_ini.valid = '1' and sba_tgt.ready = '0'
      then
        v_word := to_integer(unsigned(sba_ini.addr(7 downto 2)));
        for b in 0 to 3 loop
          if sba_ini.we = '1' and sba_ini.be(b) = '1'
          then
            sba_mem(v_word)(8*b+7 downto 8*b) <= sba_ini.wdata(8*b+7 downto 8*b);
          end if;
        end loop;
        sba_tgt.rdata <= sba_mem(v_word);
        sba_tgt.ready <= '1';
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Sequencer
  -----------------------------------------------------------------------------
  p_main : process
  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    if not VERBOSE
    then
      disable_log_msg(ID_POS_ACK);
    end if;

    log(ID_LOG_HDR, "TEST " & TEST & ", PROGBUF_SIZE " & integer'image(PROGBUF_SIZE), C_SCOPE);
    if (TEST = "progbuf0") /= (PROGBUF_SIZE = 0)
    then
      alert(TB_ERROR, "TEST progbuf0 needs PROGBUF_SIZE = 0, the others PROGBUF_SIZE = 2", C_SCOPE);
    end if;
    wait until arst_b_i = '1';
    wait until rising_edge(clk_i);

    if    TEST = "dmcontrol"
    then
      test_dmcontrol   (clk_i, ini, tgt);
    elsif TEST = "halt_resume"
    then
      test_halt_resume (clk_i, ini, tgt);
    elsif TEST = "abstract"
    then
      test_abstract    (clk_i, ini, tgt);
    elsif TEST = "cmderr"
    then
      test_cmderr      (clk_i, ini, tgt);
    elsif TEST = "autoexec"
    then
      test_autoexec    (clk_i, ini, tgt);
    elsif TEST = "resethaltreq"
    then
      test_resethaltreq(clk_i, ini, tgt);
    elsif TEST = "sba"
    then
      test_sba         (clk_i, ini, tgt);
    elsif TEST = "progbuf0"
    then
      test_progbuf0    (clk_i, ini, tgt);
    else
      alert(TB_ERROR, "Unknown TEST " & TEST, C_SCOPE);
    end if;

    wait for 100 ns;
    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process;

end architecture sim;
