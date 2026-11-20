-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_progbuf0_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_progbuf0_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario progbuf0 of tb_WardRV_dbg_dm : Debug Module without program buffer
--              (PROGBUF_SIZE = 0)
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

use     work.tb_WardRV_dbg_dm_bfm_pkg.all;

package tb_WardRV_dbg_dm_progbuf0_pkg is

  -- Debug Module without program buffer
  -- (PROGBUF_SIZE = 0)
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_progbuf0_pkg;

package body tb_WardRV_dbg_dm_progbuf0_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
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

end package body tb_WardRV_dbg_dm_progbuf0_pkg;
