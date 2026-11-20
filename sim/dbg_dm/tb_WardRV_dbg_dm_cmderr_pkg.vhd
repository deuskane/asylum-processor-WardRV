-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_cmderr_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_cmderr_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario cmderr of tb_WardRV_dbg_dm : not supported, exception, halt / resume, busy, W1C
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

package tb_WardRV_dbg_dm_cmderr_pkg is

  -- not supported, exception, halt / resume, busy, W1C
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_cmderr_pkg;

package body tb_WardRV_dbg_dm_cmderr_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
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

end package body tb_WardRV_dbg_dm_cmderr_pkg;
