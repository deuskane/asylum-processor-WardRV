-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_postincrement_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_postincrement_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario postincrement of tb_WardRV_dbg_dm : aarpostincrement (AARPOSTINCREMENT_ENABLE), with
--              abstractauto
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

package tb_WardRV_dbg_dm_postincrement_pkg is

  -- aarpostincrement (AARPOSTINCREMENT_ENABLE), with
  -- abstractauto
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_postincrement_pkg;

package body tb_WardRV_dbg_dm_postincrement_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
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

end package body tb_WardRV_dbg_dm_postincrement_pkg;
