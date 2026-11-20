-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_reset_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_reset_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario reset of tb_WardRV_dbg_dm : after reset : dmcontrol, dmstatus, abstractcs,
--              sbcs, hartinfo, haltsum0
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

package tb_WardRV_dbg_dm_reset_pkg is

  -- after reset : dmcontrol, dmstatus, abstractcs,
  -- sbcs, hartinfo, haltsum0
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_reset_pkg;

package body tb_WardRV_dbg_dm_reset_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_check(clk, ini, tgt, DM_DMCONTROL , x"00000000", "dmcontrol : dmactive = 0 after reset");
    dmi_write(clk, ini, tgt, DM_DMCONTROL , x"00000001");          -- dmactive
    dmi_check(clk, ini, tgt, DM_DMSTATUS  , x"004C0CA3", "dmstatus : impebreak, havereset, running, authenticated, hasresethaltreq, version 3");
    dmi_check(clk, ini, tgt, DM_ABSTRACTCS, x"02000002", "abstractcs : progbufsize 2, not busy, cmderr 0, datacount 2");
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20040407", "sbcs : sbversion 1, sbaccess 32, sbasize 32, sbaccess 8 / 16 / 32");
    dmi_check(clk, ini, tgt, DM_HARTINFO  , x"00112B00", "hartinfo : nscratch 1, dataaccess, datasize 2, dataaddr 0xB00");
    dmi_check(clk, ini, tgt, DM_HALTSUM0  , x"00000000", "haltsum0 : running");
    check_value(tgt.hart_req.haltreq, '0', ERROR, "no haltreq to the hart", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_dm_reset_pkg;
