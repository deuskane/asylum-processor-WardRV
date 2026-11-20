-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_resethaltreq_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_resethaltreq_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario resethaltreq of tb_WardRV_dbg_dm : set / clrresethaltreq, hart reset (havereset,
--              unavail)
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

package tb_WardRV_dbg_dm_resethaltreq_pkg is

  -- set / clrresethaltreq, hart reset (havereset,
  -- unavail)
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_resethaltreq_pkg;

package body tb_WardRV_dbg_dm_resethaltreq_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"10000001");

    log(ID_LOG_HDR, "setresethaltreq / clrresethaltreq", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000009");
    check_value(tgt.hart_req.resethaltreq, '1', ERROR, "resethaltreq", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    check_value(tgt.hart_req.resethaltreq, '1', ERROR, "resethaltreq kept", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000005");
    check_value(tgt.hart_req.resethaltreq, '0', ERROR, "resethaltreq cleared", C_TB_SCOPE);

    log(ID_LOG_HDR, "Hart reset : unavail, havereset, leaves debug mode", C_TB_SCOPE);
    hart_write(clk, ini, tgt, DBG_HALTED);
    dmi_check (clk, ini, tgt, DM_DMSTATUS, x"004003A3", "halted");
    ini.hart_rst_b <= '0';
    for i in 1 to 4 loop
      wait until rising_edge(clk);
    end loop;
    dmi_check (clk, ini, tgt, DM_DMSTATUS, x"004C30A3", "unavail, havereset");
    ini.hart_rst_b <= '1';
    for i in 1 to 4 loop
      wait until rising_edge(clk);
    end loop;
    dmi_check (clk, ini, tgt, DM_DMSTATUS, x"004C0CA3", "running after the reset");
  end procedure;

end package body tb_WardRV_dbg_dm_resethaltreq_pkg;
