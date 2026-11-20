-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_halt_resume_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_halt_resume_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario halt_resume of tb_WardRV_dbg_dm : haltreq, HALTED, resumereq, RESUMING, resumeack
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

package tb_WardRV_dbg_dm_halt_resume_pkg is

  -- haltreq, HALTED, resumereq, RESUMING, resumeack
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_halt_resume_pkg;

package body tb_WardRV_dbg_dm_halt_resume_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");          -- dmactive
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"10000001");          -- ackhavereset

    log(ID_LOG_HDR, "Halt", C_TB_SCOPE);
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"80000001");
    check_value(tgt.hart_req.haltreq, '1', ERROR, "haltreq to the hart", C_TB_SCOPE);
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"00400CA3", "running until HALTED");
    hart_write(clk, ini, tgt, DBG_HALTED);
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"004003A3", "allhalted");
    dmi_check (clk, ini, tgt, DM_HALTSUM0 , x"00000001", "haltsum0");
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"00000001");
    check_value(tgt.hart_req.haltreq, '0', ERROR, "haltreq cleared", C_TB_SCOPE);
    hart_check(clk, ini, tgt, DBG_GO    , x"00000000", "GO");
    hart_check(clk, ini, tgt, DBG_RESUME, x"00000000", "RESUME");

    log(ID_LOG_HDR, "Resume", C_TB_SCOPE);
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"C0000001");          -- resumereq with haltreq : ignored
    hart_check(clk, ini, tgt, DBG_RESUME  , x"00000000", "resumereq ignored with haltreq");
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"40000001");
    hart_check(clk, ini, tgt, DBG_RESUME  , x"00000001", "RESUME");
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"004003A3", "halted until RESUMING, no resumeack");
    hart_write(clk, ini, tgt, DBG_RESUMING);
    hart_check(clk, ini, tgt, DBG_RESUME  , x"00000000", "RESUME cleared");
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"00430CA3", "running, allresumeack");
    dmi_write (clk, ini, tgt, DM_DMCONTROL, x"40000001");          -- running : ignored
    dmi_check (clk, ini, tgt, DM_DMSTATUS , x"00430CA3", "resumereq ignored while running");
  end procedure;

end package body tb_WardRV_dbg_dm_halt_resume_pkg;
