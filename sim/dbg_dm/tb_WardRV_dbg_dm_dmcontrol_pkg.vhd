-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_dmcontrol_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_dmcontrol_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario dmcontrol of tb_WardRV_dbg_dm : dmactive, ackhavereset, ndmreset, dmactive = 0
--              resets the DM
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

package tb_WardRV_dbg_dm_dmcontrol_pkg is

  -- dmactive, ackhavereset, ndmreset, dmactive = 0
  -- resets the DM
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_dmcontrol_pkg;

package body tb_WardRV_dbg_dm_dmcontrol_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    log(ID_LOG_HDR, "dmactive = 0 : DM in reset", C_TB_SCOPE);
    dmi_check(clk, ini, tgt, DM_DMCONTROL, x"00000000", "dmcontrol after reset");
    dmi_write(clk, ini, tgt, DM_DATA0    , x"0000CAFE");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000003");          -- ndmreset ignored
    dmi_check(clk, ini, tgt, DM_DMCONTROL, x"00000001", "dmactive only");
    dmi_check(clk, ini, tgt, DM_DATA0    , x"00000000", "data0 not written while dmactive = 0");

    log(ID_LOG_HDR, "ackhavereset", C_TB_SCOPE);
    dmi_check(clk, ini, tgt, DM_DMSTATUS  , x"004C0CA3", "dmstatus : havereset");
    dmi_write(clk, ini, tgt, DM_DMCONTROL , x"10000001");         -- ackhavereset
    dmi_check(clk, ini, tgt, DM_DMSTATUS  , x"00400CA3", "dmstatus : havereset acknowledged");

    log(ID_LOG_HDR, "ndmreset", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000003");
    dmi_check(clk, ini, tgt, DM_DMCONTROL, x"00000003", "dmcontrol.ndmreset");
    check_value(tgt.ndmreset, '1', ERROR, "ndmreset_o", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    check_value(tgt.ndmreset, '0', ERROR, "ndmreset_o released", C_TB_SCOPE);

    log(ID_LOG_HDR, "dmactive = 0 resets the DM", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DATA0    , x"0000CAFE");
    dmi_check(clk, ini, tgt, DM_DATA0    , x"0000CAFE", "data0");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"80000001");          -- haltreq
    check_value(tgt.hart_req.haltreq, '1', ERROR, "haltreq", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000000");
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");
    dmi_check(clk, ini, tgt, DM_DATA0    , x"00000000", "data0 reset by dmactive = 0");
    check_value(tgt.hart_req.haltreq, '0', ERROR, "haltreq reset by dmactive = 0", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_dm_dmcontrol_pkg;
