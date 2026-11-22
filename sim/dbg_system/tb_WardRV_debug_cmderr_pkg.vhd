-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_cmderr_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_cmderr_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario cmderr of tb_WardRV_debug : register that does not exist, command while running
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_debug_pkg.vhd)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum_uvvm;
use     asylum_uvvm.jtag_bfm_pkg.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_cmderr_pkg is

  -- register that does not exist, command while running
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_cmderr_pkg;

package body tb_WardRV_debug_cmderr_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
  begin
    connect(jtag_if);
    halt   (jtag_if);
    dmi_write   (jtag_if, DM_COMMAND, x"00221020");   -- f0
    wait_command(jtag_if, CMDERR_EXCEPTION, "register that does not exist");
    resume      (jtag_if);
    dmi_write   (jtag_if, DM_COMMAND, x"00221005");
    wait_command(jtag_if, CMDERR_HALTRESUME, "command while running");
  end procedure;

end package body tb_WardRV_debug_cmderr_pkg;
