-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_jtag_bfm_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_bfm_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Constants shared by the scenarios of tb_WardRV_dbg_jtag (the
--              JTAG and DTM procedures are in WardRV_vips)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_dbg_jtag_pkg.vhd)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;

package tb_WardRV_dbg_jtag_bfm_pkg is

  constant C_IDCODE   : std_logic_vector(32-1 downto 0) := x"1DEB0001";
  constant C_TB_SCOPE : string := "tb_WardRV_dbg_jtag";

end package tb_WardRV_dbg_jtag_bfm_pkg;
