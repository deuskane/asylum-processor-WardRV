-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_jtag_tests_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_tests_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_WardRV_dbg_jtag : names (valid items of
--              TESTS_LIST) and dispatch (run_test) to the package of each
--              scenario (tb_WardRV_dbg_jtag_<name>_pkg)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (TESTS_LIST)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.string_pkg.all;

use     work.tb_WardRV_dbg_jtag_bfm_pkg.all;

package tb_WardRV_dbg_jtag_tests_pkg is

  -- Names of the scenarios (valid items of TESTS_LIST)
  constant C_TESTS : string := "reset,tap,dmi,busy";

  -- Run the scenario name
  procedure run_test
    (constant name      : in    string
    ;signal   jtag_ini  : out   jtag_ini_t
    ;signal   jtag_tgt  : in    jtag_tgt_t
    ;signal   dm_nb_req : in    natural
    );

end package tb_WardRV_dbg_jtag_tests_pkg;

package body tb_WardRV_dbg_jtag_tests_pkg is

  procedure run_test
    (constant name      : in    string
    ;signal   jtag_ini  : out   jtag_ini_t
    ;signal   jtag_tgt  : in    jtag_tgt_t
    ;signal   dm_nb_req : in    natural
    ) is
  begin
    if    string_eq(name, "reset")
    then
      work.tb_WardRV_dbg_jtag_reset_pkg.run(jtag_ini, jtag_tgt);
    elsif string_eq(name, "tap")
    then
      work.tb_WardRV_dbg_jtag_tap_pkg.run  (jtag_ini, jtag_tgt);
    elsif string_eq(name, "dmi")
    then
      work.tb_WardRV_dbg_jtag_dmi_pkg.run  (jtag_ini, jtag_tgt, dm_nb_req);
    elsif string_eq(name, "busy")
    then
      work.tb_WardRV_dbg_jtag_busy_pkg.run (jtag_ini, jtag_tgt, dm_nb_req);
    else
      alert(TB_ERROR, "Unknown test " & name, C_TB_SCOPE);
    end if;
  end procedure;

end package body tb_WardRV_dbg_jtag_tests_pkg;
