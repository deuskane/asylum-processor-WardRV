-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_tests_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_tests_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_WardRV_dbg_dm : names (valid items of
--              TESTS_LIST), configurations where they apply and dispatch
--              (run_test) to the package of each scenario
--              (tb_WardRV_dbg_dm_<name>_pkg)
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
use     asylum.string_pkg.all;

use     work.tb_WardRV_dbg_dm_bfm_pkg.all;

package tb_WardRV_dbg_dm_tests_pkg is

  -- Names of the scenarios (valid items of TESTS_LIST)
  constant C_TESTS : string := "reset,dmcontrol,halt_resume,abstract,cmderr,autoexec,resethaltreq,sba,progbuf0,postincrement";

  -- progbuf0 needs PROGBUF_SIZE = 0, postincrement AARPOSTINCREMENT_ENABLE,
  -- the other scenarios the default generics
  function test_applies
    (constant name                    : string
    ;constant progbuf_size            : natural
    ;constant aarpostincrement_enable : boolean
    ) return boolean;

  -- Run the scenario name
  procedure run_test
    (constant name : in    string
    ;signal   clk  : in    std_logic
    ;signal   ini  : out   dm_ini_t
    ;signal   tgt  : in    dm_tgt_t
    );

end package tb_WardRV_dbg_dm_tests_pkg;

package body tb_WardRV_dbg_dm_tests_pkg is

  function test_applies
    (constant name                    : string
    ;constant progbuf_size            : natural
    ;constant aarpostincrement_enable : boolean
    ) return boolean is
  begin
    return string_eq(name, "progbuf0"     ) = (progbuf_size = 0) and
           string_eq(name, "postincrement") = aarpostincrement_enable;
  end function;

  procedure run_test
    (constant name : in    string
    ;signal   clk  : in    std_logic
    ;signal   ini  : out   dm_ini_t
    ;signal   tgt  : in    dm_tgt_t
    ) is
  begin
    if    string_eq(name, "reset")
    then
      work.tb_WardRV_dbg_dm_reset_pkg.run        (clk, ini, tgt);
    elsif string_eq(name, "dmcontrol")
    then
      work.tb_WardRV_dbg_dm_dmcontrol_pkg.run    (clk, ini, tgt);
    elsif string_eq(name, "halt_resume")
    then
      work.tb_WardRV_dbg_dm_halt_resume_pkg.run  (clk, ini, tgt);
    elsif string_eq(name, "abstract")
    then
      work.tb_WardRV_dbg_dm_abstract_pkg.run     (clk, ini, tgt);
    elsif string_eq(name, "cmderr")
    then
      work.tb_WardRV_dbg_dm_cmderr_pkg.run       (clk, ini, tgt);
    elsif string_eq(name, "autoexec")
    then
      work.tb_WardRV_dbg_dm_autoexec_pkg.run     (clk, ini, tgt);
    elsif string_eq(name, "resethaltreq")
    then
      work.tb_WardRV_dbg_dm_resethaltreq_pkg.run (clk, ini, tgt);
    elsif string_eq(name, "sba")
    then
      work.tb_WardRV_dbg_dm_sba_pkg.run          (clk, ini, tgt);
    elsif string_eq(name, "progbuf0")
    then
      work.tb_WardRV_dbg_dm_progbuf0_pkg.run     (clk, ini, tgt);
    elsif string_eq(name, "postincrement")
    then
      work.tb_WardRV_dbg_dm_postincrement_pkg.run(clk, ini, tgt);
    else
      alert(TB_ERROR, "Unknown test " & name, C_TB_SCOPE);
    end if;
  end procedure;

end package body tb_WardRV_dbg_dm_tests_pkg;
