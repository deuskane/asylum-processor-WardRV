-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_tests_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_tests_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_WardRV_debug : names (valid items of
--              TESTS_LIST), configurations where they apply and dispatch
--              (run_test) to the package of each scenario
--              (tb_WardRV_debug_<name>_pkg)
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
use     asylum.WardRV_pkg.all;
use     asylum.string_pkg.all;

use     work.tb_WardRV_pkg.all;
use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_tests_pkg is

  -- Names of the scenarios (valid items of TESTS_LIST)
  constant C_TESTS       : string := "reset,halt_resume,registers,cmderr,progbuf,step,ebreak,trigger_execute,trigger_store,trigger_load,resethaltreq,interrupt," &
                                     "trigger_ge,trigger_lt,trigger_range_execute,trigger_range_store," &
                                     "exc_ebreak,exc_trigger";

  -- Scenarios of TRIGGER_RANGE_ENABLE, of EBREAK_EXCEPTION
  constant C_TESTS_RANGE : string := "trigger_ge,trigger_lt,trigger_range_execute,trigger_range_store";
  constant C_TESTS_EXC   : string := "exc_ebreak,exc_trigger";

  -- The scenarios of C_TESTS_RANGE need TRIGGER_RANGE_ENABLE, the ones of
  -- C_TESTS_EXC EBREAK_EXCEPTION, the other ones the default generics
  function test_applies
    (constant name                 : string
    ;constant ebreak_exception     : boolean
    ;constant trigger_range_enable : boolean
    ) return boolean;

  -- Run the scenario name
  procedure run_test
    (constant name     : in    string
    ;signal   jtag_ini : out   jtag_ini_t
    ;signal   jtag_tgt : in    jtag_tgt_t
    ;signal   mem      : in    ram_t
    );

end package tb_WardRV_debug_tests_pkg;

package body tb_WardRV_debug_tests_pkg is

  function test_applies
    (constant name                 : string
    ;constant ebreak_exception     : boolean
    ;constant trigger_range_enable : boolean
    ) return boolean is
  begin
    return list_contains(C_TESTS_EXC  , name) = ebreak_exception and
           list_contains(C_TESTS_RANGE, name) = trigger_range_enable;
  end function;

  procedure run_test
    (constant name     : in    string
    ;signal   jtag_ini : out   jtag_ini_t
    ;signal   jtag_tgt : in    jtag_tgt_t
    ;signal   mem      : in    ram_t
    ) is
  begin
    if    string_eq(name, "reset")
    then
      work.tb_WardRV_debug_reset_pkg.run                (jtag_ini, jtag_tgt, mem);
    elsif string_eq(name, "halt_resume")
    then
      work.tb_WardRV_debug_halt_resume_pkg.run          (jtag_ini, jtag_tgt);
    elsif string_eq(name, "registers")
    then
      work.tb_WardRV_debug_registers_pkg.run            (jtag_ini, jtag_tgt);
    elsif string_eq(name, "cmderr")
    then
      work.tb_WardRV_debug_cmderr_pkg.run               (jtag_ini, jtag_tgt);
    elsif string_eq(name, "progbuf")
    then
      work.tb_WardRV_debug_progbuf_pkg.run              (jtag_ini, jtag_tgt);
    elsif string_eq(name, "step")
    then
      work.tb_WardRV_debug_step_pkg.run                 (jtag_ini, jtag_tgt);
    elsif string_eq(name, "ebreak")
    then
      work.tb_WardRV_debug_ebreak_pkg.run               (jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_execute")
    then
      work.tb_WardRV_debug_trigger_execute_pkg.run      (jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_store")
    then
      work.tb_WardRV_debug_trigger_store_pkg.run        (jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_load")
    then
      work.tb_WardRV_debug_trigger_load_pkg.run         (jtag_ini, jtag_tgt);
    elsif string_eq(name, "resethaltreq")
    then
      work.tb_WardRV_debug_resethaltreq_pkg.run         (jtag_ini, jtag_tgt);
    elsif string_eq(name, "interrupt")
    then
      work.tb_WardRV_debug_interrupt_pkg.run            (jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_ge")
    then
      work.tb_WardRV_debug_trigger_ge_pkg.run           (jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_lt")
    then
      work.tb_WardRV_debug_trigger_lt_pkg.run           (jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_range_execute")
    then
      work.tb_WardRV_debug_trigger_range_execute_pkg.run(jtag_ini, jtag_tgt);
    elsif string_eq(name, "trigger_range_store")
    then
      work.tb_WardRV_debug_trigger_range_store_pkg.run  (jtag_ini, jtag_tgt);
    elsif string_eq(name, "exc_ebreak")
    then
      work.tb_WardRV_debug_exc_ebreak_pkg.run           (jtag_ini, jtag_tgt);
    elsif string_eq(name, "exc_trigger")
    then
      work.tb_WardRV_debug_exc_trigger_pkg.run          (jtag_ini, jtag_tgt);
    else
      alert(TB_ERROR, "Unknown test " & name, C_TB_SCOPE);
    end if;
  end procedure;

end package body tb_WardRV_debug_tests_pkg;
