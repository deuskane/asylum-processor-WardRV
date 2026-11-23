-------------------------------------------------------------------------------
-- Title      : tb_pkg_tests_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_pkg_tests_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenarios of tb_pkg : names (valid items of TESTS_LIST) and
--              dispatch (run_test) to the package of each scenario
--              (tb_pkg_<name>_pkg)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-23  1.0      mrosiere Created
-------------------------------------------------------------------------------

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.string_pkg.all;

use     work.tb_pkg_bfm_pkg.all;

package tb_pkg_tests_pkg is

  -- Names of the scenarios (valid items of TESTS_LIST)
  constant C_TESTS : string := "vote";

  -- Run the scenario name
  procedure run_test(name : string);

end package tb_pkg_tests_pkg;

package body tb_pkg_tests_pkg is

  procedure run_test(name : string) is
  begin
    if    string_eq(name, "vote")
    then
      work.tb_pkg_vote_pkg.run;
    else
      alert(TB_ERROR, "Unknown test " & name, C_TB_SCOPE);
    end if;
  end procedure;

end package body tb_pkg_tests_pkg;
