-------------------------------------------------------------------------------
-- Title      : tb_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Self-checking UVVM testbench of the functions of WardRV_pkg.
--              TESTS_LIST : scenarios run in sequence, separated by ','
--                           (tb_pkg_tests_pkg) ; "" : build and
--                           elaboration only
--              Functions only : no DUT, no clock and no reset, each scenario
--              uses its own variables.
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

use     work.tb_pkg_tests_pkg.all;

entity tb_pkg is
  generic (
    TESTS_LIST        : string    := ""        -- Scenarios, e.g. "vote"
  );
end entity tb_pkg;

architecture sim of tb_pkg is

  constant C_SCOPE    : string  := "tb_pkg";

begin  -- architecture sim

  -----------------------------------------------------------------------------
  -- Sequencer
  -----------------------------------------------------------------------------
  p_sequencer : process
  begin
    log(ID_LOG_HDR, "TESTS_LIST : " & TESTS_LIST, C_SCOPE);

    -- Every name is checked before the first scenario
    for i in 1 to list_length(TESTS_LIST) loop
      if not list_contains(C_TESTS, list_item(TESTS_LIST, i))
      then
        alert(TB_ERROR, "Unknown test """ & list_item(TESTS_LIST, i) & """ (tests : " & C_TESTS & ")", C_SCOPE);
      end if;
    end loop;

    for i in 1 to list_length(TESTS_LIST) loop
      log(ID_LOG_HDR, "TEST " & integer'image(i) & "/" & integer'image(list_length(TESTS_LIST)) & " : " & list_item(TESTS_LIST, i), C_SCOPE);
      run_test(list_item(TESTS_LIST, i));
      wait for 1 ns;
    end loop;

    report_alert_counters(FINAL);
    std.env.stop;
    wait;
  end process p_sequencer;

end architecture sim;
