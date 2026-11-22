-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_exc_trigger_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_exc_trigger_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario exc_trigger of tb_WardRV_debug : EBREAK_EXCEPTION, M-mode trigger (dmode 0) :
--              breakpoint exception
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

package tb_WardRV_debug_exc_trigger_pkg is

  -- EBREAK_EXCEPTION, M-mode trigger (dmode 0) :
  -- breakpoint exception
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_exc_trigger_pkg;

package body tb_WardRV_debug_exc_trigger_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
    variable v_mem  : std_logic_vector(32-1 downto 0);
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_if);
    halt     (jtag_if);
    reg_write(jtag_if, TDATA2, C_SW_COUNTER);
    reg_write(jtag_if, TDATA1, x"60000044");           -- dmode 0, m, execute
    reg_check(jtag_if, TDATA1, x"60000044", "tdata1 : trigger of M-mode, action 0");
    mem_read (jtag_if, C_COUNTER, v_mem);
    run_for  (jtag_if, 5 us, "trigger of M-mode");
    reg_read (jtag_if, S3, v_data);
    check_value(unsigned(v_data) > 1, ERROR, "breakpoint exception at each loop", C_TB_SCOPE);
    reg_check(jtag_if, MCAUSE, x"00000003", "mcause : breakpoint");
    mem_read (jtag_if, C_COUNTER, v_data);
    check_value(v_data, v_mem, ERROR, "store never executed (skipped by the handler)", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_exc_trigger_pkg;
