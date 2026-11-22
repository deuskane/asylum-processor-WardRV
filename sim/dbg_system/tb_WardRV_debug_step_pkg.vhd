-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_step_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_step_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario step of tb_WardRV_debug : dcsr.step (cause 4, dpc of each instruction)
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

package tb_WardRV_debug_step_pkg is

  -- dcsr.step (cause 4, dpc of each instruction)
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_step_pkg;

package body tb_WardRV_debug_step_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
    variable v_dpc : std_logic_vector(32-1 downto 0) := C_LOOP;
    variable v_t0  : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_if);
    halt     (jtag_if);
    reg_write(jtag_if, DPC , C_LOOP);
    reg_write(jtag_if, DCSR, DCSR_STEP);
    for i in 1 to 6 loop
      reg_read   (jtag_if, T0, v_t0);
      resume     (jtag_if, "step");
      wait_halted(jtag_if, "step");
      check_cause(jtag_if, DCSR_CAUSE_STEP, "step");
      reg_check  (jtag_if, DPC, next_pc(v_dpc), "dpc after the step of 0x" & to_hstring(v_dpc));
      if v_dpc = C_LOOP
      then
        reg_check(jtag_if, T0, std_logic_vector(unsigned(v_t0) + 1), "addi t0 executed once");
      end if;
      v_dpc := next_pc(v_dpc);
    end loop;
    reg_write(jtag_if, DCSR, DCSR_NONE);
    run_for  (jtag_if, 5 us, "without step");
  end procedure;

end package body tb_WardRV_debug_step_pkg;
