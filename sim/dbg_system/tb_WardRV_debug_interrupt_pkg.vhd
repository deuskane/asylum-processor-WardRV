-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_interrupt_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_interrupt_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario interrupt of tb_WardRV_debug : interrupt pending in debug mode, step with and
--              without dcsr.stepie
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

package tb_WardRV_debug_interrupt_pkg is

  -- interrupt pending in debug mode, step with and
  -- without dcsr.stepie
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_interrupt_pkg;

package body tb_WardRV_debug_interrupt_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
    variable v_dpc : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_if);
    halt   (jtag_if);

    log(ID_LOG_HDR, "Interrupt pending in debug mode : masked", C_TB_SCOPE);
    reg_read   (jtag_if, DPC, v_dpc);
    mem_write  (jtag_if, C_MEIP_ADDR, x"00000001");     -- meip_i = 1
    reg_write  (jtag_if, MSTATUS, x"00001808");         -- MIE written in debug mode
    wait for 5 us;
    wait_halted(jtag_if, "interrupt pending");
    reg_check  (jtag_if, S2 , x"00000000", "no interrupt taken in debug mode");
    reg_check  (jtag_if, DPC, v_dpc     , "dpc unchanged");

    log(ID_LOG_HDR, "Step without dcsr.stepie : interrupt masked", C_TB_SCOPE);
    reg_write  (jtag_if, DCSR, DCSR_STEP);
    resume     (jtag_if);
    wait_halted(jtag_if, "step");
    check_cause(jtag_if, DCSR_CAUSE_STEP, "step");
    reg_check  (jtag_if, DPC, next_pc(v_dpc), "dpc of the next instruction");
    reg_check  (jtag_if, S2 , x"00000000", "no interrupt taken during the step");

    log(ID_LOG_HDR, "Step with dcsr.stepie : halt in the handler", C_TB_SCOPE);
    reg_write  (jtag_if, DCSR, DCSR_STEPIE);
    resume     (jtag_if);
    wait_halted(jtag_if, "step");
    check_cause(jtag_if, DCSR_CAUSE_STEP, "step");
    reg_check  (jtag_if, DPC   , C_HANDLER  , "dpc = mtvec");
    reg_check  (jtag_if, MCAUSE, x"8000000B", "mcause : external interrupt");

    log(ID_LOG_HDR, "Resume : the handler runs", C_TB_SCOPE);
    reg_write  (jtag_if, DCSR, DCSR_NONE);
    run_for    (jtag_if, 5 us, "handler");
    reg_check  (jtag_if, S2, x"00000001", "one interrupt taken");
  end procedure;

end package body tb_WardRV_debug_interrupt_pkg;
