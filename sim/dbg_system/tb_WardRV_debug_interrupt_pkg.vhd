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

library asylum;
use     asylum.jtag_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_interrupt_pkg is

  -- interrupt pending in debug mode, step with and
  -- without dcsr.stepie
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_interrupt_pkg;

package body tb_WardRV_debug_interrupt_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_dpc : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "Interrupt pending in debug mode : masked", C_TB_SCOPE);
    reg_read   (jtag_ini, jtag_tgt, DPC, v_dpc);
    mem_write  (jtag_ini, jtag_tgt, C_MEIP_ADDR, x"00000001");     -- meip_i = 1
    reg_write  (jtag_ini, jtag_tgt, MSTATUS, x"00001808");         -- MIE written in debug mode
    wait for 5 us;
    wait_halted(jtag_ini, jtag_tgt, "interrupt pending");
    reg_check  (jtag_ini, jtag_tgt, S2 , x"00000000", "no interrupt taken in debug mode");
    reg_check  (jtag_ini, jtag_tgt, DPC, v_dpc     , "dpc unchanged");

    log(ID_LOG_HDR, "Step without dcsr.stepie : interrupt masked", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_STEP);
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "step");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_STEP, "step");
    reg_check  (jtag_ini, jtag_tgt, DPC, next_pc(v_dpc), "dpc of the next instruction");
    reg_check  (jtag_ini, jtag_tgt, S2 , x"00000000", "no interrupt taken during the step");

    log(ID_LOG_HDR, "Step with dcsr.stepie : halt in the handler", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_STEPIE);
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "step");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_STEP, "step");
    reg_check  (jtag_ini, jtag_tgt, DPC   , C_HANDLER  , "dpc = mtvec");
    reg_check  (jtag_ini, jtag_tgt, MCAUSE, x"8000000B", "mcause : external interrupt");

    log(ID_LOG_HDR, "Resume : the handler runs", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR, DCSR_NONE);
    run_for    (jtag_ini, jtag_tgt, 5 us, "handler");
    reg_check  (jtag_ini, jtag_tgt, S2, x"00000001", "one interrupt taken");
  end procedure;

end package body tb_WardRV_debug_interrupt_pkg;
