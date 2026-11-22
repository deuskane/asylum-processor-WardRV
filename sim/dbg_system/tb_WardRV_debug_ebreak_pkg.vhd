-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_ebreak_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_ebreak_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario ebreak of tb_WardRV_debug : ebreak with dcsr.ebreakm (cause 1), NOP without
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

package tb_WardRV_debug_ebreak_pkg is

  -- ebreak with dcsr.ebreakm (cause 1), NOP without
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_ebreak_pkg;

package body tb_WardRV_debug_ebreak_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_if);
    halt     (jtag_if);

    log(ID_LOG_HDR, "ebreak with dcsr.ebreakm : debug mode", C_TB_SCOPE);
    reg_write  (jtag_if, DCSR, DCSR_EBREAKM);
    mem_write  (jtag_if, C_FLAG, x"00000001");
    resume     (jtag_if);
    wait_halted(jtag_if, "ebreak");
    check_cause(jtag_if, DCSR_CAUSE_EBREAK, "ebreak");
    reg_check  (jtag_if, DPC, C_EBREAK, "dpc = address of the ebreak");
    reg_check  (jtag_if, S1 , x"00000000", "ebreak not executed");

    log(ID_LOG_HDR, "ebreak without dcsr.ebreakm : NOP", C_TB_SCOPE);
    reg_write  (jtag_if, DCSR, DCSR_NONE);
    run_for    (jtag_if, 5 us, "ebreak NOP");
    reg_check  (jtag_if, S1, x"00000001", "the firmware went on after the ebreak");
    mem_read   (jtag_if, C_FLAG, v_data);
    check_value(v_data, x"00000000", ERROR, "flag cleared by the firmware", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_ebreak_pkg;
