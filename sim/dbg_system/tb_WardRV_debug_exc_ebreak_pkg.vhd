-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_exc_ebreak_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_exc_ebreak_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario exc_ebreak of tb_WardRV_debug : EBREAK_EXCEPTION, ebreak without ebreakm traps
--              (mcause 3), with ebreakm enters debug mode
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

package tb_WardRV_debug_exc_ebreak_pkg is

  -- EBREAK_EXCEPTION, ebreak without ebreakm traps
  -- (mcause 3), with ebreakm enters debug mode
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_exc_ebreak_pkg;

package body tb_WardRV_debug_exc_ebreak_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);

    log(ID_LOG_HDR, "ebreak without dcsr.ebreakm : breakpoint exception", C_TB_SCOPE);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");
    run_for    (jtag_ini, jtag_tgt, 5 us, "breakpoint exception");
    reg_check  (jtag_ini, jtag_tgt, S3    , x"00000001", "one breakpoint exception");
    reg_check  (jtag_ini, jtag_tgt, S1    , x"00000001", "the firmware went on after the ebreak");
    reg_check  (jtag_ini, jtag_tgt, MCAUSE, x"00000003", "mcause : breakpoint");
    reg_check  (jtag_ini, jtag_tgt, MEPC  , std_logic_vector(unsigned(C_EBREAK) + 4), "mepc = ebreak + 4 (handler)");

    log(ID_LOG_HDR, "ebreak with dcsr.ebreakm : debug mode", C_TB_SCOPE);
    reg_write  (jtag_ini, jtag_tgt, DCSR  , DCSR_EBREAKM);
    mem_write  (jtag_ini, jtag_tgt, C_FLAG, x"00000001");
    resume     (jtag_ini, jtag_tgt);
    wait_halted(jtag_ini, jtag_tgt, "ebreak");
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_EBREAK, "ebreak");
    reg_check  (jtag_ini, jtag_tgt, DPC, C_EBREAK  , "dpc = address of the ebreak");
    reg_check  (jtag_ini, jtag_tgt, S3 , x"00000001", "no breakpoint exception");
    mem_read   (jtag_ini, jtag_tgt, C_FLAG, v_data);
    check_value(v_data, x"00000001", ERROR, "flag not cleared yet", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_exc_ebreak_pkg;
