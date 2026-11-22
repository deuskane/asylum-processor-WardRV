-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_trigger_execute_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_trigger_execute_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario trigger_execute of tb_WardRV_debug : tselect / tinfo / tdata1 WARL (no range, no
--              chain), execute trigger
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

package tb_WardRV_debug_trigger_execute_pkg is

  -- tselect / tinfo / tdata1 WARL (no range, no
  -- chain), execute trigger
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_trigger_execute_pkg;

package body tb_WardRV_debug_trigger_execute_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
    variable v_mem : std_logic_vector(32-1 downto 0);
    variable v_t0  : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_if);
    halt   (jtag_if);

    log(ID_LOG_HDR, "tselect, tinfo, tdata1", C_TB_SCOPE);
    reg_check(jtag_if, TSELECT, x"00000000", "tselect after reset");
    reg_write(jtag_if, TSELECT, std_logic_vector(to_unsigned(C_NB_TRIGGERS, 32)));
    reg_check(jtag_if, TSELECT, x"00000000", "tselect : index out of range ignored");
    reg_write(jtag_if, TSELECT, std_logic_vector(to_unsigned(C_NB_TRIGGERS-1, 32)));
    reg_check(jtag_if, TSELECT, std_logic_vector(to_unsigned(C_NB_TRIGGERS-1, 32)), "last trigger");
    reg_check(jtag_if, TINFO  , x"01000040", "tinfo : version 1, mcontrol6");
    reg_check(jtag_if, TDATA1 , x"60000000", "tdata1 after reset");
    reg_write(jtag_if, TSELECT, x"00000000");

    log(ID_LOG_HDR, "dmode = 0 without EBREAK_EXCEPTION : the trigger cannot fire", C_TB_SCOPE);
    reg_write(jtag_if, TDATA2 , C_SW_COUNTER);
    reg_write(jtag_if, TDATA1 , x"60001044");          -- dmode 0, action 1, m, execute
    reg_check(jtag_if, TDATA1 , x"60000004", "tdata1 : action = dmode = 0, m = 0");
    run_for  (jtag_if, 5 us, "trigger of M-mode");

    log(ID_LOG_HDR, "Execute trigger", C_TB_SCOPE);
    reg_write  (jtag_if, TDATA1, x"68001944");         -- chain, match >= : not implemented
    reg_check  (jtag_if, TDATA1, x"68001044", "tdata1 : match equal, no chain");
    reg_write  (jtag_if, TDATA1, x"68001044");         -- dmode, action 1, m, execute
    reg_check  (jtag_if, TDATA1, x"68001044", "tdata1");
    mem_read   (jtag_if, C_COUNTER, v_mem);
    reg_write  (jtag_if, DPC   , C_LOOP);
    reg_read   (jtag_if, T0    , v_t0);
    resume     (jtag_if);
    wait_halted(jtag_if, "execute trigger");
    check_cause(jtag_if, DCSR_CAUSE_TRIGGER, "execute trigger");
    reg_check  (jtag_if, DPC, C_SW_COUNTER, "dpc = address of the instruction");
    reg_check  (jtag_if, T0 , std_logic_vector(unsigned(v_t0) + 1), "addi before the trigger executed");
    mem_read   (jtag_if, C_COUNTER, v_t0);
    check_value(v_t0, v_mem, ERROR, "store not executed (timing before)", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_trigger_execute_pkg;
