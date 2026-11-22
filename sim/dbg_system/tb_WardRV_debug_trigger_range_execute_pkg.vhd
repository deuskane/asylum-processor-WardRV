-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_trigger_range_execute_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_trigger_range_execute_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario trigger_range_execute of tb_WardRV_debug : TRIGGER_RANGE_ENABLE, chain >= A, < B on
--              the pc, chain WARL, chain of M-mode triggers (tdata2 written,
--              dmode not set)
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

package tb_WardRV_debug_trigger_range_execute_pkg is

  -- TRIGGER_RANGE_ENABLE, chain >= A, < B on
  -- the pc, chain WARL, chain of M-mode triggers (tdata2 written,
  -- dmode not set)
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_trigger_range_execute_pkg;

package body tb_WardRV_debug_trigger_range_execute_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
  begin
    connect    (jtag_if);
    halt       (jtag_if);

    log(ID_LOG_HDR, "chain of M-mode triggers : tdata2 writable, dmode not set", C_TB_SCOPE);
    reg_write  (jtag_if, TSELECT, x"00000000");
    reg_write  (jtag_if, TDATA1, x"60000804");          -- dmode 0, chain
    reg_check  (jtag_if, TDATA1, x"60000804", "tdata1 : chain of M-mode triggers");
    reg_write  (jtag_if, TSELECT, x"00000001");
    reg_write  (jtag_if, TDATA2, x"08000000");          -- bit 27 : dmode in tdata1
    reg_check  (jtag_if, TDATA2, x"08000000", "tdata2 : written after a chain of M-mode");
    reg_write  (jtag_if, TDATA1, x"680011C4");
    reg_check  (jtag_if, TDATA1, x"60000000", "tdata1 : dmode not set after a chain of M-mode");
    reg_write  (jtag_if, TSELECT, x"00000000");
    reg_write  (jtag_if, TDATA1, x"60000000");          -- No chain

    log(ID_LOG_HDR, "chain : not on the last trigger, not from M-mode into the debugger", C_TB_SCOPE);
    trigger_set(jtag_if, 1, x"680011C4", C_BEQZ);      -- pc < beqz
    reg_write  (jtag_if, TDATA1, x"680019C4");
    reg_check  (jtag_if, TDATA1, x"680011C4", "tdata1 : no chain on the last trigger");
    reg_write  (jtag_if, TSELECT, x"00000000");
    reg_write  (jtag_if, TDATA1, x"60000844");          -- dmode 0, chain
    reg_check  (jtag_if, TDATA1, x"60000004", "tdata1 : no chain from M-mode into the debugger");

    log(ID_LOG_HDR, "Range [lw, beqz) : addi and sw match the < trigger only", C_TB_SCOPE);
    trigger_set(jtag_if, 0, x"68001944", C_LW_FLAG);   -- pc >= lw, chain
    trigger_run(jtag_if, C_LOOP, C_LW_FLAG, "range trigger on the pc");
  end procedure;

end package body tb_WardRV_debug_trigger_range_execute_pkg;
