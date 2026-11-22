-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_halt_resume_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_halt_resume_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario halt_resume of tb_WardRV_debug : haltreq (cause 3,
--              dpc in the loop), resume, dpc = next instruction (counter in
--              memory)
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

package tb_WardRV_debug_halt_resume_pkg is

  -- haltreq (cause 3, dpc in the loop), resume, dpc = next instruction
  -- (counter in memory)
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_halt_resume_pkg;

package body tb_WardRV_debug_halt_resume_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_dpc : std_logic_vector(32-1 downto 0);
    variable v_t0  : std_logic_vector(32-1 downto 0);
    variable v_t0b : std_logic_vector(32-1 downto 0);
    variable v_mem : std_logic_vector(32-1 downto 0);
    variable v_store : boolean;
  begin
    connect(jtag_ini, jtag_tgt);
    wait for 20 us;
    log(ID_LOG_HDR, "Halt", C_TB_SCOPE);
    halt       (jtag_ini, jtag_tgt);
    check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_HALTREQ, "haltreq");
    reg_read   (jtag_ini, jtag_tgt, DPC, v_dpc);
    check_value(unsigned(v_dpc) >= unsigned(C_LOOP) and unsigned(v_dpc) <= unsigned(C_BEQZ), ERROR,
                "dpc in the loop : 0x" & to_hstring(v_dpc), C_TB_SCOPE);
    reg_read   (jtag_ini, jtag_tgt, T0, v_t0);
    check_value(unsigned(v_t0) > 10, ERROR, "the counter runs : " & to_hstring(v_t0), C_TB_SCOPE);
    wait for 5 us;
    reg_check  (jtag_ini, jtag_tgt, T0, v_t0, "halted : the counter does not change");

    log(ID_LOG_HDR, "Resume", C_TB_SCOPE);
    run_for    (jtag_ini, jtag_tgt, 10 us, "resume");
    reg_read   (jtag_ini, jtag_tgt, T0, v_t0b);
    check_value(unsigned(v_t0b) > unsigned(v_t0) + 10, ERROR, "the counter ran after resume", C_TB_SCOPE);

    -- dpc is the next instruction, not executed yet : the counter in memory
    -- is t0 - 1 when dpc is the store of the counter, t0 otherwise. Halts at
    -- different times, until one of them stops before the store.
    log(ID_LOG_HDR, "dpc : next instruction (counter in memory)", C_TB_SCOPE);
    v_store := false;
    for i in 1 to 16 loop
      run_for    (jtag_ini, jtag_tgt, 1 us + i * 130 ns, "halt " & integer'image(i));
      reg_read   (jtag_ini, jtag_tgt, DPC, v_dpc);
      reg_read   (jtag_ini, jtag_tgt, T0 , v_t0);
      mem_read   (jtag_ini, jtag_tgt, C_COUNTER, v_mem);
      if v_dpc = C_SW_COUNTER
      then
        v_store := true;
        check_value(v_mem, std_logic_vector(unsigned(v_t0) - 1), ERROR, "dpc on the store : counter in memory = t0 - 1", C_TB_SCOPE);
      else
        check_value(v_mem, v_t0, ERROR, "dpc 0x" & to_hstring(v_dpc) & " : counter in memory = t0", C_TB_SCOPE);
      end if;
      exit when v_store;
    end loop;
    check_value(v_store, ERROR, "a halt stopped the core before the store of the counter", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_halt_resume_pkg;
