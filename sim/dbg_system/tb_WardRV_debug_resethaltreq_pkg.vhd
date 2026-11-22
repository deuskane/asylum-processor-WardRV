-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_resethaltreq_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_resethaltreq_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario resethaltreq of tb_WardRV_debug : ndmreset with resethaltreq (cause 5) and haltreq
--              (cause 3)
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

package tb_WardRV_debug_resethaltreq_pkg is

  -- ndmreset with resethaltreq (cause 5) and haltreq
  -- (cause 3)
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_resethaltreq_pkg;

package body tb_WardRV_debug_resethaltreq_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_ini, jtag_tgt);
    halt   (jtag_ini, jtag_tgt);
    for i in 0 to 1 loop
      -- t0 is not reset : it shows whether an instruction was executed
      reg_write(jtag_ini, jtag_tgt, T0, x"12345678");
      if i = 0
      then
        log(ID_LOG_HDR, "ndmreset with resethaltreq", C_TB_SCOPE);
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000009");  -- setresethaltreq
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000003");  -- ndmreset
      else
        log(ID_LOG_HDR, "ndmreset with haltreq", C_TB_SCOPE);
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"80000003");  -- haltreq, ndmreset
      end if;
      dmi_read   (jtag_ini, jtag_tgt, DM_DMSTATUS , v_data);
      check_value(v_data(13 downto 12), "11", ERROR, "dmstatus.unavail during ndmreset", C_TB_SCOPE);
      -- ndmreset released, haltreq kept
      if i = 0
      then
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000001");
      else
        dmi_write(jtag_ini, jtag_tgt, DM_DMCONTROL, x"80000001");
      end if;
      wait_halted(jtag_ini, jtag_tgt, "after the reset");
      dmi_write  (jtag_ini, jtag_tgt, DM_DMCONTROL, x"00000001");
      dmi_read   (jtag_ini, jtag_tgt, DM_DMSTATUS , v_data);
      check_value(v_data(19 downto 18), "11", ERROR, "dmstatus.havereset", C_TB_SCOPE);
      if i = 0
      then
        check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_RESETHALTREQ, "resethaltreq");
      else
        check_cause(jtag_ini, jtag_tgt, DCSR_CAUSE_HALTREQ     , "haltreq at reset");
      end if;
      reg_check  (jtag_ini, jtag_tgt, DPC, C_FIRMWARE_ADDR, "dpc = reset address");
      reg_check  (jtag_ini, jtag_tgt, T0 , x"12345678", "no instruction executed");
      dmi_write  (jtag_ini, jtag_tgt, DM_DMCONTROL, x"10000005");  -- ackhavereset, clrresethaltreq
      run_for    (jtag_ini, jtag_tgt, 5 us, "after the reset");
      reg_read   (jtag_ini, jtag_tgt, T0, v_data);
      check_value(unsigned(v_data) > 0 and unsigned(v_data) < 16#12345678#, ERROR, "the firmware restarted", C_TB_SCOPE);
    end loop;
  end procedure;

end package body tb_WardRV_debug_resethaltreq_pkg;
