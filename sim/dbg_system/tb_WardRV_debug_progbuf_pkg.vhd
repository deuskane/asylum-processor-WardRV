-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_progbuf_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_progbuf_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario progbuf of tb_WardRV_debug : memory read / write through the program buffer
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
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;
use     work.tb_WardRV_pkg.all;

use     work.tb_WardRV_debug_bfm_pkg.all;

package tb_WardRV_debug_progbuf_pkg is

  -- memory read / write through the program buffer
  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t);

end package tb_WardRV_debug_progbuf_pkg;

package body tb_WardRV_debug_progbuf_pkg is

  procedure run(signal jtag_ini : out jtag_ini_t; signal jtag_tgt : in jtag_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
    variable v_t0   : std_logic_vector(32-1 downto 0);
  begin
    connect  (jtag_ini, jtag_tgt);
    halt     (jtag_ini, jtag_tgt);
    mem_write(jtag_ini, jtag_tgt, x"80001100", x"DEADBEEF");
    mem_read (jtag_ini, jtag_tgt, x"80001100", v_data);
    check_value(v_data, x"DEADBEEF", ERROR, "memory written and read through the program buffer", C_TB_SCOPE);
    mem_read (jtag_ini, jtag_tgt, C_FIRMWARE_ADDR, v_data);
    check_value(v_data, x"00000317", ERROR, "first instruction of the firmware", C_TB_SCOPE);
    reg_read (jtag_ini, jtag_tgt, T0, v_t0);
    mem_read (jtag_ini, jtag_tgt, C_COUNTER, v_data);
    check_value(v_data = v_t0 or unsigned(v_data) + 1 = unsigned(v_t0), ERROR,
                "counter in memory = t0 or t0 - 1", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_progbuf_pkg;
