-------------------------------------------------------------------------------
-- Title      : tb_WardRV_debug_registers_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_debug_registers_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario registers of tb_WardRV_debug : GPR / CSR access with abstract commands, dpc write
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

package tb_WardRV_debug_registers_pkg is

  -- GPR / CSR access with abstract commands, dpc write
  procedure run(signal jtag_if : inout t_jtag_if);

end package tb_WardRV_debug_registers_pkg;

package body tb_WardRV_debug_registers_pkg is

  procedure run(signal jtag_if : inout t_jtag_if) is
    variable v_t0 : std_logic_vector(32-1 downto 0);
  begin
    connect(jtag_if);
    halt   (jtag_if);

    log(ID_LOG_HDR, "GPR", C_TB_SCOPE);
    reg_write(jtag_if, A5, x"A5A5A5A5");
    reg_check(jtag_if, A5, x"A5A5A5A5", "a5");
    reg_write(jtag_if, X0, x"0000FFFF");
    reg_check(jtag_if, X0, x"00000000", "x0 stays 0");
    reg_check(jtag_if, 16#100A#, C_COUNTER, "a0 = address of the counter");

    log(ID_LOG_HDR, "CSR", C_TB_SCOPE);
    reg_check(jtag_if, MHARTID , C_HARTID, "mhartid");
    reg_write(jtag_if, MSCRATCH, x"12345678");
    reg_check(jtag_if, MSCRATCH, x"12345678", "mscratch");
    reg_write(jtag_if, S0      , x"55AA55AA");
    reg_check(jtag_if, DCSR    , x"400000C3", "dcsr : debugver 4, cause haltreq, prv 3");
    reg_check(jtag_if, S0      , x"55AA55AA", "s0 kept by the CSR access");
    reg_write(jtag_if, S0      , x"00000000");

    log(ID_LOG_HDR, "dpc and t0 written, then resume", C_TB_SCOPE);
    reg_write(jtag_if, DPC     , C_LOOP);
    reg_check(jtag_if, DPC     , C_LOOP, "dpc");
    reg_write(jtag_if, T0      , x"00010000");
    run_for  (jtag_if, 5 us, "after the register writes");
    reg_read (jtag_if, T0, v_t0);
    check_value(unsigned(v_t0) > 16#10000#, ERROR, "the loop goes on from t0 = 0x10000 : 0x" & to_hstring(v_t0), C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_debug_registers_pkg;
