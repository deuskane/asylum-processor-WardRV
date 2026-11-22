-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_jtag_dmi_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_jtag_dmi_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario dmi of tb_WardRV_dbg_jtag : DMI writes and reads of the fake Debug Module, one
--              DMI request per access
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_dbg_jtag_pkg.vhd)
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
use     asylum.WardRV_debug_pkg.all;

use     work.WardRV_vips.all;

use     work.tb_WardRV_dbg_jtag_bfm_pkg.all;

package tb_WardRV_dbg_jtag_dmi_pkg is

  -- DMI writes and reads of the fake Debug Module, one
  -- DMI request per access
  procedure run(
    signal   jtag_if    : inout t_jtag_if;
    signal   dm_nb_req  : in  natural
  );

end package tb_WardRV_dbg_jtag_dmi_pkg;

package body tb_WardRV_dbg_jtag_dmi_pkg is

  procedure run(
    signal   jtag_if    : inout t_jtag_if;
    signal   dm_nb_req  : in  natural
  ) is
    variable v_data     : std_logic_vector(32-1 downto 0);
    variable v_addr     : std_logic_vector(DMI_ABITS-1 downto 0);
  begin
    jtag_reset(msg => "Test-Logic-Reset", jtag_if => jtag_if, config => C_JTAG_BFM_CONFIG);
    for i in 0 to 7 loop
      v_addr := std_logic_vector(to_unsigned(4*i+3, DMI_ABITS));
      vip_jtag_dmi_write(jtag_if, v_addr, std_logic_vector(to_unsigned(i, 4)) & x"BADC0D" & std_logic_vector(to_unsigned(i, 4)));
    end loop;
    for i in 7 downto 0 loop
      v_addr := std_logic_vector(to_unsigned(4*i+3, DMI_ABITS));
      vip_jtag_dmi_read(jtag_if, v_addr, v_data);
      check_value(v_data, std_logic_vector(to_unsigned(i, 4)) & x"BADC0D" & std_logic_vector(to_unsigned(i, 4)), ERROR, "DMI read 0x" & to_hstring(v_addr), C_TB_SCOPE);
    end loop;
    check_value(dm_nb_req, 16, ERROR, "One DMI request per access", C_TB_SCOPE);
  end procedure;

end package body tb_WardRV_dbg_jtag_dmi_pkg;
