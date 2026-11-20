-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_sba_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_sba_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario sba of tb_WardRV_dbg_dm : System Bus Access 8 / 16 / 32 bits, autoincrement,
--              readonaddr, readondata, sberror, sbbusyerror
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-20  1.0      mrosiere Created (split of tb_WardRV_dbg_dm_pkg.vhd)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

use     work.tb_WardRV_dbg_dm_bfm_pkg.all;

package tb_WardRV_dbg_dm_sba_pkg is

  -- System Bus Access 8 / 16 / 32 bits, autoincrement,
  -- readonaddr, readondata, sberror, sbbusyerror
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_sba_pkg;

package body tb_WardRV_dbg_dm_sba_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is

    procedure sba_wait is
      variable v_sbcs : std_logic_vector(32-1 downto 0);
    begin
      for i in 1 to 20 loop
        dmi_access(clk, ini, tgt, '0', DM_SBCS, x"00000000", v_sbcs);
        exit when v_sbcs(21) = '0';
      end loop;
      check_value(v_sbcs(21), '0', ERROR, "sbcs.sbbusy", C_TB_SCOPE);
    end procedure;

  begin
    dmi_write(clk, ini, tgt, DM_DMCONTROL, x"00000001");

    log(ID_LOG_HDR, "32-bit write, read on address", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");
    dmi_write(clk, ini, tgt, DM_SBDATA0   , x"A1B2C3D4");
    sba_wait;
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00140000");          -- sbreadonaddr, 32 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBDATA0   , x"A1B2C3D4", "32-bit read");

    log(ID_LOG_HDR, "8 and 16-bit accesses", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00000000");          -- 8 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000011");
    dmi_write(clk, ini, tgt, DM_SBDATA0   , x"0000005A");
    sba_wait;
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00120000");          -- sbreadonaddr, 16 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000012");
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBDATA0   , x"0000A1B2", "16-bit read");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00140000");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBDATA0   , x"A1B25AD4", "8-bit write");

    log(ID_LOG_HDR, "Autoincrement, read on data", C_TB_SCOPE);
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00050000");          -- autoincrement, 32 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000020");
    for i in 0 to 3 loop
      dmi_write(clk, ini, tgt, DM_SBDATA0 , std_logic_vector(to_unsigned(16#100# + i, 32)));
      sba_wait;
    end loop;
    dmi_check(clk, ini, tgt, DM_SBADDRESS0, x"00000030", "sbaddress0 incremented");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00158000");          -- readonaddr, autoincrement, readondata
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000020");
    for i in 0 to 3 loop
      sba_wait;
      dmi_check(clk, ini, tgt, DM_SBDATA0 , std_logic_vector(to_unsigned(16#100# + i, 32)), "read on data " & integer'image(i));
    end loop;

    log(ID_LOG_HDR, "sberror, sbbusyerror", C_TB_SCOPE);
    sba_wait;
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00140000");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000012");          -- Misaligned
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20143407", "sberror 3");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000010");          -- No access while sberror
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20143407", "no access while sberror is set");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00067000");          -- W1C, sbaccess 64
    dmi_write(clk, ini, tgt, DM_SBDATA0   , x"00000000");
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20064407", "sberror 4");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00147000");          -- W1C, readonaddr, 32 bits
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000020");
    dmi_write(clk, ini, tgt, DM_SBADDRESS0, x"00000024");          -- During the access
    sba_wait;
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20540407", "sbbusyerror");
    dmi_check(clk, ini, tgt, DM_SBADDRESS0, x"00000020", "sbaddress0 not written during the access");
    dmi_write(clk, ini, tgt, DM_SBCS      , x"00440000");          -- W1C, 32 bits
    dmi_check(clk, ini, tgt, DM_SBCS      , x"20040407", "sbbusyerror cleared");
  end procedure;

end package body tb_WardRV_dbg_dm_sba_pkg;
