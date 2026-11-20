-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_autoexec_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_autoexec_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario autoexec of tb_WardRV_dbg_dm : abstractauto on data and program buffer accesses
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

package tb_WardRV_dbg_dm_autoexec_pkg is

  -- abstractauto on data and program buffer accesses
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_autoexec_pkg;

package body tb_WardRV_dbg_dm_autoexec_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
    variable v_data : std_logic_vector(32-1 downto 0);
  begin
    activate_and_halt(clk, ini, tgt);
    dmi_write  (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001005");
    hart_write (clk, ini, tgt, DBG_DATA, x"00000001");
    end_command(clk, ini, tgt, CMDERR_NONE, "read x5");

    dmi_write(clk, ini, tgt, DM_ABSTRACTAUTO, x"FFFFFFFF");
    dmi_check(clk, ini, tgt, DM_ABSTRACTAUTO, x"00030003", "abstractauto : 2 progbuf, 2 data");
    dmi_write(clk, ini, tgt, DM_ABSTRACTAUTO, x"00010001");

    for i in 0 to 2 loop
      log(ID_LOG_HDR, "autoexec " & integer'image(i), C_TB_SCOPE);
      case i is
        when 0      => dmi_access(clk, ini, tgt, '0', DM_DATA0   , x"00000000", v_data);
                       check_value(v_data, x"00000001", ERROR, "data0 read before the command", C_TB_SCOPE);
        when 1      => dmi_write (clk, ini, tgt, DM_DATA0   , x"00000002");
        when others => dmi_write (clk, ini, tgt, DM_PROGBUF0, NOP);
      end case;
      check_program(clk, ini, tgt, (SW_X5_DATA0, NOP, NOP, NOP, EBREAK), "autoexec read x5");
      hart_write   (clk, ini, tgt, DBG_DATA, std_logic_vector(to_unsigned(16+i, 32)));
      end_command  (clk, ini, tgt, CMDERR_NONE, "autoexec");
    end loop;

    log(ID_LOG_HDR, "No autoexec", C_TB_SCOPE);
    dmi_write (clk, ini, tgt, DM_DATA1       , x"00000003");
    dmi_write (clk, ini, tgt, DM_PROGBUF1    , NOP);
    hart_check(clk, ini, tgt, DBG_GO, x"00000000", "no command started by data1 / progbuf1");
    dmi_write (clk, ini, tgt, DM_ABSTRACTAUTO, x"00000000");
    dmi_check (clk, ini, tgt, DM_DATA0       , x"00000012", "data0 of the last command");
    dmi_write (clk, ini, tgt, DM_DATA0       , x"00000004");
    hart_check(clk, ini, tgt, DBG_GO, x"00000000", "no command started without abstractauto");
  end procedure;

end package body tb_WardRV_dbg_dm_autoexec_pkg;
