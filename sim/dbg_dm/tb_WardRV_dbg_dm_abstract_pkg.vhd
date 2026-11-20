-------------------------------------------------------------------------------
-- Title      : tb_WardRV_dbg_dm_abstract_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_WardRV_dbg_dm_abstract_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario abstract of tb_WardRV_dbg_dm : programs of GPR / CSR reads and writes, postexec
--              and the program buffer with its implicit ebreak, data registers
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

package tb_WardRV_dbg_dm_abstract_pkg is

  -- programs of GPR / CSR reads and writes, postexec
  -- and the program buffer with its implicit ebreak, data registers
  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t);

end package tb_WardRV_dbg_dm_abstract_pkg;

package body tb_WardRV_dbg_dm_abstract_pkg is

  procedure run(signal clk : in std_logic; signal ini : out dm_ini_t; signal tgt : in dm_tgt_t) is
  begin
    activate_and_halt(clk, ini, tgt);

    log(ID_LOG_HDR, "GPR read : x5", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_READ or x"00001005");
    dmi_check    (clk, ini, tgt, DM_ABSTRACTCS, x"02001002", "abstractcs.busy");
    check_program(clk, ini, tgt, (SW_X5_DATA0, NOP, NOP, NOP, EBREAK), "read x5");
    hart_write   (clk, ini, tgt, DBG_DATA, x"55555555");
    end_command  (clk, ini, tgt, CMDERR_NONE, "read x5");
    dmi_check    (clk, ini, tgt, DM_DATA0, x"55555555", "data0 written by the hart");

    log(ID_LOG_HDR, "GPR write : x31", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_DATA0  , x"12345678");
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"0000101F");
    check_program(clk, ini, tgt, (LW_X31_DATA0, NOP, NOP, NOP, EBREAK), "write x31");
    hart_check   (clk, ini, tgt, DBG_DATA, x"12345678", "data0 read by the hart");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write x31");

    log(ID_LOG_HDR, "CSR read : mhartid, CSR write : mscratch", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_READ  or x"00000F14");
    check_program(clk, ini, tgt, (CSRW_DSCRATCH0, CSRR_MHARTID, SW_S0_DATA0, CSRR_DSCRATCH0, EBREAK), "read mhartid");
    end_command  (clk, ini, tgt, CMDERR_NONE, "read mhartid");
    dmi_write    (clk, ini, tgt, DM_COMMAND, CMD_WRITE or x"00000340");
    check_program(clk, ini, tgt, (CSRW_DSCRATCH0, LW_S0_DATA0, CSRW_MSCRATCH, CSRR_DSCRATCH0, EBREAK), "write mscratch");
    end_command  (clk, ini, tgt, CMDERR_NONE, "write mscratch");

    log(ID_LOG_HDR, "postexec : program buffer and implicit ebreak, data1", C_TB_SCOPE);
    dmi_write    (clk, ini, tgt, DM_PROGBUF0, LW_S1_S0);
    dmi_write    (clk, ini, tgt, DM_PROGBUF1, SW_S1_S0);
    dmi_check    (clk, ini, tgt, DM_PROGBUF1, SW_S1_S0, "progbuf1");
    dmi_write    (clk, ini, tgt, DM_COMMAND , CMD_POSTEXEC);
    check_program(clk, ini, tgt, (NOP, NOP, NOP, NOP, NOP), "postexec");
    hart_check   (clk, ini, tgt, DBG_PROGBUF    , LW_S1_S0, "progbuf0 after the program");
    hart_check   (clk, ini, tgt, DBG_PROGBUF + 4, SW_S1_S0, "progbuf1");
    hart_check   (clk, ini, tgt, DBG_PROGBUF + 8, EBREAK  , "implicit ebreak");
    hart_write   (clk, ini, tgt, DBG_DATA + 4, x"0000DA7A");
    end_command  (clk, ini, tgt, CMDERR_NONE, "postexec");
    dmi_check    (clk, ini, tgt, DM_DATA1, x"0000DA7A", "data1 written by the hart");
  end procedure;

end package body tb_WardRV_dbg_dm_abstract_pkg;
