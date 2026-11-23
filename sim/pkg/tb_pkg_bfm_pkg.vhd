-------------------------------------------------------------------------------
-- Title      : tb_pkg_bfm_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_pkg_bfm_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Helpers of the tb_pkg scenarios : scope, arrays of three
--              copies of a bus, check of every field of a bus record
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-23  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

library asylum;
use     asylum.WardRV_pkg.all;

package tb_pkg_bfm_pkg is

  constant C_TB_SCOPE : string := "tb_pkg";

  -- Copies of a bus
  type imem_inis_t is array (natural range <>) of imem_ini_t;
  type imem_tgts_t is array (natural range <>) of imem_tgt_t;
  type dmem_inis_t is array (natural range <>) of dmem_ini_t;
  type dmem_tgts_t is array (natural range <>) of dmem_tgt_t;

  -- Check every field of value against exp
  procedure check_fields(value : imem_ini_t; exp : imem_ini_t; msg : string);
  procedure check_fields(value : imem_tgt_t; exp : imem_tgt_t; msg : string);
  procedure check_fields(value : dmem_ini_t; exp : dmem_ini_t; msg : string);
  procedure check_fields(value : dmem_tgt_t; exp : dmem_tgt_t; msg : string);

end package tb_pkg_bfm_pkg;

package body tb_pkg_bfm_pkg is

  procedure check_fields(value : imem_ini_t; exp : imem_ini_t; msg : string) is
  begin
    check_value(value.valid, exp.valid, ERROR, msg & " : valid", C_TB_SCOPE);
    check_value(value.addr , exp.addr , ERROR, msg & " : addr" , C_TB_SCOPE);
  end procedure;

  procedure check_fields(value : imem_tgt_t; exp : imem_tgt_t; msg : string) is
  begin
    check_value(value.ready, exp.ready, ERROR, msg & " : ready", C_TB_SCOPE);
    check_value(value.inst , exp.inst , ERROR, msg & " : inst" , C_TB_SCOPE);
  end procedure;

  procedure check_fields(value : dmem_ini_t; exp : dmem_ini_t; msg : string) is
  begin
    check_value(value.valid, exp.valid, ERROR, msg & " : valid", C_TB_SCOPE);
    check_value(value.addr , exp.addr , ERROR, msg & " : addr" , C_TB_SCOPE);
    check_value(value.wdata, exp.wdata, ERROR, msg & " : wdata", C_TB_SCOPE);
    check_value(value.we   , exp.we   , ERROR, msg & " : we"   , C_TB_SCOPE);
    check_value(value.be   , exp.be   , ERROR, msg & " : be"   , C_TB_SCOPE);
  end procedure;

  procedure check_fields(value : dmem_tgt_t; exp : dmem_tgt_t; msg : string) is
  begin
    check_value(value.ready, exp.ready, ERROR, msg & " : ready", C_TB_SCOPE);
    check_value(value.rdata, exp.rdata, ERROR, msg & " : rdata", C_TB_SCOPE);
    check_value(value.err  , exp.err  , ERROR, msg & " : err"  , C_TB_SCOPE);
  end procedure;

end package body tb_pkg_bfm_pkg;
