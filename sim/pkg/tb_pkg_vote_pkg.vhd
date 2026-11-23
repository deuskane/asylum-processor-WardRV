-------------------------------------------------------------------------------
-- Title      : tb_pkg_vote_pkg
-- Project    : WardRV
-------------------------------------------------------------------------------
-- File       : tb_pkg_vote_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Scenario vote of tb_pkg : vote of imem_ini_t, imem_tgt_t,
--              dmem_ini_t and dmem_tgt_t (WardRV_pkg). Two values with every
--              bit of every field different : the three copies equal, then
--              each copy in turn different from the two others in every
--              field, for both values : the 8 cases of a majority of three
--              bits, on every bit of every field
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

use     work.tb_pkg_bfm_pkg.all;

package tb_pkg_vote_pkg is

  -- vote of the four bus records
  procedure run;

end package tb_pkg_vote_pkg;

package body tb_pkg_vote_pkg is

  -- Two values per record, every bit of every field different (the
  -- vectors are complements, the bits of a record not all equal)
  constant C_IMEM_INI : imem_inis_t(0 to 1) := ((valid => '1', addr => x"12345678"),
                                                (valid => '0', addr => x"EDCBA987"));
  constant C_IMEM_TGT : imem_tgts_t(0 to 1) := ((ready => '1', inst => x"0F1E2D3C"),
                                                (ready => '0', inst => x"F0E1D2C3"));
  constant C_DMEM_INI : dmem_inis_t(0 to 1) := ((valid => '1', addr => x"12345678", wdata => x"9ABCDEF0", we => '0', be => "1010"),
                                                (valid => '0', addr => x"EDCBA987", wdata => x"6543210F", we => '1', be => "0101"));
  constant C_DMEM_TGT : dmem_tgts_t(0 to 1) := ((ready => '1', rdata => x"0F1E2D3C", err => '0'),
                                                (ready => '0', rdata => x"F0E1D2C3", err => '1'));

  -- Name of a case : majority value p, copy k different (3 : none)
  function case_name(p : natural; k : natural) return string is
  begin
    if k = 3
    then
      return "value " & integer'image(p) & ", three copies equal";
    else
      return "value " & integer'image(p) & ", i" & integer'image(k) & " different";
    end if;
  end function;

  procedure run is
    variable v_imem_ini : imem_inis_t(0 to 2);
    variable v_imem_tgt : imem_tgts_t(0 to 2);
    variable v_dmem_ini : dmem_inis_t(0 to 2);
    variable v_dmem_tgt : dmem_tgts_t(0 to 2);
  begin
    -- p : value of the majority (index in the constants), the different copy
    -- takes the other value
    for p in 0 to 1 loop
      for k in 0 to 3 loop
        log(ID_SEQUENCER, "vote : " & case_name(p, k), C_TB_SCOPE);

        v_imem_ini := (others => C_IMEM_INI(p));
        v_imem_tgt := (others => C_IMEM_TGT(p));
        v_dmem_ini := (others => C_DMEM_INI(p));
        v_dmem_tgt := (others => C_DMEM_TGT(p));

        if k < 3
        then
          v_imem_ini(k) := C_IMEM_INI(1-p);
          v_imem_tgt(k) := C_IMEM_TGT(1-p);
          v_dmem_ini(k) := C_DMEM_INI(1-p);
          v_dmem_tgt(k) := C_DMEM_TGT(1-p);
        end if;

        check_fields(vote(v_imem_ini(0), v_imem_ini(1), v_imem_ini(2)), C_IMEM_INI(p), "vote imem_ini_t, " & case_name(p, k));
        check_fields(vote(v_imem_tgt(0), v_imem_tgt(1), v_imem_tgt(2)), C_IMEM_TGT(p), "vote imem_tgt_t, " & case_name(p, k));
        check_fields(vote(v_dmem_ini(0), v_dmem_ini(1), v_dmem_ini(2)), C_DMEM_INI(p), "vote dmem_ini_t, " & case_name(p, k));
        check_fields(vote(v_dmem_tgt(0), v_dmem_tgt(1), v_dmem_tgt(2)), C_DMEM_TGT(p), "vote dmem_tgt_t, " & case_name(p, k));
      end loop;
    end loop;
  end procedure;

end package body tb_pkg_vote_pkg;
