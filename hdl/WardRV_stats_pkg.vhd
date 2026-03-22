-------------------------------------------------------------------------------
-- Title      : WardRV Statistics Package
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_stats_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Statistics class for WardRV ISS
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

package WardRV_stats_pkg is

  type inst_type_t is (
    I_ADD,
    I_ADDI,
    I_AND,
    I_ANDI,
    I_AUIPC,
    I_BEQ,
    I_BGE,
    I_BGEU,
    I_BLT,
    I_BLTU,
    I_BNE,
    I_JAL,
    I_JALR,
    I_LB,
    I_LBU,
    I_LH,
    I_LHU,
    I_LUI,
    I_LW,
    I_OR,
    I_ORI,
    I_SB,
    I_SH,
    I_SLL,
    I_SLLI,
    I_SLT,
    I_SLTI,
    I_SLTIU,
    I_SLTU,
    I_SRA,
    I_SRAI,
    I_SRL,
    I_SRLI,
    I_SUB,
    I_SW,
    I_XOR,
    I_XORI,
    I_UNKNOWN,
    I_TOTAL
  );

  type inst_names_t is array (inst_type_t) of string(1 to 8);
  constant INST_NAMES : inst_names_t;

  function get_inst_name(inst : in inst_type_t) return string;

  type WardRV_stats is protected
    procedure reset;
    procedure increment(inst : in inst_type_t);
    procedure dump(filename : in string := "");
  end protected;

end package;

package body WardRV_stats_pkg is

  type stats_array_t is array (inst_type_t) of natural;

  constant INST_NAMES : inst_names_t := (
    I_ADD         => "ADD     ",
    I_ADDI        => "ADDI    ",
    I_AND         => "AND     ",
    I_ANDI        => "ANDI    ",
    I_AUIPC       => "AUIPC   ",
    I_BEQ         => "BEQ     ",
    I_BGE         => "BGE     ",
    I_BGEU        => "BGEU    ",
    I_BLT         => "BLT     ",
    I_BLTU        => "BLTU    ",
    I_BNE         => "BNE     ",
    I_JAL         => "JAL     ",
    I_JALR        => "JALR    ",
    I_LB          => "LB      ",
    I_LBU         => "LBU     ",
    I_LH          => "LH      ",
    I_LHU         => "LHU     ",
    I_LUI         => "LUI     ",
    I_LW          => "LW      ",
    I_OR          => "OR      ",
    I_ORI         => "ORI     ",
    I_SB          => "SB      ",
    I_SH          => "SH      ",
    I_SLL         => "SLL     ",
    I_SLLI        => "SLLI    ",
    I_SLT         => "SLT     ",
    I_SLTI        => "SLTI    ",
    I_SLTIU       => "SLTIU   ",
    I_SLTU        => "SLTU    ",
    I_SRA         => "SRA     ",
    I_SRAI        => "SRAI    ",
    I_SRL         => "SRL     ",
    I_SRLI        => "SRLI    ",
    I_SUB         => "SUB     ",
    I_SW          => "SW      ",
    I_XOR         => "XOR     ",
    I_XORI        => "XORI    ",
    I_UNKNOWN     => "UNKNOWN ",
    I_TOTAL       => "Total   "
  );


    function get_inst_name(inst : in inst_type_t) return string is
    begin
      return INST_NAMES(inst);
    end function;
    
    type WardRV_stats is protected body
    variable stats_v : stats_array_t;

    procedure reset is
    begin
      stats_v := (others => 0);
    end procedure;

    procedure increment(inst : in inst_type_t) is
    begin
      stats_v(inst)    := stats_v(inst) + 1;
      stats_v(I_TOTAL) := stats_v(I_TOTAL) + 1;
    end procedure;


    procedure dump(filename : in string := "") is
      variable v_ratio : real;
      file f_out       : text;
      variable l       : line;
      variable v_open  : boolean := false;
    begin
      if filename /= "" then
        file_open(f_out, filename, write_mode);
        v_open := true;
      end if;

      report "--- WardRV ISS Statistics ---";
      for i in inst_type_t loop
        if stats_v(I_TOTAL) > 0 then
          v_ratio := (real(stats_v(i)) * 100.0) / real(stats_v(I_TOTAL));
        else
          v_ratio := 0.0;
        end if;
        report INST_NAMES(i) & " : " & integer'image(stats_v(i)) & " (" & to_string(v_ratio, 2) & " %)";
        if v_open then
          write(l, INST_NAMES(i) & " : " & integer'image(stats_v(i)) & " (" & to_string(v_ratio, 2) & " %)");
          writeline(f_out, l);
        end if;
      end loop;
      report "-----------------------------";

      if v_open then
        file_close(f_out);
      end if;
    end procedure;
  end protected body;

end package body;