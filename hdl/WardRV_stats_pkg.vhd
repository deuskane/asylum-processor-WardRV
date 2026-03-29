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
use ieee.numeric_bit.all;
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

  type inst_t is record
    inst        : bit_vector(31 downto 0);
    pc          : bit_vector(31 downto 0);
    npc         : bit_vector(31 downto 0);
    inst_type   : inst_type_t;
    rd          : integer;
    rs1         : integer;
    rs2         : integer;
    imm_u       : bit_vector(31 downto 0);
    imm_j       : bit_vector(31 downto 0);
    imm_i       : bit_vector(31 downto 0);
    imm_b       : bit_vector(31 downto 0);
    imm_s       : bit_vector(31 downto 0);
    op1         : bit_vector(31 downto 0);
    op2         : bit_vector(31 downto 0);
    res         : bit_vector(31 downto 0);
    mem_addr    : bit_vector(31 downto 0);
    mem_rdata   : bit_vector(31 downto 0);
    mem_be      : bit_vector(3 downto 0);
  end record;

  procedure print_inst(r : in inst_t);

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
    
    procedure print_inst(r : in inst_t) is
    begin
       case r.inst_type is
         when I_LUI | I_AUIPC =>
            report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rd) & ", 0x" & to_hstring(r.imm_u) & " = 0x" & to_hstring(r.res);
         when I_JAL =>
            report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rd) & ", 0x" & to_hstring(r.imm_j) & " (Link=0x" & to_hstring(r.res) & ", NPC=0x" & to_hstring(r.npc) & ")";
         when I_JALR =>
            report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rd) & ", R" & integer'image(r.rs1) & ", " & integer'image(to_integer(signed(r.imm_i))) & " (R" & integer'image(r.rs1) & "=0x" & to_hstring(r.op1) & ", Link=0x" & to_hstring(r.res) & ", NPC=0x" & to_hstring(r.npc) & ")";
         when I_BEQ | I_BNE | I_BLT | I_BGE | I_BLTU | I_BGEU =>
            report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rs1) & ", R" & integer'image(r.rs2) & ", 0x" & to_hstring(r.imm_b) & " (0x" & to_hstring(r.op1) & ", 0x" & to_hstring(r.op2) & ") NPC=0x" & to_hstring(r.npc);
         when I_LB | I_LH | I_LW | I_LBU | I_LHU =>
            report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rd) & ", " & integer'image(to_integer(signed(r.imm_i))) & "(R" & integer'image(r.rs1) & ") (Addr=0x" & to_hstring(r.mem_addr) & ", Rdata=0x" & to_hstring(r.mem_rdata) & ")";
         when I_SB | I_SH | I_SW =>
            report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rs2) & ", " & integer'image(to_integer(signed(r.imm_s))) & "(R" & integer'image(r.rs1) & ") (Addr=0x" & to_hstring(r.mem_addr) & ", Wdata=0x" & to_hstring(r.op2) & ", BE=0x" & to_hstring(r.mem_be) & ")";
         when I_ADDI | I_SLLI | I_SLTI | I_SLTIU | I_XORI | I_SRLI | I_SRAI | I_ORI | I_ANDI =>
             report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rd) & ", R" & integer'image(r.rs1) & ", " & integer'image(to_integer(signed(r.imm_i))) & " (0x" & to_hstring(r.op1) & ", 0x" & to_hstring(r.imm_i) & ") = 0x" & to_hstring(r.res);
         when I_ADD | I_SUB | I_SLL | I_SLT | I_SLTU | I_XOR | I_SRL | I_SRA | I_OR | I_AND =>
             report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type) & " R" & integer'image(r.rd) & ", R" & integer'image(r.rs1) & ", R" & integer'image(r.rs2) & " (0x" & to_hstring(r.op1) & ", 0x" & to_hstring(r.op2) & ") = 0x" & to_hstring(r.res);
         when others =>
             report "[ISS] PC=0x" & to_hstring(r.pc) & " NPC=0x" & to_hstring(r.npc) & " : " & INST_NAMES(r.inst_type);
       end case;
    end procedure;

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