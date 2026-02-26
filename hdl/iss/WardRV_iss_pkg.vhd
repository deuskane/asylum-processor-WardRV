-------------------------------------------------------------------------------
-- Title      : WardRV ISS Package
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_iss_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Instruction Set Simulator with full RV32I support
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;
use std.textio.all;

library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;

package WardRV_iss_pkg is

  -- Generic parameters for memory configuration
  constant IMEM_ADDR_WIDTH : positive := 32;
  constant DMEM_ADDR_WIDTH : positive := 32;
  constant DMEM_DATA_WIDTH : positive := 32;

  type iss_t is protected
    
    -- Initialization
    procedure reset(
      start_pc : in std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0)
    );

    -- Accessors
    impure function get_pc return std_logic_vector;
    impure function get_reg(r : integer) return std_logic_vector;
    
    -- Verbose control
    procedure set_verbose(v : in boolean);

    -- Execution Step
    -- Returns info about memory request if any
    procedure execute_instruction(
      inst          : in  std_logic_vector(31 downto 0);
      -- Memory Request Info
      mem_req       : out boolean;
      mem_we        : out std_logic;
      mem_addr      : out std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      mem_wdata     : out std_logic_vector(DMEM_DATA_WIDTH-1 downto 0);
      mem_be        : out std_logic_vector((DMEM_DATA_WIDTH/8)-1 downto 0)
    );

    -- Complete the instruction execution (Update PC and Registers)
    procedure complete;

    -- Complete a Load operation
    procedure complete_load(
      mem_rdata     : in std_logic_vector(DMEM_DATA_WIDTH-1 downto 0)
    );

    -- Statistics
    procedure stats(filename : in string := "");

  end protected;

end package;

package body WardRV_iss_pkg is

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
    I_TOTAL
  );

  type stats_array_t is array (inst_type_t) of natural;

  type inst_names_t is array (inst_type_t) of string(1 to 18);

  constant INST_NAMES : inst_names_t := (
    I_ADD   => "ADD               ",
    I_ADDI  => "ADDI              ",
    I_AND   => "AND               ",
    I_ANDI  => "ANDI              ",
    I_AUIPC => "AUIPC             ",
    I_BEQ   => "BEQ               ",
    I_BGE   => "BGE               ",
    I_BGEU  => "BGEU              ",
    I_BLT   => "BLT               ",
    I_BLTU  => "BLTU              ",
    I_BNE   => "BNE               ",
    I_JAL   => "JAL               ",
    I_JALR  => "JALR              ",
    I_LB    => "LB                ",
    I_LBU   => "LBU               ",
    I_LH    => "LH                ",
    I_LHU   => "LHU               ",
    I_LUI   => "LUI               ",
    I_LW    => "LW                ",
    I_OR    => "OR                ",
    I_ORI   => "ORI               ",
    I_SB    => "SB                ",
    I_SH    => "SH                ",
    I_SLL   => "SLL               ",
    I_SLLI  => "SLLI              ",
    I_SLT   => "SLT               ",
    I_SLTI  => "SLTI              ",
    I_SLTIU => "SLTIU             ",
    I_SLTU  => "SLTU              ",
    I_SRA   => "SRA               ",
    I_SRAI  => "SRAI              ",
    I_SRL   => "SRL               ",
    I_SRLI  => "SRLI              ",
    I_SUB   => "SUB               ",
    I_SW    => "SW                ",
    I_XOR   => "XOR               ",
    I_XORI  => "XORI              ",
    I_TOTAL => "Total Instructions"
  );

  type regfile_t is array (0 to 31) of bit_vector(31 downto 0);

  type iss_t is protected body
    
    variable pc_r                  : bit_vector(IMEM_ADDR_WIDTH-1 downto 0);
    variable regs_r                : regfile_t;
    variable verbose_r             : boolean := false;

    -- Intermediate results for completion
    variable pending_npc           : bit_vector(IMEM_ADDR_WIDTH-1 downto 0);
    variable pending_rd            : bit_vector(4 downto 0);
    variable pending_res           : bit_vector(31 downto 0);
    
    -- State for pending load
    variable pending_load_funct3   : bit_vector(2 downto 0);
    variable pending_load_byte_off : integer;

    -- Statistics counters
    variable stats_v : stats_array_t;

    procedure reset(
      start_pc : in std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0)
    ) is
    begin
      pc_r   := to_bitvector(start_pc);
      regs_r := (others => (others => '0'));
      stats_v := (others => 0);
      pending_npc     := (others => '0');
      pending_rd      := (others => '0');
      pending_res     := (others => '0');
    end procedure;

    impure function get_pc return std_logic_vector is
    begin
      return to_stdlogicvector(pc_r);
    end function;

    impure function get_reg(r : integer) return std_logic_vector is
    begin
      if r = 0 then return x"00000000"; end if;
      return to_stdlogicvector(regs_r(r));
    end function;

    procedure set_verbose(v : in boolean) is
    begin
      verbose_r := v;
    end procedure;

    procedure execute_instruction(
      inst          : in  std_logic_vector(31 downto 0);
      mem_req       : out boolean;
      mem_we        : out std_logic;
      mem_addr      : out std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      mem_wdata     : out std_logic_vector(DMEM_DATA_WIDTH-1 downto 0);
      mem_be        : out std_logic_vector((DMEM_DATA_WIDTH/8)-1 downto 0)
    ) is
      variable v_inst         : bit_vector(31 downto 0) := to_bitvector(inst);
      variable opcode         : bit_vector(6 downto 0)  := v_inst( 6 downto  0);
      variable rd             : bit_vector(4 downto 0)  := v_inst(11 downto  7);
      variable funct3         : bit_vector(2 downto 0)  := v_inst(14 downto 12);
      variable rs1            : bit_vector(4 downto 0)  := v_inst(19 downto 15);
      variable rs2            : bit_vector(4 downto 0)  := v_inst(24 downto 20);
      variable funct7         : bit_vector(6 downto 0)  := v_inst(31 downto 25);
      variable funct12        : bit_vector(11 downto 0) := v_inst(31 downto 20);      
      
      variable v_imm_sign     : bit_vector(31 downto 0);
      variable v_imm_i        : bit_vector(31 downto 0);
      variable v_imm_s        : bit_vector(31 downto 0);
      variable v_imm_b        : bit_vector(31 downto 0);
      variable v_imm_u        : bit_vector(31 downto 0);
      variable v_imm_j        : bit_vector(31 downto 0);
      variable v_op1          : signed(31 downto 0);
      variable v_op2          : signed(31 downto 0);
      variable v_res          : bit_vector(31 downto 0);
      variable v_pc           : bit_vector(IMEM_ADDR_WIDTH-1 downto 0);
      variable v_npc          : bit_vector(IMEM_ADDR_WIDTH-1 downto 0);
      variable v_addr         : bit_vector(DMEM_ADDR_WIDTH-1 downto 0);
      variable v_shamt        : integer;
      variable v_mnemonic_str : string(1 to 10);
    begin
      -- Initialize outputs
      mem_req    := false;
      mem_we     := '0';
      mem_addr   := (others => '0');
      mem_wdata  := (others => '0');
      mem_be     := (others => '0');

      -- Reset pending write state
      pending_rd  := (others => '0');
      pending_res := (others => '0');

      -- Immediates
      v_imm_sign := (others => v_inst(31));
      v_imm_i    := v_imm_sign(31 downto 12) & v_inst(31 downto 20);
      v_imm_s    := v_imm_sign(31 downto 12) & v_inst(31 downto 25) & v_inst(11 downto 7);
      v_imm_b    := v_imm_sign(31 downto 13) & v_inst(31) & v_inst(7) & v_inst(30 downto 25) & v_inst(11 downto 8) & '0';
      v_imm_u    := v_inst(31 downto 12) & x"000";
      v_imm_j    := v_imm_sign(31 downto 21) & v_inst(31) & v_inst(19 downto 12) & v_inst(20) & v_inst(30 downto 21) & '0';

      -- Operands
      if unsigned(rs1) = 0 then v_op1 := (others => '0'); else v_op1 := signed(regs_r(to_integer(unsigned(rs1)))); end if;
      if unsigned(rs2) = 0 then v_op2 := (others => '0'); else v_op2 := signed(regs_r(to_integer(unsigned(rs2)))); end if;

      -- Default Next PC
      v_pc  := pc_r;
      v_npc := bit_vector(unsigned(pc_r) + 4);
      v_res := (others => '0');

      stats_v(I_TOTAL) := stats_v(I_TOTAL) + 1;

      case (opcode) is
        -- =====================================================================
        -- U-Type Instructions
        -- =====================================================================
        when OPC_LUI =>
          -- Load Upper Immediate
          v_res := v_imm_u;
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   "LUI R" & integer'image(to_integer(unsigned(rd))) & ", 0x" & to_hstring(v_imm_u) & " = 0x" & to_hstring(v_imm_u);
          end if;
          -- synthesis translate_on
          pending_rd  := rd;
          pending_res := v_res;
          stats_v(I_LUI) := stats_v(I_LUI) + 1;

        when OPC_AUIPC =>
          -- Add Upper Immediate to PC
          v_res := bit_vector(unsigned(pc_r) + unsigned(v_imm_u));
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   "AUIPC R" & integer'image(to_integer(unsigned(rd))) & ", 0x" & to_hstring(v_imm_u) & " (PC=0x" & to_hstring(pc_r) & ") = 0x" & to_hstring(v_res);
          end if;
          -- synthesis translate_on
          pending_rd  := rd;
          pending_res := v_res;
          stats_v(I_AUIPC) := stats_v(I_AUIPC) + 1;

        -- =====================================================================
        -- J-Type Instructions
        -- =====================================================================
        when OPC_JAL =>
          -- Jump and Link
          v_res := bit_vector(unsigned(pc_r) + 4);
          v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_j));
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   "JAL R" & integer'image(to_integer(unsigned(rd))) & ", 0x" & to_hstring(v_imm_j) & " (Link=0x" & to_hstring(v_res) & ", NPC=0x" & to_hstring(v_npc) & ")";
          end if;
          -- synthesis translate_on
          pending_rd  := rd;
          pending_res := v_res;
          stats_v(I_JAL) := stats_v(I_JAL) + 1;

        -- =====================================================================
        -- I-Type Instructions (JALR, Loads, OP_IMM)
        -- =====================================================================
        when OPC_JALR =>
          -- Jump and Link Register
          v_res := bit_vector(unsigned(pc_r) + 4);
          v_npc := bit_vector(unsigned(unsigned(v_op1) + unsigned(v_imm_i)) and x"FFFFFFFE");
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   "JALR R" & integer'image(to_integer(unsigned(rd))) & ", R" & integer'image(to_integer(unsigned(rs1))) & ", " & integer'image(to_integer(signed(v_imm_i))) & " (R" & integer'image(to_integer(unsigned(rs1))) & "=0x" & to_hstring(bit_vector(v_op1)) & ", Link=0x" & to_hstring(v_res) & ", NPC=0x" & to_hstring(v_npc) & ")";
          end if;
          -- synthesis translate_on
          pending_rd  := rd;
          pending_res := v_res;
          stats_v(I_JALR) := stats_v(I_JALR) + 1;

        -- =====================================================================
        -- B-Type Instructions (Branch)
        -- =====================================================================
        when OPC_BRANCH =>
          -- Conditional Branch Instructions
          case funct3 is
            when F3_BEQ  => v_mnemonic_str := "BEQ       ";  if v_op1 = v_op2 then v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats_v(I_BEQ) := stats_v(I_BEQ) + 1;
            when F3_BNE  => v_mnemonic_str := "BNE       ";  if v_op1 /= v_op2 then v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats_v(I_BNE) := stats_v(I_BNE) + 1;
            when F3_BLT  => v_mnemonic_str := "BLT       ";  if v_op1 < v_op2 then v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats_v(I_BLT) := stats_v(I_BLT) + 1;
            when F3_BGE  => v_mnemonic_str := "BGE       ";  if v_op1 >= v_op2 then v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats_v(I_BGE) := stats_v(I_BGE) + 1;
            when F3_BLTU => v_mnemonic_str := "BLTU      "; if unsigned(v_op1) < unsigned(v_op2) then v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats_v(I_BLTU) := stats_v(I_BLTU) + 1;
            when F3_BGEU => v_mnemonic_str := "BGEU      "; if unsigned(v_op1) >= unsigned(v_op2) then v_npc := bit_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats_v(I_BGEU) := stats_v(I_BGEU) + 1;
            when others => v_mnemonic_str := "BRANCH_UNK";
          end case;
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   v_mnemonic_str & " R" & integer'image(to_integer(unsigned(rs1))) & ", R" & integer'image(to_integer(unsigned(rs2))) & ", 0x" & to_hstring(v_imm_b) & " (0x" & to_hstring(bit_vector(v_op1)) & ", 0x" & to_hstring(bit_vector(v_op2)) & ") NPC=0x" & to_hstring(v_npc);
          end if;
          -- synthesis translate_on

        -- =====================================================================
        -- S-Type Instructions (Load)
        -- =====================================================================
        when OPC_LOAD =>
          -- Load Instructions: LB, LH, LW, LBU, LHU
          case funct3 is
            when F3_LB  => v_mnemonic_str := "LB        ";  stats_v(I_LB) := stats_v(I_LB) + 1;
            when F3_LH  => v_mnemonic_str := "LH        ";  stats_v(I_LH) := stats_v(I_LH) + 1;
            when F3_LW  => v_mnemonic_str := "LW        ";  stats_v(I_LW) := stats_v(I_LW) + 1;
            when F3_LBU => v_mnemonic_str := "LBU       "; stats_v(I_LBU) := stats_v(I_LBU) + 1;
            when F3_LHU => v_mnemonic_str := "LHU       "; stats_v(I_LHU) := stats_v(I_LHU) + 1;
            when others => v_mnemonic_str := "LOAD_UNK  ";
          end case;
          v_addr := bit_vector(resize(unsigned(v_op1) + unsigned(v_imm_i), DMEM_ADDR_WIDTH));
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   v_mnemonic_str & " R" & integer'image(to_integer(unsigned(rd))) & ", " & integer'image(to_integer(signed(v_imm_i))) & "(R" & integer'image(to_integer(unsigned(rs1))) & ") (Addr=0x" & to_hstring(v_addr) & ")";
          end if;
          -- synthesis translate_on
          mem_req  := true;
          mem_we   := '0';
          mem_addr := to_stdlogicvector(v_addr);
          
          -- Save state for completion
          pending_rd            := rd;
          pending_load_funct3   := funct3;
          pending_load_byte_off := to_integer(unsigned(v_addr(1 downto 0)));
          --pending_load_byte_off := 0;

        -- =====================================================================
        -- S-Type Instructions (Store)
        -- =====================================================================
        when OPC_STORE =>
          -- Store Instructions: SB, SH, SW
          case funct3 is
            when F3_SB  => v_mnemonic_str := "SB        "; stats_v(I_SB) := stats_v(I_SB) + 1;
            when F3_SH  => v_mnemonic_str := "SH        "; stats_v(I_SH) := stats_v(I_SH) + 1;
            when F3_SW  => v_mnemonic_str := "SW        "; stats_v(I_SW) := stats_v(I_SW) + 1;
            when others => v_mnemonic_str := "STORE_UNK ";
          end case;
          v_addr := bit_vector(resize(unsigned(v_op1) + unsigned(v_imm_s), DMEM_ADDR_WIDTH));
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   v_mnemonic_str & " R" & integer'image(to_integer(unsigned(rs2))) & ", " & integer'image(to_integer(signed(v_imm_s))) & "(R" & integer'image(to_integer(unsigned(rs1))) & ") (Addr=0x" & to_hstring(v_addr) & ", Data=0x" & to_hstring(bit_vector(v_op2)) & ")";
          end if;
          -- synthesis translate_on
          mem_req   := true;
          mem_we    := '1';
          mem_addr  := to_stdlogicvector(v_addr);
          v_shamt   := to_integer(unsigned(v_addr(1 downto 0))) * 8;
          mem_wdata := to_stdlogicvector(bit_vector(shift_left(unsigned(bit_vector(v_op2)), v_shamt)));
          
          case funct3 is
            when F3_SB  => mem_be := to_stdlogicvector(bit_vector(shift_left(unsigned'("0001"), to_integer(unsigned(v_addr(1 downto 0))))));
            when F3_SH  => mem_be := to_stdlogicvector(bit_vector(shift_left(unsigned'("0011"), to_integer(unsigned(v_addr(1 downto 0))))));
            when F3_SW  => mem_be := (others => '1');
            when others => mem_be := (others => '0');
          end case;

        -- =====================================================================
        -- I-Type Instructions (Arithmetic & Logical with Immediate)
        -- =====================================================================
        when OPC_OP_IMM =>
          -- Arithmetic & Logical Operations with Immediate
          case funct3 is
            when F3_ADD_SUB =>
              -- ADDI
              v_mnemonic_str := "ADDI      ";
              v_res := bit_vector(v_op1 + signed(v_imm_i));
              stats_v(I_ADDI) := stats_v(I_ADDI) + 1;
            when F3_SLL =>
              -- SLLI
              v_mnemonic_str := "SLLI      ";
              v_shamt := to_integer(unsigned(v_imm_i(4 downto 0)));
              v_res := bit_vector(shift_left(unsigned(v_op1), v_shamt));
              stats_v(I_SLLI) := stats_v(I_SLLI) + 1;
            when F3_SLT =>
              -- SLTI
              v_mnemonic_str := "SLTI      ";
              if v_op1 < signed(v_imm_i) then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats_v(I_SLTI) := stats_v(I_SLTI) + 1;
            when F3_SLTU =>
              -- SLTIU
              v_mnemonic_str := "SLTIU     ";
              if unsigned(v_op1) < unsigned(v_imm_i) then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats_v(I_SLTIU) := stats_v(I_SLTIU) + 1;
            when F3_XOR =>
              -- XORI
              v_mnemonic_str := "XORI      ";
              v_res := bit_vector(v_op1 xor signed(v_imm_i));
              stats_v(I_XORI) := stats_v(I_XORI) + 1;
            when F3_SRL_SRA =>
              -- SRLI / SRAI
              v_shamt := to_integer(unsigned(v_imm_i(4 downto 0)));
              if funct7(5) = '1' then
                -- SRAI (Arithmetic Right Shift)
                v_mnemonic_str := "SRAI      ";
                v_res := bit_vector(shift_right(v_op1, v_shamt));
                stats_v(I_SRAI) := stats_v(I_SRAI) + 1;
              else
                -- SRLI (Logical Right Shift)
                v_mnemonic_str := "SRLI      ";
                v_res := bit_vector(shift_right(unsigned(v_op1), v_shamt));
                stats_v(I_SRLI) := stats_v(I_SRLI) + 1;
              end if;
            when F3_OR =>
              -- ORI
              v_mnemonic_str := "ORI       ";
              v_res := bit_vector(v_op1 or signed(v_imm_i));
              stats_v(I_ORI) := stats_v(I_ORI) + 1;
            when F3_AND =>
              -- ANDI
              v_mnemonic_str := "ANDI      ";
              v_res := bit_vector(v_op1 and signed(v_imm_i));
              stats_v(I_ANDI) := stats_v(I_ANDI) + 1;
            when others => null;
          end case;
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   v_mnemonic_str & " R" & integer'image(to_integer(unsigned(rd))) & ", R" & integer'image(to_integer(unsigned(rs1))) & ", " & integer'image(to_integer(signed(v_imm_i))) & " (0x" & to_hstring(bit_vector(v_op1)) & ", 0x" & to_hstring(v_imm_i) & ") = 0x" & to_hstring(v_res);
          end if;
          -- synthesis translate_on
          pending_rd  := rd;
          pending_res := v_res;

        -- =====================================================================
        -- R-Type Instructions (Arithmetic & Logical with Registers)
        -- =====================================================================
        when OPC_OP =>
          -- Arithmetic & Logical Operations with Registers
          case funct3 is
            when F3_ADD_SUB =>
              if funct7(5) = '1' then
                v_mnemonic_str := "SUB       ";
                v_res := bit_vector(v_op1 - v_op2);
                stats_v(I_SUB) := stats_v(I_SUB) + 1;
              else
                v_mnemonic_str := "ADD       ";
                v_res := bit_vector(v_op1 + v_op2);
                stats_v(I_ADD) := stats_v(I_ADD) + 1;
              end if;
            when F3_SLL =>
              v_mnemonic_str := "SLL       ";
              v_shamt := to_integer(unsigned(v_op2(4 downto 0)));
              v_res := bit_vector(shift_left(unsigned(v_op1), v_shamt));
              stats_v(I_SLL) := stats_v(I_SLL) + 1;
            when F3_SLT =>
              v_mnemonic_str := "SLT       ";
              if v_op1 < v_op2 then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats_v(I_SLT) := stats_v(I_SLT) + 1;
            when F3_SLTU =>
              v_mnemonic_str := "SLTU      ";
              if unsigned(v_op1) < unsigned(v_op2) then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats_v(I_SLTU) := stats_v(I_SLTU) + 1;
            when F3_XOR =>
              v_mnemonic_str := "XOR       ";
              v_res := bit_vector(v_op1 xor v_op2);
              stats_v(I_XOR) := stats_v(I_XOR) + 1;
            when F3_SRL_SRA =>
              if funct7(5) = '1' then
                v_mnemonic_str := "SRA       ";
                v_res := bit_vector(shift_right(v_op1, to_integer(unsigned(v_op2(4 downto 0)))));
                stats_v(I_SRA) := stats_v(I_SRA) + 1;
              else
                v_mnemonic_str := "SRL       ";
                v_res := bit_vector(shift_right(unsigned(v_op1), to_integer(unsigned(v_op2(4 downto 0)))));
                stats_v(I_SRL) := stats_v(I_SRL) + 1;
              end if;
            when F3_OR =>
              v_mnemonic_str := "OR        ";
              v_res := bit_vector(v_op1 or v_op2);
              stats_v(I_OR) := stats_v(I_OR) + 1;
            when F3_AND =>
              v_mnemonic_str := "AND       ";
              v_res := bit_vector(v_op1 and v_op2);
              stats_v(I_AND) := stats_v(I_AND) + 1;
            when others => null;
          end case;
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   v_mnemonic_str & " R" & integer'image(to_integer(unsigned(rd))) & ", R" & integer'image(to_integer(unsigned(rs1))) & ", R" & integer'image(to_integer(unsigned(rs2))) & " (0x" & to_hstring(bit_vector(v_op1)) & ", 0x" & to_hstring(bit_vector(v_op2)) & ") = 0x" & to_hstring(v_res);
          end if;
          -- synthesis translate_on
          pending_rd  := rd;
          pending_res := v_res;

        -- =====================================================================
        -- MISC-MEM Instructions (FENCE, FENCE.I)
        -- =====================================================================
        when OPC_MISC_MEM =>
          -- FENCE, FENCE.I
          -- These instructions are typically NOPs in simulation
          -- synthesis translate_off
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " &
                   "FENCE";
          end if;
          -- synthesis translate_on

        -- =====================================================================
        -- SYSTEM Instructions (ECALL, EBREAK, CSR)
        -- =====================================================================
        when OPC_SYSTEM =>
          -- ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI
          case funct3 is
            when F3_PRIV =>
              -- ECALL, EBREAK
              v_mnemonic_str := "UNKNOWN   ";
              if funct12 = x"000" then
                -- ECALL - Environment Call
                v_mnemonic_str := "ECALL     ";
              elsif funct12 = x"001" then
                -- EBREAK - Environment Breakpoint
                v_mnemonic_str := "EBREAK    ";
              end if;
              -- synthesis translate_off
              if verbose_r then
                report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " & v_mnemonic_str;
              end if;
              -- synthesis translate_on
            when F3_CSRRW | F3_CSRRS | F3_CSRRC | F3_CSRRWI | F3_CSRRSI | F3_CSRRCI =>
              -- CSR instructions (require CSR register file support)
              -- synthesis translate_off
              if verbose_r then
                report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " & "CSR_OP";
              end if;
              -- synthesis translate_on
            when others => null;
          end case;

        when others =>
          -- Unknown instruction - NOP
          if verbose_r then
            report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " & "UNKNOWN";
          end if;
      end case;
      pending_npc := v_npc;
    end procedure;

    procedure complete is
    begin
      if unsigned(pending_rd) /= 0 then
        regs_r(to_integer(unsigned(pending_rd))) := pending_res;
      end if;
      pc_r := pending_npc;
    end procedure;


    procedure complete_load(
      mem_rdata     : in std_logic_vector(DMEM_DATA_WIDTH-1 downto 0)
    ) is
      variable v_shamt : integer;
      variable v_rdata : bit_vector(31 downto 0);
      variable v_res   : bit_vector(31 downto 0);
    begin
      v_shamt := pending_load_byte_off * 8;
      v_rdata := bit_vector(shift_right(unsigned(to_bitvector(mem_rdata)), v_shamt));
      
      case pending_load_funct3 is
        when F3_LB  => v_res := bit_vector(resize(signed(v_rdata(7 downto 0)),   32));
        when F3_LH  => v_res := bit_vector(resize(signed(v_rdata(15 downto 0)),  32));
        when F3_LW  => v_res := bit_vector(resize(unsigned(to_bitvector(mem_rdata)), 32));
        when F3_LBU => v_res := bit_vector(resize(unsigned(v_rdata(7 downto 0)),   32));
        when F3_LHU => v_res := bit_vector(resize(unsigned(v_rdata(15 downto 0)),  32));
        when others => v_res := bit_vector(resize(unsigned(to_bitvector(mem_rdata)), 32));
      end case;

      -- synthesis translate_off
      if verbose_r
      then
        report "[ISS] Complete Load: R" & integer'image(to_integer(unsigned(pending_rd))) & " data=0x" & to_hstring(mem_rdata) & " offset=" & integer'image(pending_load_byte_off) & " -> final_res=0x" & to_hstring(v_res);
      end if;
      -- synthesis translate_on

      pending_res := v_res;
      end procedure;

    procedure stats(filename : in string := "") is
      variable v_ratio : real;
      file f_out       : text;
      variable l       : line;
      variable v_open  : boolean := false;
    begin
      if filename /= ""
      then
        file_open(f_out, filename, write_mode);
        v_open := true;
      end if;

      report "--- WardRV ISS Statistics ---";
      for i in inst_type_t
      loop
        if stats_v(I_TOTAL) > 0
        then
          v_ratio := (real(stats_v(i)) * 100.0) / real(stats_v(I_TOTAL));
        else
          v_ratio := 0.0;
        end if;
        report INST_NAMES(i) & " : " & integer'image(stats_v(i)) & " (" & to_string(v_ratio, 2) & " %)";
        if v_open
        then
          write(l, INST_NAMES(i) & " : " & integer'image(stats_v(i)) & " (" & to_string(v_ratio, 2) & " %)");
          writeline(f_out, l);
        end if;
      end loop;
      report "-----------------------------";

      if v_open
      then
        file_close(f_out);
      end if;
    end procedure;

  end protected body;

end package body;
