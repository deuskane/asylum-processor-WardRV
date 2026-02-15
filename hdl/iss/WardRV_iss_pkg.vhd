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
use ieee.numeric_std.all;
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

    -- Complete a Load operation
    procedure complete_load(
      mem_rdata     : in std_logic_vector(DMEM_DATA_WIDTH-1 downto 0)
    );

    -- Statistics
    procedure print_stats;

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

  type stats_array_t is array (inst_type_t) of integer;

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

  type regfile_t is array (0 to 31) of std_logic_vector(31 downto 0);

  type iss_t is protected body
    
    variable pc_r   : std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0);
    variable regs_r : regfile_t;
    
    -- State for pending load
    variable pending_load_rd     : std_logic_vector(4 downto 0);
    variable pending_load_funct3 : std_logic_vector(2 downto 0);
    variable pending_load_byte_off : integer;

    -- Statistics counters
    variable stats : stats_array_t;

    procedure reset(
      start_pc : in std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0)
    ) is
    begin
      pc_r   := start_pc;
      regs_r := (others => (others => '0'));
      stats  := (others => 0);
      pending_load_rd := (others => '0');
    end procedure;

    impure function get_pc return std_logic_vector is
    begin
      return pc_r;
    end function;

    impure function get_reg(r : integer) return std_logic_vector is
    begin
      if r = 0 then return x"00000000"; end if;
      return regs_r(r);
    end function;

    procedure execute_instruction(
      inst          : in  std_logic_vector(31 downto 0);
      mem_req       : out boolean;
      mem_we        : out std_logic;
      mem_addr      : out std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      mem_wdata     : out std_logic_vector(DMEM_DATA_WIDTH-1 downto 0);
      mem_be        : out std_logic_vector((DMEM_DATA_WIDTH/8)-1 downto 0)
    ) is
      alias opcode : std_logic_vector(6 downto 0) is inst(6 downto 0);
      alias rd     : std_logic_vector(4 downto 0) is inst(11 downto 7);
      alias funct3 : std_logic_vector(2 downto 0) is inst(14 downto 12);
      alias rs1    : std_logic_vector(4 downto 0) is inst(19 downto 15);
      alias rs2    : std_logic_vector(4 downto 0) is inst(24 downto 20);
      alias funct7 : std_logic_vector(6 downto 0) is inst(31 downto 25);
      alias funct12: std_logic_vector(11 downto 0) is inst(31 downto 20);

      variable v_imm_i : std_logic_vector(31 downto 0);
      variable v_imm_s : std_logic_vector(31 downto 0);
      variable v_imm_b : std_logic_vector(31 downto 0);
      variable v_imm_u : std_logic_vector(31 downto 0);
      variable v_imm_j : std_logic_vector(31 downto 0);
      variable v_op1   : signed(31 downto 0);
      variable v_op2   : signed(31 downto 0);
      variable v_res   : std_logic_vector(31 downto 0);
      variable v_pc    : std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0);
      variable v_npc   : std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0);
      variable v_addr  : std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      variable v_shamt : integer;
      variable v_msg   : line;
    begin
      -- Initialize outputs
      mem_req   := false;
      mem_we    := '0';
      mem_addr  := (others => '0');
      mem_wdata := (others => '0');
      mem_be    := (others => '0');

      -- Immediates
      v_imm_i := std_logic_vector(resize(signed(inst(31 downto 20)), 32));
      v_imm_s := std_logic_vector(resize(signed(std_logic_vector'(inst(31 downto 25) & inst(11 downto 7))), 32));
      v_imm_b := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8) & '0')), 32));
      v_imm_u := inst(31 downto 12) & x"000";
      v_imm_j := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21) & '0')), 32));

      -- Operands
      if unsigned(rs1) = 0 then v_op1 := (others => '0'); else v_op1 := signed(regs_r(to_integer(unsigned(rs1)))); end if;
      if unsigned(rs2) = 0 then v_op2 := (others => '0'); else v_op2 := signed(regs_r(to_integer(unsigned(rs2)))); end if;

      -- Default Next PC
      v_pc  := pc_r;
      v_npc := std_logic_vector(unsigned(pc_r) + 4);
      v_res := (others => '0');

      stats(I_TOTAL) := stats(I_TOTAL) + 1;

      case opcode is
        -- =====================================================================
        -- U-Type Instructions
        -- =====================================================================
        when OPC_LUI =>
          -- Load Upper Immediate
          write(v_msg, string'("LUI R") & integer'image(to_integer(unsigned(rd))) & ", 0x" & to_hstring(v_imm_u) & " = 0x" & to_hstring(v_imm_u));
          v_res := v_imm_u;
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;
          stats(I_LUI) := stats(I_LUI) + 1;

        when OPC_AUIPC =>
          -- Add Upper Immediate to PC
          v_res := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_u));
          write(v_msg, string'("AUIPC R") & integer'image(to_integer(unsigned(rd))) & ", 0x" & to_hstring(v_imm_u) & " (PC=0x" & to_hstring(pc_r) & ") = 0x" & to_hstring(v_res));
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;
          stats(I_AUIPC) := stats(I_AUIPC) + 1;

        -- =====================================================================
        -- J-Type Instructions
        -- =====================================================================
        when OPC_JAL =>
          -- Jump and Link
          v_res := std_logic_vector(unsigned(pc_r) + 4);
          v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_j));
          write(v_msg, string'("JAL R") & integer'image(to_integer(unsigned(rd))) & ", 0x" & to_hstring(v_imm_j) & " (Link=0x" & to_hstring(v_res) & ", NPC=0x" & to_hstring(v_npc) & ")");
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;
          stats(I_JAL) := stats(I_JAL) + 1;

        -- =====================================================================
        -- I-Type Instructions (JALR, Loads, OP_IMM)
        -- =====================================================================
        when OPC_JALR =>
          -- Jump and Link Register
          v_res := std_logic_vector(unsigned(pc_r) + 4);
          v_npc := std_logic_vector(unsigned(unsigned(v_op1) + unsigned(v_imm_i)) and x"FFFFFFFE");
          write(v_msg, string'("JALR R") & integer'image(to_integer(unsigned(rd))) & ", R" & integer'image(to_integer(unsigned(rs1))) & ", " & integer'image(to_integer(signed(v_imm_i))) & " (R" & integer'image(to_integer(unsigned(rs1))) & "=0x" & to_hstring(v_op1) & ", Link=0x" & to_hstring(v_res) & ", NPC=0x" & to_hstring(v_npc) & ")");
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;
          stats(I_JALR) := stats(I_JALR) + 1;

        -- =====================================================================
        -- B-Type Instructions (Branch)
        -- =====================================================================
        when OPC_BRANCH =>
          -- Conditional Branch Instructions
          case funct3 is
            when F3_BEQ  => write(v_msg, string'("BEQ"));  if v_op1 = v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats(I_BEQ) := stats(I_BEQ) + 1;
            when F3_BNE  => write(v_msg, string'("BNE"));  if v_op1 /= v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats(I_BNE) := stats(I_BNE) + 1;
            when F3_BLT  => write(v_msg, string'("BLT"));  if v_op1 < v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats(I_BLT) := stats(I_BLT) + 1;
            when F3_BGE  => write(v_msg, string'("BGE"));  if v_op1 >= v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats(I_BGE) := stats(I_BGE) + 1;
            when F3_BLTU => write(v_msg, string'("BLTU")); if unsigned(v_op1) < unsigned(v_op2) then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats(I_BLTU) := stats(I_BLTU) + 1;
            when F3_BGEU => write(v_msg, string'("BGEU")); if unsigned(v_op1) >= unsigned(v_op2) then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if; stats(I_BGEU) := stats(I_BGEU) + 1;
            when others => write(v_msg, string'("BRANCH_UNK"));
          end case;
          write(v_msg, string'(" R") & integer'image(to_integer(unsigned(rs1))) & ", R" & integer'image(to_integer(unsigned(rs2))) & ", 0x" & to_hstring(v_imm_b) & " (0x" & to_hstring(v_op1) & ", 0x" & to_hstring(v_op2) & ") NPC=0x" & to_hstring(v_npc));
          pc_r := v_npc;

        -- =====================================================================
        -- S-Type Instructions (Load)
        -- =====================================================================
        when OPC_LOAD =>
          -- Load Instructions: LB, LH, LW, LBU, LHU
          case funct3 is
            when F3_LB  => write(v_msg, string'("LB"));  stats(I_LB) := stats(I_LB) + 1;
            when F3_LH  => write(v_msg, string'("LH"));  stats(I_LH) := stats(I_LH) + 1;
            when F3_LW  => write(v_msg, string'("LW"));  stats(I_LW) := stats(I_LW) + 1;
            when F3_LBU => write(v_msg, string'("LBU")); stats(I_LBU) := stats(I_LBU) + 1;
            when F3_LHU => write(v_msg, string'("LHU")); stats(I_LHU) := stats(I_LHU) + 1;
            when others => write(v_msg, string'("LOAD_UNK"));
          end case;
          v_addr := std_logic_vector(resize(unsigned(v_op1) + unsigned(v_imm_i), DMEM_ADDR_WIDTH));
          write(v_msg, string'(" R") & integer'image(to_integer(unsigned(rd))) & ", " & integer'image(to_integer(signed(v_imm_i))) & "(R" & integer'image(to_integer(unsigned(rs1))) & ") (Addr=0x" & to_hstring(v_addr) & ")");
          mem_req  := true;
          mem_we   := '0';
          mem_addr := v_addr;
          
          -- Save state for completion
          pending_load_rd       := rd;
          pending_load_funct3   := funct3;
          pending_load_byte_off := to_integer(unsigned(v_addr(1 downto 0)));
          --pending_load_byte_off := 0;
          
          pc_r := v_npc;

        -- =====================================================================
        -- S-Type Instructions (Store)
        -- =====================================================================
        when OPC_STORE =>
          -- Store Instructions: SB, SH, SW
          case funct3 is
            when F3_SB  => write(v_msg, string'("SB")); stats(I_SB) := stats(I_SB) + 1;
            when F3_SH  => write(v_msg, string'("SH")); stats(I_SH) := stats(I_SH) + 1;
            when F3_SW  => write(v_msg, string'("SW")); stats(I_SW) := stats(I_SW) + 1;
            when others => write(v_msg, string'("STORE_UNK"));
          end case;
          v_addr := std_logic_vector(resize(unsigned(v_op1) + unsigned(v_imm_s), DMEM_ADDR_WIDTH));
          write(v_msg, string'(" R") & integer'image(to_integer(unsigned(rs2))) & ", " & integer'image(to_integer(signed(v_imm_s))) & "(R" & integer'image(to_integer(unsigned(rs1))) & ") (Addr=0x" & to_hstring(v_addr) & ", Data=0x" & to_hstring(v_op2) & ")");
          mem_req   := true;
          mem_we    := '1';
          mem_addr  := v_addr;
          v_shamt   := to_integer(unsigned(v_addr(1 downto 0))) * 8;
          mem_wdata := std_logic_vector(shift_left(unsigned(v_op2), v_shamt));
          
          case funct3 is
            when F3_SB  => mem_be := std_logic_vector(shift_left(unsigned'("0001"), to_integer(unsigned(v_addr(1 downto 0)))));
            when F3_SH  => mem_be := std_logic_vector(shift_left(unsigned'("0011"), to_integer(unsigned(v_addr(1 downto 0)))));
            when F3_SW  => mem_be := (others => '1');
            when others => mem_be := (others => '0');
          end case;
          pc_r := v_npc;

        -- =====================================================================
        -- I-Type Instructions (Arithmetic & Logical with Immediate)
        -- =====================================================================
        when OPC_OP_IMM =>
          -- Arithmetic & Logical Operations with Immediate
          case funct3 is
            when F3_ADD_SUB =>
              -- ADDI
              write(v_msg, string'("ADDI"));
              v_res := std_logic_vector(v_op1 + signed(v_imm_i));
              stats(I_ADDI) := stats(I_ADDI) + 1;
            when F3_SLL =>
              -- SLLI
              write(v_msg, string'("SLLI"));
              v_shamt := to_integer(unsigned(v_imm_i(4 downto 0)));
              v_res := std_logic_vector(shift_left(unsigned(v_op1), v_shamt));
              stats(I_SLLI) := stats(I_SLLI) + 1;
            when F3_SLT =>
              -- SLTI
              write(v_msg, string'("SLTI"));
              if v_op1 < signed(v_imm_i) then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats(I_SLTI) := stats(I_SLTI) + 1;
            when F3_SLTU =>
              -- SLTIU
              write(v_msg, string'("SLTIU"));
              if unsigned(v_op1) < unsigned(v_imm_i) then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats(I_SLTIU) := stats(I_SLTIU) + 1;
            when F3_XOR =>
              -- XORI
              write(v_msg, string'("XORI"));
              v_res := std_logic_vector(v_op1 xor signed(v_imm_i));
              stats(I_XORI) := stats(I_XORI) + 1;
            when F3_SRL_SRA =>
              -- SRLI / SRAI
              v_shamt := to_integer(unsigned(v_imm_i(4 downto 0)));
              if funct7(5) = '1' then
                -- SRAI (Arithmetic Right Shift)
                write(v_msg, string'("SRAI"));
                v_res := std_logic_vector(shift_right(v_op1, v_shamt));
                stats(I_SRAI) := stats(I_SRAI) + 1;
              else
                -- SRLI (Logical Right Shift)
                write(v_msg, string'("SRLI"));
                v_res := std_logic_vector(shift_right(unsigned(v_op1), v_shamt));
                stats(I_SRLI) := stats(I_SRLI) + 1;
              end if;
            when F3_OR =>
              -- ORI
              write(v_msg, string'("ORI"));
              v_res := std_logic_vector(v_op1 or signed(v_imm_i));
              stats(I_ORI) := stats(I_ORI) + 1;
            when F3_AND =>
              -- ANDI
              write(v_msg, string'("ANDI"));
              v_res := std_logic_vector(v_op1 and signed(v_imm_i));
              stats(I_ANDI) := stats(I_ANDI) + 1;
            when others => null;
          end case;
          write(v_msg, string'(" R") & integer'image(to_integer(unsigned(rd))) & ", R" & integer'image(to_integer(unsigned(rs1))) & ", " & integer'image(to_integer(signed(v_imm_i))) & " (0x" & to_hstring(v_op1) & ", 0x" & to_hstring(v_imm_i) & ") = 0x" & to_hstring(v_res));
          
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        -- =====================================================================
        -- R-Type Instructions (Arithmetic & Logical with Registers)
        -- =====================================================================
        when OPC_OP =>
          -- Arithmetic & Logical Operations with Registers
          case funct3 is
            when F3_ADD_SUB =>
              if funct7(5) = '1' then
                write(v_msg, string'("SUB"));
                v_res := std_logic_vector(v_op1 - v_op2);
                stats(I_SUB) := stats(I_SUB) + 1;
              else
                write(v_msg, string'("ADD"));
                v_res := std_logic_vector(v_op1 + v_op2);
                stats(I_ADD) := stats(I_ADD) + 1;
              end if;
            when F3_SLL =>
              write(v_msg, string'("SLL"));
              v_shamt := to_integer(unsigned(v_op2(4 downto 0)));
              v_res := std_logic_vector(shift_left(unsigned(v_op1), v_shamt));
              stats(I_SLL) := stats(I_SLL) + 1;
            when F3_SLT =>
              write(v_msg, string'("SLT"));
              if v_op1 < v_op2 then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats(I_SLT) := stats(I_SLT) + 1;
            when F3_SLTU =>
              write(v_msg, string'("SLTU"));
              if unsigned(v_op1) < unsigned(v_op2) then v_res := x"00000001"; else v_res := (others => '0'); end if;
              stats(I_SLTU) := stats(I_SLTU) + 1;
            when F3_XOR =>
              write(v_msg, string'("XOR"));
              v_res := std_logic_vector(v_op1 xor v_op2);
              stats(I_XOR) := stats(I_XOR) + 1;
            when F3_SRL_SRA =>
              if funct7(5) = '1' then
                write(v_msg, string'("SRA"));
                v_res := std_logic_vector(shift_right(v_op1, to_integer(unsigned(v_op2(4 downto 0)))));
                stats(I_SRA) := stats(I_SRA) + 1;
              else
                write(v_msg, string'("SRL"));
                v_res := std_logic_vector(shift_right(unsigned(v_op1), to_integer(unsigned(v_op2(4 downto 0)))));
                stats(I_SRL) := stats(I_SRL) + 1;
              end if;
            when F3_OR =>
              write(v_msg, string'("OR"));
              v_res := std_logic_vector(v_op1 or v_op2);
              stats(I_OR) := stats(I_OR) + 1;
            when F3_AND =>
              write(v_msg, string'("AND"));
              v_res := std_logic_vector(v_op1 and v_op2);
              stats(I_AND) := stats(I_AND) + 1;
            when others => null;
          end case;
          write(v_msg, string'(" R") & integer'image(to_integer(unsigned(rd))) & ", R" & integer'image(to_integer(unsigned(rs1))) & ", R" & integer'image(to_integer(unsigned(rs2))) & " (0x" & to_hstring(v_op1) & ", 0x" & to_hstring(v_op2) & ") = 0x" & to_hstring(v_res));
          
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        -- =====================================================================
        -- MISC-MEM Instructions (FENCE, FENCE.I)
        -- =====================================================================
        when OPC_MISC_MEM =>
          -- FENCE, FENCE.I
          -- These instructions are typically NOPs in simulation
          write(v_msg, string'("FENCE"));
          pc_r := v_npc;

        -- =====================================================================
        -- SYSTEM Instructions (ECALL, EBREAK, CSR)
        -- =====================================================================
        when OPC_SYSTEM =>
          -- ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI
          case funct3 is
            when F3_PRIV =>
              -- ECALL, EBREAK
              if funct12 = x"000" then
                -- ECALL - Environment Call
                write(v_msg, string'("ECALL"));
              elsif funct12 = x"001" then
                -- EBREAK - Environment Breakpoint
                write(v_msg, string'("EBREAK"));
              end if;
            when F3_CSRRW | F3_CSRRS | F3_CSRRC | F3_CSRRWI | F3_CSRRSI | F3_CSRRCI =>
              -- CSR instructions (require CSR register file support)
              write(v_msg, string'("CSR_OP"));
            when others => null;
          end case;
          pc_r := v_npc;

        when others =>
          -- Unknown instruction - NOP
          write(v_msg, string'("UNKNOWN"));
          pc_r := v_npc;
      end case;

      -- synthesis translate_off
      if v_msg /= null then
        report "[ISS] PC=0x" & to_hstring(v_pc) & " NPC=0x" & to_hstring(v_npc) & " : " & v_msg.all;
        deallocate(v_msg);
      end if;
      -- synthesis translate_on
    end procedure;

    procedure complete_load(
      mem_rdata     : in std_logic_vector(DMEM_DATA_WIDTH-1 downto 0)
    ) is
      variable v_shamt : integer;
      variable v_rdata : std_logic_vector(31 downto 0);
      variable v_res   : std_logic_vector(31 downto 0);
      variable v_msg   : line;
    begin
      -- synthesis translate_off
      write(v_msg, string'("Complete Load: R") & integer'image(to_integer(unsigned(pending_load_rd))) & " data=0x" & to_hstring(mem_rdata) & " offset=" & integer'image(pending_load_byte_off));
      -- synthesis translate_on

      v_shamt := pending_load_byte_off * 8;
      v_rdata := std_logic_vector(shift_right(unsigned(mem_rdata), v_shamt));
      
      case pending_load_funct3 is
        when F3_LB  => v_res := std_logic_vector(resize(signed(v_rdata(7 downto 0)), 32));
        when F3_LH  => v_res := std_logic_vector(resize(signed(v_rdata(15 downto 0)), 32));
        when F3_LW  => v_res := std_logic_vector(resize(unsigned(mem_rdata), 32));
        when F3_LBU => v_res := std_logic_vector(resize(unsigned(v_rdata(7 downto 0)), 32));
        when F3_LHU => v_res := std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
        when others => v_res := std_logic_vector(resize(unsigned(mem_rdata), 32));
      end case;

      -- synthesis translate_off
      write(v_msg, string'(" -> final_res=0x") & to_hstring(v_res));
      report "[ISS] " & v_msg.all;
      deallocate(v_msg);
      -- synthesis translate_on

      if unsigned(pending_load_rd) /= 0 then 
        regs_r(to_integer(unsigned(pending_load_rd))) := v_res; 
      end if;
    end procedure;

    procedure print_stats is
      variable v_ratio : real;
    begin
      report "--- WardRV ISS Statistics ---";
      for i in inst_type_t loop
        if stats(I_TOTAL) > 0 then
          v_ratio := (real(stats(i)) * 100.0) / real(stats(I_TOTAL));
        else
          v_ratio := 0.0;
        end if;
        report INST_NAMES(i) & " : " & integer'image(stats(i)) & " (" & to_string(v_ratio, 2) & " %)";
      end loop;
      report "-----------------------------";
    end procedure;

  end protected body;

end package body;
