-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_fsm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_fsm_alu_pkg.all;

entity WardRV_fsm is
  generic (
    RESET_ADDR : std_logic_vector(31 downto 0) := (others => '0');
    VERBOSE    : boolean                       := true
  );
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    -- Instruction Interface
    inst_ini_o : out inst_ini_t;
    inst_tgt_i : in  inst_tgt_t;

    -- Data Interface
    sbi_ini_o  : out sbi_ini_t;
    sbi_tgt_i  : in  sbi_tgt_t
  );
end entity WardRV_fsm;

architecture behavioural of WardRV_fsm is

  -- State Machine
  type state_t is (S_FETCH_REQ, 
                   S_FETCH_WAIT,
                   S_DECODE, 
                   S_BRANCH_DECISION, 
                   S_MEM_REQ, 
                   S_MEM_WAIT, 
                   S_WRITEBACK);
  signal state   : state_t;

  -- CPU State
  signal pc      : std_logic_vector(31 downto 0);
  signal next_pc : std_logic_vector(31 downto 0);
  
  -- Register File (x0 is hardwired to 0 in logic)
  type regfile_t is array (0 to 31) of std_logic_vector(31 downto 0);
  signal regs     : regfile_t;

  -- Current Instruction
  signal inst     : std_logic_vector(31 downto 0);
  signal inst_bv  : bit_vector(31 downto 0);


  -- Decoded Fields
  alias opcode     : bit_vector      (6 downto 0) is inst_bv(6 downto 0);
  alias rd         : std_logic_vector(4 downto 0) is inst   (11 downto 7);
  alias funct3     : bit_vector      (2 downto 0) is inst_bv(14 downto 12);
  alias rs1        : std_logic_vector(4 downto 0) is inst   (19 downto 15);
  alias rs2        : std_logic_vector(4 downto 0) is inst   (24 downto 20);
  alias funct7     : bit_vector      (6 downto 0) is inst_bv(31 downto 25);

  -- Internal
  signal alu_res   : std_logic_vector(31 downto 0);
  signal mem_addr  : std_logic_vector(31 downto 0);
  signal mem_wdata : std_logic_vector(31 downto 0);
  signal mem_we    : std_logic;
  signal mem_be    : std_logic_vector(3 downto 0);

  -- ALU Status (for Branch Decision)
  signal alu_zero  : std_logic;
  signal alu_sign  : std_logic;

  -- ALU Interconnect
  signal alu_src_a : std_logic_vector(31 downto 0);
  signal alu_src_b : std_logic_vector(31 downto 0);
  signal alu_op    : alu_op_t;
  signal w_alu_res : std_logic_vector(31 downto 0);
  signal w_alu_zero: std_logic;
  signal w_alu_sign: std_logic;

  procedure log(
    msg   : in string
  ) is
  begin
    -- synthesis translate_off
    if VERBOSE then
      report "[WardRV FSM] " & to_hstring(pc) & " : " & to_hstring(inst) & " : " & msg;
    end if;
    -- synthesis translate_on
  end procedure;

begin
  inst_bv <= to_bitvector(inst);

  -- ALU Instance
  u_alu : entity work.WardRV_fsm_alu
  port map (
    src_a_i => alu_src_a,
    src_b_i => alu_src_b,
    op_i    => alu_op,
    res_o   => w_alu_res,
    zero_o  => w_alu_zero,
    sign_o  => w_alu_sign
  );

  -- ALU Control Process (Combinatorial)
  process(all)
    variable v_imm_i : std_logic_vector(31 downto 0);
    variable v_imm_s : std_logic_vector(31 downto 0);
    variable v_imm_b : std_logic_vector(31 downto 0);
    variable v_imm_u : std_logic_vector(31 downto 0);
    variable v_imm_j : std_logic_vector(31 downto 0);
    variable v_src_a : std_logic_vector(31 downto 0);
    variable v_src_b : std_logic_vector(31 downto 0);
  begin
    -- Default ALU Control
    alu_src_a <= (others => '0');
    alu_src_b <= (others => '0');
    alu_op    <= ALU_ADD;

    -- Immediates Decoding
    v_imm_i := std_logic_vector(resize(signed(inst(31 downto 20)), 32));
    v_imm_s := std_logic_vector(resize(signed(std_logic_vector'(inst(31 downto 25) & inst(11 downto 7))), 32));
    v_imm_b := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8) & '0')), 32));
    v_imm_u := inst(31 downto 12) & x"000";
    v_imm_j := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21) & '0')), 32));

    -- Operand Reading
    v_src_a := regs(to_integer(unsigned(rs1)));
    v_src_b := regs(to_integer(unsigned(rs2)));

    case state is
      when S_FETCH_REQ =>
        alu_src_a <= pc;
        alu_src_b <= x"00000004";
        alu_op    <= ALU_ADD;

      when S_DECODE =>
        case opcode is
          when OPC_LUI =>
            alu_src_a <= (others => '0');
            alu_src_b <= v_imm_u;
            alu_op    <= ALU_ADD;
          
          when OPC_AUIPC =>
            alu_src_a <= pc;
            alu_src_b <= v_imm_u;
            alu_op    <= ALU_ADD;

          when OPC_JAL =>
            -- Target calculation
            alu_src_a <= pc;
            alu_src_b <= v_imm_j;
            alu_op    <= ALU_ADD;

          when OPC_JALR =>
            alu_src_a <= v_src_a;
            alu_src_b <= v_imm_i;
            alu_op    <= ALU_ADD;

          when OPC_BRANCH =>
            -- Comparison for flags
            alu_src_a <= v_src_a;
            alu_src_b <= v_src_b;
            alu_op    <= ALU_SUB; 

          when OPC_LOAD =>
            alu_src_a <= v_src_a;
            alu_src_b <= v_imm_i;
            alu_op    <= ALU_ADD;

          when OPC_STORE =>
            alu_src_a <= v_src_a;
            alu_src_b <= v_imm_s;
            alu_op    <= ALU_ADD;

          when OPC_OP_IMM =>
            alu_src_a <= v_src_a;
            alu_src_b <= v_imm_i;
            case funct3 is
              when F3_ADD  => alu_op <= ALU_ADD;
              when F3_SLT  => alu_op <= ALU_SLT;
              when F3_SLTU => alu_op <= ALU_SLTU;
              when F3_XOR  => alu_op <= ALU_XOR;
              when F3_OR   => alu_op <= ALU_OR;
              when F3_AND  => alu_op <= ALU_AND;
              when F3_SLL  => alu_op <= ALU_SLL;
              when F3_SRL  => 
                if v_imm_i(30) = '1' then alu_op <= ALU_SRA; else alu_op <= ALU_SRL; end if;
              when others => null;
            end case;

          when OPC_OP =>
            alu_src_a <= v_src_a;
            alu_src_b <= v_src_b;
            case funct3 is
              when F3_ADD  => 
                if funct7(5) = '1' then alu_op <= ALU_SUB; else alu_op <= ALU_ADD; end if;
              when F3_SLL  => alu_op <= ALU_SLL;
              when F3_SLT  => alu_op <= ALU_SLT;
              when F3_SLTU => alu_op <= ALU_SLTU;
              when F3_XOR  => alu_op <= ALU_XOR;
              when F3_SRL  => 
                if funct7(5) = '1' then alu_op <= ALU_SRA; else alu_op <= ALU_SRL; end if;
              when F3_OR   => alu_op <= ALU_OR;
              when F3_AND  => alu_op <= ALU_AND;
              when others  => null;
            end case;

          when OPC_SYSTEM =>
             -- CSR Pass Through (dummy)
             alu_src_a <= (others => '0');
             alu_src_b <= (others => '0');
             alu_op    <= ALU_PASS_B;

          when others => null;
        end case;

      when S_BRANCH_DECISION =>
        -- Calculate Branch Target
        alu_src_a <= pc;
        alu_src_b <= v_imm_b;
        alu_op    <= ALU_ADD;
      
      -- For S_MEM_REQ/WAIT/WB, alu inputs don't matter much unless we pipeline,
      -- but we must ensure we don't latch garbage if we were using alu_res logic.
      when others =>
        alu_src_a <= (others => '0');
        alu_src_b <= (others => '0');
        alu_op    <= ALU_ADD;
    end case;
  end process;

  process(clk_i, arst_b_i)
    variable v_imm_i : std_logic_vector(31 downto 0);
    variable v_imm_s : std_logic_vector(31 downto 0);
    variable v_imm_b : std_logic_vector(31 downto 0);
    variable v_imm_u : std_logic_vector(31 downto 0);
    variable v_imm_j : std_logic_vector(31 downto 0);
    variable v_op1   : signed(31 downto 0);
    variable v_op2   : signed(31 downto 0);

    variable v_addr  : std_logic_vector(31 downto 0);
    variable v_shamt : integer;
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    if arst_b_i = '0' then
      state            <= S_FETCH_REQ;
      pc               <= RESET_ADDR;
      regs             <= (others => (others => '0'));
      inst_ini_o.valid <= '0';
      sbi_ini_o.valid  <= '0';
      inst             <= (others => '0');
      next_pc          <= (others => '0');
      alu_zero         <= '0';
      alu_sign         <= '0';
    elsif rising_edge(clk_i) then
      
      -- Default Bus Outputs
      inst_ini_o.valid <= '0';
      sbi_ini_o.valid  <= '0';
      sbi_ini_o.we     <= '0';
      sbi_ini_o.be     <= "0000";

      case state is
        -- 1. Fetch Request
        when S_FETCH_REQ =>
          inst_ini_o.valid <= '1';
          inst_ini_o.addr  <= pc;
          next_pc          <= w_alu_res; -- PC + 4 from ALU
          state            <= S_FETCH_WAIT;

        -- 2. Fetch Wait
        when S_FETCH_WAIT =>
          inst_ini_o.valid <= '1';
          inst_ini_o.addr  <= pc;
          if inst_tgt_i.ready = '1' then
            inst <= inst_tgt_i.inst;
            state <= S_DECODE;
            inst_ini_o.valid <= '0';
          end if;

        -- 3. Decode & Execute (Behavioral)
        when S_DECODE =>
          -- Immediates
          v_imm_i := std_logic_vector(resize(signed(inst(31 downto 20)), 32));
          v_imm_s := std_logic_vector(resize(signed(std_logic_vector'(inst(31 downto 25) & inst(11 downto 7))), 32)); -- Store
          v_imm_b := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8) & '0')), 32));
          v_imm_u := inst(31 downto 12) & x"000";
          v_imm_j := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21) & '0')), 32));

          -- Read Operands for Log/Mem calculations (ALU handled in comb process)
          v_op1 := signed(regs(to_integer(unsigned(rs1))));
          v_op2 := signed(regs(to_integer(unsigned(rs2))));
          
          mem_we <= '0';

          case opcode is
            when OPC_LUI => -- LUI
              log("LUI  x" & integer'image(to_integer(unsigned(rd))) & " = " & to_hstring(v_imm_u) & " # " & to_hstring(w_alu_res));
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_AUIPC => -- AUIPC
              log("AUIPC x" & integer'image(to_integer(unsigned(rd))) & " # " & to_hstring(w_alu_res));
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_JAL => -- JAL
              log("JAL  x" & integer'image(to_integer(unsigned(rd))) & " # " & to_hstring(next_pc));
              pc <= w_alu_res; -- Target from ALU
              alu_res  <= next_pc; -- Hack to pass next_pc to WB via alu_res signal
              state <= S_WRITEBACK;

            when OPC_JALR => -- JALR
              log("JALR x" & integer'image(to_integer(unsigned(rd))) & " # " & to_hstring(next_pc) & " : " & to_hstring(std_logic_vector(v_op1)));
              pc <= w_alu_res; -- Target from ALU
              alu_res  <= next_pc; 
              state <= S_WRITEBACK;

            when OPC_BRANCH => -- BRANCH
              case funct3 is
                when F3_BEQ  => log("BEQ x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & ", target" & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_BNE  => log("BNE x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & ", target" & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_BLT  => log("BLT x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & ", target" & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_BGE  => log("BGE x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & ", target" & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_BLTU => log("BLTU x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & ", target" & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_BGEU => log("BGEU x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & ", target" & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when others => null;
              end case;
              state <= S_BRANCH_DECISION;

            when OPC_LOAD => -- LOAD
              log("LOAD x" & integer'image(to_integer(unsigned(rd))) & " from " & to_hstring(std_logic_vector(unsigned(v_op1) + unsigned(v_imm_i))) & " # " & to_hstring(std_logic_vector(v_op1)));
              mem_addr <= w_alu_res;
              state <= S_MEM_REQ;

            when OPC_STORE => -- STORE
              v_addr := w_alu_res; -- From ALU (Calc in comb process)
              log("STORE from x" & integer'image(to_integer(unsigned(rs2))) & " to " & to_hstring(v_addr) & " # " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
              mem_addr <= v_addr;
              v_shamt  := to_integer(unsigned(v_addr(1 downto 0))) * 8;
              mem_wdata <= std_logic_vector(shift_left(unsigned(v_op2), v_shamt));
              mem_we <= '1';
              case funct3 is
                when F3_SB  => mem_be <= std_logic_vector(shift_left(unsigned'("0001"), to_integer(unsigned(v_addr(1 downto 0)))));
                when F3_SH  => mem_be <= std_logic_vector(shift_left(unsigned'("0011"), to_integer(unsigned(v_addr(1 downto 0)))));
                when F3_SW  => mem_be <= "1111"; -- Word
                when others => mem_be <= "0000";
              end case;
              state <= S_MEM_REQ;

            when OPC_OP_IMM => -- OP-IMM
              case funct3 is
                when F3_ADD  => log("ADDI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", " & to_hstring(v_imm_i) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_SLT  => log("SLTI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", " & to_hstring(v_imm_i) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_SLTU => log("SLTIU x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", " & to_hstring(v_imm_i) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_XOR  => log("XORI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", " & to_hstring(v_imm_i) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_OR   => log("ORI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", " & to_hstring(v_imm_i) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_AND  => log("ANDI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", " & to_hstring(v_imm_i) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_SLL  => log("SLLI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", shamt" & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                when F3_SRL  => 
                  if v_imm_i(30) = '1' then 
                    log("SRAI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", shamt" & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                  else
                    log("SRLI x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", shamt" & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)));
                  end if;
                when others => null;
              end case;
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_OP => -- OP
              case funct3 is
                when F3_ADD  => 
                  if funct7(5) = '1' then 
                    log("SUB x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                  else
                    log("ADD x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                  end if;
                when F3_SLL  => log("SLL x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_SLT  => log("SLT x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_SLTU => log("SLTU x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_XOR  => log("XOR x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_SRL  => 
                  if funct7(5) = '1' then
                    log("SRA x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                  else
                    log("SRL x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                  end if;
                when F3_OR   => log("OR x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when F3_AND  => log("AND x" & integer'image(to_integer(unsigned(rd))) & ", x" & integer'image(to_integer(unsigned(rs1))) & ", x" & integer'image(to_integer(unsigned(rs2))) & " # " & to_hstring(w_alu_res) & " : " & to_hstring(std_logic_vector(v_op1)) & " " & to_hstring(std_logic_vector(v_op2)));
                when others  => null;
              end case;
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_MISC_MEM => -- FENCE / FENCE.I
              log("FENCE");
              state <= S_FETCH_REQ;

            when OPC_SYSTEM => -- SYSTEM
              case funct3 is
                when F3_PRIV => -- ECALL / EBREAK
                  log("SYSTEM PRIV (Ignored)");
                  state <= S_FETCH_REQ;
                when others => -- CSR Instructions (CSRRW, CSRRS, etc.)
                  -- Simplified: Read 0, Write Ignored, just for compliance
                  log("SYSTEM CSR x" & integer'image(to_integer(unsigned(rd))));
                  alu_res <= w_alu_res; -- (0)
                  state <= S_WRITEBACK;
              end case;

            when others => -- NOP
              state <= S_WRITEBACK;
          end case;

          -- Update Flags from ALU (Combinatorial inputs valid for this state)
          alu_zero <= w_alu_zero;
          alu_sign <= w_alu_sign;
          
        -- 3.b Branch Decision
        when S_BRANCH_DECISION =>
           -- Check result of Comparison (performed in S_DECODE, latched in alu_zero/alu_sign)
           
           -- Default: Next is PC+4 (already in next_pc)
           state <= S_WRITEBACK; -- Update PC with next_pc

           case funct3 is
             when F3_BEQ  => if alu_zero = '1' then state <= S_BRANCH_DECISION; end if; -- Taken
             when F3_BNE  => if alu_zero = '0' then state <= S_BRANCH_DECISION; end if;
             when F3_BLT  => if alu_sign = '1' then state <= S_BRANCH_DECISION; end if;
             when F3_BGE  => if alu_sign = '0' then state <= S_BRANCH_DECISION; end if;
             when F3_BLTU => if alu_sign = '1' then state <= S_BRANCH_DECISION; end if; -- Unsigned comparison via S_DECODE op
             when F3_BGEU => if alu_sign = '0' then state <= S_BRANCH_DECISION; end if;
             when others => null;
           end case;
           
           -- Reuse state to indicate "Taken"? 
           -- If Taken, we update next_pc with Target (v_alu_res).
           -- Since we are in the same state case, we can check condition and write next_pc.
           if (funct3 = F3_BEQ and alu_zero = '1') or
                (funct3 = F3_BNE and alu_zero = '0') or
                (funct3 = F3_BLT and alu_sign = '1') or
                (funct3 = F3_BGE and alu_sign = '0') or
                (funct3 = F3_BLTU and alu_sign = '1') or
                (funct3 = F3_BGEU and alu_sign = '0') then
              -- Taken
              next_pc <= w_alu_res; -- ALU computed Target (PC+ImmB) in this state
              -- Flag logic:
              -- log("BRANCH TAKEN");
           else
              -- log("BRANCH NOT TAKEN");
              -- If not taken, we just go to WB with existing next_pc.
           end if;
           state <= S_WRITEBACK;

        -- 4. Memory Access
        when S_MEM_REQ =>
          sbi_ini_o.valid <= '1';
          sbi_ini_o.addr  <= mem_addr;
          sbi_ini_o.wdata <= mem_wdata;
          sbi_ini_o.we    <= mem_we;
          sbi_ini_o.be    <= mem_be;
          if sbi_tgt_i.ready = '1' then
             if mem_we = '0' then
               v_shamt := to_integer(unsigned(mem_addr(1 downto 0))) * 8;
               v_rdata := std_logic_vector(shift_right(unsigned(sbi_tgt_i.rdata), v_shamt));
               case funct3 is
                 when F3_LB  => alu_res <= std_logic_vector(resize(signed(v_rdata(7 downto 0)), 32));
                 when F3_LH  => alu_res <= std_logic_vector(resize(signed(v_rdata(15 downto 0)), 32));
                 when F3_LW  => alu_res <= sbi_tgt_i.rdata;
                 when F3_LBU => alu_res <= std_logic_vector(resize(unsigned(v_rdata(7 downto 0)), 32));
                 when F3_LHU => alu_res <= std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
                 when others => alu_res <= sbi_tgt_i.rdata;
               end case;
             end if;
             if mem_we = '1' then state <= S_FETCH_REQ; pc <= next_pc; else state <= S_WRITEBACK; end if;
          else
             state <= S_MEM_WAIT;
          end if;

        when S_MEM_WAIT =>
          sbi_ini_o.valid <= '1';
          sbi_ini_o.addr  <= mem_addr;
          sbi_ini_o.wdata <= mem_wdata;
          sbi_ini_o.we    <= mem_we;
          sbi_ini_o.be    <= mem_be;
          if sbi_tgt_i.ready = '1' then
             if mem_we = '0' then
               v_shamt := to_integer(unsigned(mem_addr(1 downto 0))) * 8;
               v_rdata := std_logic_vector(shift_right(unsigned(sbi_tgt_i.rdata), v_shamt));
               case funct3 is
                 when F3_LB  => alu_res <= std_logic_vector(resize(signed(v_rdata(7 downto 0)), 32));
                 when F3_LH  => alu_res <= std_logic_vector(resize(signed(v_rdata(15 downto 0)), 32));
                 when F3_LW  => alu_res <= sbi_tgt_i.rdata;
                 when F3_LBU => alu_res <= std_logic_vector(resize(unsigned(v_rdata(7 downto 0)), 32));
                 when F3_LHU => alu_res <= std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
                 when others => alu_res <= sbi_tgt_i.rdata;
               end case;
             end if;
             if mem_we = '1' then state <= S_FETCH_REQ; pc <= next_pc; else state <= S_WRITEBACK; end if;
          end if;

        -- 5. Writeback
        when S_WRITEBACK =>
          if unsigned(rd) /= 0 then regs(to_integer(unsigned(rd))) <= alu_res; end if;
          pc <= next_pc;
          state <= S_FETCH_REQ;

      end case;
    end if;
  end process;

end architecture behavioural;