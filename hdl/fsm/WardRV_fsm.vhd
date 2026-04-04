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
use     asylum.WardRV_stats_pkg.all;
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

  -- Report structure for logging
  signal pending_report : inst_t;
 
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
    variable v_report : inst_t;
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
      pending_report   <= INST_UNKNOWN;
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
            pending_report.pc   <= to_bitvector(pc);
            pending_report.inst <= to_bitvector(inst_tgt_i.inst);
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
          
          -- Common report fields
          pending_report.rd    <= to_integer(unsigned(rd));
          pending_report.rs1   <= to_integer(unsigned(rs1));
          pending_report.rs2   <= to_integer(unsigned(rs2));
          pending_report.imm_i <= to_bitvector(v_imm_i);
          pending_report.imm_s <= to_bitvector(v_imm_s);
          pending_report.imm_b <= to_bitvector(v_imm_b);
          pending_report.imm_u <= to_bitvector(v_imm_u);
          pending_report.imm_j <= to_bitvector(v_imm_j);
          pending_report.op1   <= to_bitvector(std_logic_vector(v_op1));
          pending_report.op2   <= to_bitvector(std_logic_vector(v_op2));

          mem_we <= '0';

          case opcode is
            when OPC_LUI => -- LUI
              pending_report.inst_type <= I_LUI;
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_AUIPC => -- AUIPC
              pending_report.inst_type <= I_AUIPC;
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_JAL => -- JAL
              pending_report.inst_type <= I_JAL;
              alu_res <= std_logic_vector(unsigned(pc) + 4); -- Link address
              next_pc <= w_alu_res; -- Target (PC+ImmJ)
              state <= S_BRANCH_DECISION;

            when OPC_JALR => -- JALR
              pending_report.inst_type <= I_JALR;
              alu_res <= std_logic_vector(unsigned(pc) + 4); -- Link address
              next_pc <= w_alu_res; -- Target (Rs1+ImmI)
              state <= S_BRANCH_DECISION;


            when OPC_BRANCH => -- BRANCH
              case funct3 is
                when F3_BEQ  => pending_report.inst_type <= I_BEQ;
                when F3_BNE  => pending_report.inst_type <= I_BNE;
                when F3_BLT  => pending_report.inst_type <= I_BLT;
                when F3_BGE  => pending_report.inst_type <= I_BGE;
                when F3_BLTU => pending_report.inst_type <= I_BLTU;
                when F3_BGEU => pending_report.inst_type <= I_BGEU;
                when others => null;
              end case;
              state <= S_BRANCH_DECISION;

            when OPC_LOAD => -- LOAD
              case funct3 is
                when F3_LB  => pending_report.inst_type <= I_LB;
                when F3_LH  => pending_report.inst_type <= I_LH;
                when F3_LW  => pending_report.inst_type <= I_LW;
                when F3_LBU => pending_report.inst_type <= I_LBU;
                when F3_LHU => pending_report.inst_type <= I_LHU;
                when others => null;
              end case;
              mem_addr <= w_alu_res;
              pending_report.mem_addr <= to_bitvector(w_alu_res);
              state <= S_MEM_REQ;

            when OPC_STORE => -- STORE
              v_addr := w_alu_res; -- From ALU (Calc in comb process)
              mem_addr <= v_addr;
              pending_report.mem_addr <= to_bitvector(v_addr);
              v_shamt  := to_integer(unsigned(v_addr(1 downto 0))) * 8;
              mem_wdata <= std_logic_vector(shift_left(unsigned(v_op2), v_shamt));
              mem_we <= '1';
              case funct3 is
                when F3_SB  => 
                  pending_report.inst_type <= I_SB;
                  mem_be <= std_logic_vector(shift_left(unsigned'("0001"), to_integer(unsigned(v_addr(1 downto 0)))));
                when F3_SH  => 
                  pending_report.inst_type <= I_SH;
                  mem_be <= std_logic_vector(shift_left(unsigned'("0011"), to_integer(unsigned(v_addr(1 downto 0)))));
                when F3_SW  => 
                  pending_report.inst_type <= I_SW;
                  mem_be <= "1111";
                when others => 
                  mem_be <= "0000";
              end case;
              pending_report.mem_be <= to_bitvector(mem_be);
              state <= S_MEM_REQ;

            when OPC_OP_IMM => -- OP-IMM
              case funct3 is
                when F3_ADD  => pending_report.inst_type <= I_ADDI;
                when F3_SLT  => pending_report.inst_type <= I_SLTI;
                when F3_SLTU => pending_report.inst_type <= I_SLTIU;
                when F3_XOR  => pending_report.inst_type <= I_XORI;
                when F3_OR   => pending_report.inst_type <= I_ORI;
                when F3_AND  => pending_report.inst_type <= I_ANDI;
                when F3_SLL  => pending_report.inst_type <= I_SLLI;
                when F3_SRL  => 
                  if v_imm_i(30) = '1' then pending_report.inst_type <= I_SRAI;
                  else                     pending_report.inst_type <= I_SRLI; end if;
                when others => null;
              end case;
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_OP => -- OP
              case funct3 is
                when F3_ADD  => 
                  if funct7(5) = '1' then pending_report.inst_type <= I_SUB;
                  else                     pending_report.inst_type <= I_ADD; end if;
                when F3_SLL  => pending_report.inst_type <= I_SLL;
                when F3_SLT  => pending_report.inst_type <= I_SLT;
                when F3_SLTU => pending_report.inst_type <= I_SLTU;
                when F3_XOR  => pending_report.inst_type <= I_XOR;
                when F3_SRL  => 
                  if funct7(5) = '1' then pending_report.inst_type <= I_SRA;
                  else                     pending_report.inst_type <= I_SRL; end if;
                when F3_OR   => pending_report.inst_type <= I_OR;
                when F3_AND  => pending_report.inst_type <= I_AND;
                when others  => null;
              end case;
              alu_res <= w_alu_res;
              state <= S_WRITEBACK;

            when OPC_MISC_MEM => -- FENCE / FENCE.I
              pending_report.inst_type <= I_UNKNOWN;
              state <= S_WRITEBACK; next_pc <= std_logic_vector(unsigned(pc) + 4);

            when OPC_SYSTEM => -- SYSTEM
              case funct3 is
                when F3_PRIV => -- ECALL / EBREAK
                  pending_report.inst_type <= I_UNKNOWN;
                  state <= S_WRITEBACK; next_pc <= std_logic_vector(unsigned(pc) + 4);
                when others => -- CSR Instructions (CSRRW, CSRRS, etc.)
                  -- Simplified: Read 0, Write Ignored, just for compliance
                  pending_report.inst_type <= I_UNKNOWN;
                  alu_res <= w_alu_res; -- (0)
                  state <= S_WRITEBACK;
              end case;

            when others => -- NOP
              pending_report.inst_type <= I_UNKNOWN;
              next_pc <= std_logic_vector(unsigned(pc) + 4);
              state <= S_WRITEBACK;
          end case;

          -- Update Flags from ALU (Combinatorial inputs valid for this state)
          alu_zero <= w_alu_zero;
          alu_sign <= w_alu_sign;
          
        -- 3.b Branch Decision
        when S_BRANCH_DECISION =>
           if  
                (opcode = OPC_JAL) or 
                (opcode = OPC_JALR)
           then
              -- Unconditional Jumps
              null; -- next_pc already set in S_DECODE
           elsif opcode = OPC_BRANCH then
           if (funct3 = F3_BEQ and alu_zero = '1') or
                (funct3 = F3_BNE and alu_zero = '0') or
                (funct3 = F3_BLT and alu_sign = '1') or
                (funct3 = F3_BGE and alu_sign = '0') or
                (funct3 = F3_BLTU and alu_sign = '1') or
                (funct3 = F3_BGEU and alu_sign = '0') 
                then
              next_pc <= w_alu_res;
           else
              next_pc <= std_logic_vector(unsigned(pc) + 4);
           end if;
           else
             next_pc <= std_logic_vector(unsigned(pc) + 4);
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
               pending_report.mem_rdata <= to_bitvector(alu_res);
               next_pc <= std_logic_vector(unsigned(pc) + 4);
             end if;
             state <= S_WRITEBACK;
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
               pending_report.mem_rdata <= to_bitvector(alu_res);
               next_pc <= std_logic_vector(unsigned(pc) + 4);
             end if;
             state <= S_WRITEBACK;
          end if;

        -- 5. Writeback
        when S_WRITEBACK =>
          -- Use variables to capture current signal states for accurate logging
          v_report           := pending_report;
          v_report.res       := to_bitvector(alu_res);
          v_report.npc       := to_bitvector(next_pc);
          
          -- synthesis translate_off
          if VERBOSE then
            print_inst(v_report, "exec_fsm.log");
          end if;
          -- synthesis translate_on
          
          -- Only update register file for instructions that have a destination register
          case opcode is
            when OPC_LUI | OPC_AUIPC | OPC_JAL | OPC_JALR | OPC_LOAD | OPC_OP_IMM | OPC_OP | OPC_SYSTEM =>
              if unsigned(rd) /= 0 then 
                regs(to_integer(unsigned(rd))) <= alu_res; 
              end if;
            when others => null;
          end case;

          pc <= next_pc;
          state <= S_FETCH_REQ;

      end case;
    end if;
  end process;

end architecture behavioural;