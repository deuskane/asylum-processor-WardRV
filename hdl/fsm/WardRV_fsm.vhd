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
-- 2026-04-06  1.1      mrosiere Move decode into specific module
--                               Add instruction type to report
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
  signal state     : state_t;

  -- CPU State
  signal pc        : std_logic_vector(31 downto 0);
  signal next_pc   : std_logic_vector(31 downto 0);
  
  -- Register File (x0 is hardwired to 0 in logic)
  type regfile_t is array (0 to 31) of std_logic_vector(31 downto 0);
  signal regs      : regfile_t;

  -- Current Instruction
  signal inst      : std_logic_vector(31 downto 0);
  signal inst_bv   : bit_vector(31 downto 0);


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
  signal alu_carry : std_logic;

  -- ALU Interconnect
  signal alu_src_a  : std_logic_vector(31 downto 0);
  signal alu_src_b  : std_logic_vector(31 downto 0);
  signal alu_op     : alu_op_t;
  signal w_alu_res  : std_logic_vector(31 downto 0);
  signal w_alu_carry: std_logic;
  signal w_alu_zero : std_logic;
  signal w_alu_sign : std_logic;

  -- Report structure for logging
  signal pending_report : inst_t;

  -- Signals from Decoder
  signal dec_imm_i, dec_imm_s, dec_imm_b, dec_imm_u, dec_imm_j : std_logic_vector(31 downto 0);
  signal dec_rd_addr, dec_rs1_addr, dec_rs2_addr             : std_logic_vector(4 downto 0);
  signal dec_rd_we, dec_rs1_re, dec_rs2_re                   : std_logic;
  signal dec_alu_op                                          : alu_op_t;
  signal dec_alu_src_a_sel                                   : std_logic;
  signal dec_alu_src_b_sel                                   : std_logic_vector(2 downto 0);
  signal dec_mem_req, dec_mem_we                              : std_logic;
  signal dec_is_branch, dec_is_jal, dec_is_jalr               : std_logic;
  signal dec_funct3                                          : bit_vector(2 downto 0);
  signal dec_inst_type                                       : inst_type_t;

  signal src_a_val, src_b_val             : std_logic_vector(31 downto 0);
  signal load_data_formatted               : std_logic_vector(31 downto 0);
 
begin

  -- Decoder Instance
  u_decode : entity work.WardRV_fsm_decode
  port map (
    inst_i            => inst,
    imm_i_o           => dec_imm_i,
    imm_s_o           => dec_imm_s,
    imm_b_o           => dec_imm_b,
    imm_u_o           => dec_imm_u,
    imm_j_o           => dec_imm_j,
    rd_addr_o         => dec_rd_addr,
    rs1_addr_o        => dec_rs1_addr,
    rs2_addr_o        => dec_rs2_addr,
    rd_we_o           => dec_rd_we,
    rs1_re_o          => dec_rs1_re,
    rs2_re_o          => dec_rs2_re,
    alu_op_o          => dec_alu_op,
    alu_src_a_sel_o   => dec_alu_src_a_sel,
    alu_src_b_sel_o   => dec_alu_src_b_sel,
    mem_req_o         => dec_mem_req,
    mem_we_o          => dec_mem_we,
    is_branch_o       => dec_is_branch,
    is_jal_o          => dec_is_jal,
    is_jalr_o         => dec_is_jalr,
    funct3_o          => dec_funct3,
    inst_type_o       => dec_inst_type
  );

  -- Register File Read (Controlled)
  src_a_val <= regs(to_integer(unsigned(dec_rs1_addr))) when dec_rs1_re = '1' else (others => '0');
  src_b_val <= regs(to_integer(unsigned(dec_rs2_addr))) when dec_rs2_re = '1' else (others => '0');

  -- ALU Instance
  u_alu : entity work.WardRV_fsm_alu
  port map (
    src_a_i => alu_src_a,
    src_b_i => alu_src_b,
    op_i    => alu_op,
    res_o   => w_alu_res,
    carry_o => w_alu_carry,
    zero_o  => w_alu_zero,
    sign_o  => w_alu_sign
  );

  -- Load Data Formatting (Combinatorial)
  process(all)
    variable v_shamt : integer;
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    v_shamt := to_integer(unsigned(mem_addr(1 downto 0))) * 8;
    v_rdata := std_logic_vector(shift_right(unsigned(sbi_tgt_i.rdata), v_shamt));
    case dec_funct3 is
      when F3_LB  => load_data_formatted <= std_logic_vector(resize(signed(v_rdata(7 downto 0)), 32));
      when F3_LH  => load_data_formatted <= std_logic_vector(resize(signed(v_rdata(15 downto 0)), 32));
      when F3_LBU => load_data_formatted <= std_logic_vector(resize(unsigned(v_rdata(7 downto 0)), 32));
      when F3_LHU => load_data_formatted <= std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
      when others => load_data_formatted <= sbi_tgt_i.rdata;
    end case;
  end process;

  -- ALU Control Process (Combinatorial)
  process(all)
  begin
    alu_src_a <= (others => '0');
    alu_src_b <= (others => '0');
    alu_op    <= ALU_ADD;

    case state is
      when S_FETCH_REQ =>
        alu_src_a <= pc;
        alu_src_b <= x"00000004";

      when S_DECODE =>
        -- Source A selection
        if dec_alu_src_a_sel = '1' then alu_src_a <= pc;
        else                           alu_src_a <= src_a_val;
        end if;

        -- Source B selection
        case dec_alu_src_b_sel is
          when "000" => alu_src_b <= src_b_val;
          when "001" => alu_src_b <= dec_imm_i;
          when "010" => alu_src_b <= dec_imm_s;
          when "011" => alu_src_b <= dec_imm_u;
          when "100" => alu_src_b <= dec_imm_j;
          when others => alu_src_b <= (others => '0');
        end case;

        alu_op <= dec_alu_op;

      when S_BRANCH_DECISION =>
        alu_src_a <= pc;
        alu_src_b <= dec_imm_b;
      
      when others => null;
    end case;
  end process;

  process(clk_i, arst_b_i)
    variable v_be    : std_logic_vector(3 downto 0);
    variable v_report : inst_t;
  begin
    if arst_b_i = '0' then
      state            <= S_FETCH_REQ;
      pc               <= RESET_ADDR;
      regs             <= (others => (others => '0'));
      inst_ini_o.valid <= '0';
      sbi_ini_o.valid  <= '0';
      sbi_ini_o.addr   <= (others => '0');
      sbi_ini_o.wdata  <= (others => '0');
      sbi_ini_o.we     <= '0';
      sbi_ini_o.be     <= "0000";
      inst             <= (others => '0');
      next_pc          <= (others => '0');
      alu_zero         <= '0';
      alu_carry        <= '0';
      alu_sign         <= '0';
      -- synthesis translate_off
      pending_report   <= INST_UNKNOWN;
      -- synthesis translate_on
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
            -- synthesis translate_off
            pending_report.pc   <= to_bitvector(pc);
            pending_report.inst <= to_bitvector(inst_tgt_i.inst);
            -- synthesis translate_on
            state <= S_DECODE;
            inst_ini_o.valid <= '0';
          end if;

        -- 3. Decode & Execute (Behavioral)
        when S_DECODE =>
          -- synthesis translate_off
          pending_report.inst_type <= dec_inst_type;
          pending_report.rd    <= to_integer(unsigned(dec_rd_addr));
          pending_report.rs1   <= to_integer(unsigned(dec_rs1_addr));
          pending_report.rs2   <= to_integer(unsigned(dec_rs2_addr));
          pending_report.imm_i <= to_bitvector(dec_imm_i);
          pending_report.imm_s <= to_bitvector(dec_imm_s);
          pending_report.imm_b <= to_bitvector(dec_imm_b);
          pending_report.imm_u <= to_bitvector(dec_imm_u);
          pending_report.imm_j <= to_bitvector(dec_imm_j);
          pending_report.op1   <= to_bitvector(src_a_val);
          pending_report.op2   <= to_bitvector(src_b_val);
          -- synthesis translate_on

          mem_we  <= dec_mem_we;
          alu_res <= w_alu_res;

          if dec_is_jal = '1' or dec_is_jalr = '1' then
            alu_res <= std_logic_vector(unsigned(pc) + 4); -- Link address
            next_pc <= w_alu_res;                         -- Target address
            state   <= S_BRANCH_DECISION;

          elsif dec_is_branch = '1' then
            state <= S_BRANCH_DECISION;

          elsif dec_mem_req = '1' then
            mem_addr <= w_alu_res;
            -- synthesis translate_off
            pending_report.mem_addr <= to_bitvector(w_alu_res);
            -- synthesis translate_on
            if dec_mem_we = '1' then
              mem_wdata <= std_logic_vector(shift_left(unsigned(src_b_val), to_integer(unsigned(w_alu_res(1 downto 0))) * 8));
              -- synthesis translate_off
              case dec_funct3 is
                when F3_SB  => v_be := std_logic_vector(shift_left(unsigned'("0001"), to_integer(unsigned(w_alu_res(1 downto 0)))));
                when F3_SH  => v_be := std_logic_vector(shift_left(unsigned'("0011"), to_integer(unsigned(w_alu_res(1 downto 0)))));
                when F3_SW  => v_be := "1111";
                when others => v_be := "0000";
              end case;
              pending_report.mem_be <= to_bitvector(v_be);
              -- synthesis translate_on
              mem_be <= v_be;
            end if;
            state <= S_MEM_REQ;

          elsif dec_inst_type /= I_UNKNOWN then
             state <= S_WRITEBACK;
          else
             next_pc <= std_logic_vector(unsigned(pc) + 4);
             state   <= S_WRITEBACK;
          end if;

          -- Update Flags from ALU (Combinatorial inputs valid for this state)
          alu_zero <= w_alu_zero;
          alu_sign <= w_alu_sign;
          alu_carry <= w_alu_carry;
          
        -- 3.b Branch Decision
        when S_BRANCH_DECISION =>
           if dec_is_branch = '1' then
              if (dec_funct3 = F3_BEQ  and alu_zero = '1') or
                 (dec_funct3 = F3_BNE  and alu_zero = '0') or
                 (dec_funct3 = F3_BLT  and alu_sign = '1') or
                 (dec_funct3 = F3_BGE  and alu_sign = '0') or
                 (dec_funct3 = F3_BLTU and alu_carry = '1') or
                 (dec_funct3 = F3_BGEU and alu_carry = '0') then
                next_pc <= w_alu_res;
              else
                next_pc <= std_logic_vector(unsigned(pc) + 4);
              end if;
           end if;
           state <= S_WRITEBACK;

        -- 4. Memory Access
        when S_MEM_REQ | S_MEM_WAIT =>
          sbi_ini_o.valid <= '1';
          sbi_ini_o.addr  <= mem_addr;
          sbi_ini_o.wdata <= mem_wdata;
          sbi_ini_o.we    <= mem_we;
          sbi_ini_o.be    <= mem_be;
          if sbi_tgt_i.ready = '1' then
             if mem_we = '0' then
               alu_res <= load_data_formatted;
               -- synthesis translate_off
               pending_report.mem_rdata <= to_bitvector(load_data_formatted);
               -- synthesis translate_on
               next_pc <= std_logic_vector(unsigned(pc) + 4);
             end if;
             state <= S_WRITEBACK;
          else state <= S_MEM_WAIT;
          end if;

        -- 5. Writeback
        when S_WRITEBACK =>
          -- synthesis translate_off
          -- Use variables to capture current signal states for accurate logging
          v_report           := pending_report;
          v_report.res       := to_bitvector(alu_res);
          v_report.npc       := to_bitvector(next_pc);
          
          if VERBOSE then
            print_inst(v_report, "exec_fsm.log");
          end if;
          -- synthesis translate_on
          
          if dec_rd_we = '1' and unsigned(dec_rd_addr) /= 0 then
            regs(to_integer(unsigned(dec_rd_addr))) <= alu_res;
          end if;

          pc <= next_pc;
          state <= S_FETCH_REQ;

      end case;
    end if;
  end process;

end architecture behavioural;