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
use     asylum.WardRV_decode_pkg.all;
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
  signal state_r             : state_t;

  -- CPU State
  signal pc_r                : std_logic_vector(31 downto 0);
  signal next_pc_r           : std_logic_vector(31 downto 0);

  -- Current Instruction
  signal inst_r              : std_logic_vector(31 downto 0);

  -- Internal
  signal mem_valid_r         : std_logic;
  signal mem_addr_r          : std_logic_vector(31 downto 0);
  signal mem_wdata_r         : std_logic_vector(31 downto 0);
  signal mem_we_r            : std_logic;
  signal mem_be_r            : std_logic_vector(3 downto 0);

  -- ALU Status (for Branch Decision)
  signal alu_res_r           : std_logic_vector(31 downto 0);
  signal alu_zero_r          : std_logic;
  signal alu_sign_r          : std_logic;
  signal alu_carry_r         : std_logic;

  -- ALU Interconnect
  signal alu_src_a           : std_logic_vector(31 downto 0);
  signal alu_src_b           : std_logic_vector(31 downto 0);
  signal alu_op              : alu_op_t;
  signal alu_res             : std_logic_vector(31 downto 0);
  signal alu_carry           : std_logic;
  signal alu_zero            : std_logic;
  signal alu_sign            : std_logic;

  -- Report structure for logging
  signal pending_report_r    : inst_t;

  -- Signals from Decoder
  signal dec_imm_i           : std_logic_vector(31 downto 0);
  signal dec_imm_s           : std_logic_vector(31 downto 0);
  signal dec_imm_b           : std_logic_vector(31 downto 0);
  signal dec_imm_u           : std_logic_vector(31 downto 0);
  signal dec_imm_j           : std_logic_vector(31 downto 0);
  signal dec_rd_addr         : std_logic_vector(4 downto 0);
  signal dec_rs1_addr        : std_logic_vector(4 downto 0);
  signal dec_rs2_addr        : std_logic_vector(4 downto 0);
  signal dec_rd_we           : std_logic;
  signal dec_rs1_re          : std_logic;
  signal dec_rs2_re          : std_logic;
  signal dec_alu_op          : alu_op_t;
  signal dec_alu_src_a_sel   : std_logic;
  signal dec_alu_src_b_sel   : std_logic_vector(2 downto 0);
  signal dec_mem_req         : std_logic;
  signal dec_mem_we          : std_logic;
  signal dec_is_branch       : std_logic;
  signal dec_is_jal          : std_logic;
  signal dec_is_jalr         : std_logic;
  signal dec_funct3          : bit_vector(2 downto 0);
  signal dec_inst_type       : inst_type_t;
  
  signal src_a_val           : std_logic_vector(31 downto 0);
  signal src_b_val           : std_logic_vector(31 downto 0);
  signal load_data_formatted : std_logic_vector(31 downto 0);
  signal regfile_we          : std_logic;
 
begin
  -- Decoder Instance
  u_decode : entity work.WardRV_fsm_decode
  port map (
    inst_i            => inst_r,
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

  -- Register File Instance
  regfile_we <= '1' when state_r = S_WRITEBACK and dec_rd_we = '1' and unsigned(dec_rd_addr) /= 0 else '0';

  u_regfile : entity work.WardRV_fsm_regfile
  port map (
    clk_i       => clk_i,
    arst_b_i    => arst_b_i,
    rs1_addr_i  => dec_rs1_addr,
    rs1_re_i    => dec_rs1_re,
    rs1_rdata_o => src_a_val,
    rs2_addr_i  => dec_rs2_addr,
    rs2_re_i    => dec_rs2_re,
    rs2_rdata_o => src_b_val,
    rd_addr_i   => dec_rd_addr,
    rd_wdata_i  => alu_res_r,
    rd_we_i     => regfile_we
  );

  -- ALU Instance
  u_alu : entity work.WardRV_fsm_alu
  port map (
    src_a_i => alu_src_a,
    src_b_i => alu_src_b,
    op_i    => alu_op,
    res_o   => alu_res,
    carry_o => alu_carry,
    zero_o  => alu_zero,
    sign_o  => alu_sign
  );

  -- Load Data Formatting (Combinatorial)
  process(all)
    variable v_shamt : integer;
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    v_shamt := to_integer(unsigned(mem_addr_r(1 downto 0))) * 8;
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

    case state_r is
      when S_FETCH_REQ =>
        alu_src_a <= pc_r;
        alu_src_b <= x"00000004";

      when S_DECODE =>
        -- Source A selection
        if dec_alu_src_a_sel = ALU_SRC_A_PC then alu_src_a <= pc_r;
        else                                     alu_src_a <= src_a_val;
        end if;

        -- Source B selection
        case dec_alu_src_b_sel is
          when ALU_SRC_B_RS2   => alu_src_b <= src_b_val;
          when ALU_SRC_B_IMM_I => alu_src_b <= dec_imm_i;
          when ALU_SRC_B_IMM_S => alu_src_b <= dec_imm_s;
          when ALU_SRC_B_IMM_U => alu_src_b <= dec_imm_u;
          when ALU_SRC_B_IMM_J => alu_src_b <= dec_imm_j;
          when others => alu_src_b <= (others => '0');
        end case;

        alu_op <= dec_alu_op;

      when S_BRANCH_DECISION =>
        alu_src_a <= pc_r;
        alu_src_b <= dec_imm_b;
      
      when others => null;
    end case;
  end process;

  process(clk_i, arst_b_i)
    variable v_be    : std_logic_vector(3 downto 0);
    variable v_report : inst_t;
  begin
    if arst_b_i = '0' then
      state_r          <= S_FETCH_REQ;
      pc_r             <= RESET_ADDR;
      inst_ini_o.valid <= '0';
      mem_valid_r      <= '0';
      mem_addr_r       <= (others => '0');
      mem_wdata_r      <= (others => '0');
      mem_we_r         <= '0';
      mem_be_r         <= "0000";
      inst_r           <= (others => '0');
      next_pc_r        <= (others => '0');
      alu_zero_r       <= '0';
      alu_carry_r      <= '0';
      alu_sign_r       <= '0';
      -- synthesis translate_off
      pending_report_r <= INST_UNKNOWN;
      -- synthesis translate_on
    elsif rising_edge(clk_i) then
      
      -- Default Bus Outputs
      inst_ini_o.valid <= '0';
      mem_valid_r      <= '0';


      case state_r is
        -- 1. Fetch Request
        when S_FETCH_REQ =>
          inst_ini_o.valid <= '1';
          inst_ini_o.addr  <= pc_r;
          next_pc_r        <= alu_res; -- PC + 4 from ALU
          state_r          <= S_FETCH_WAIT;

        -- 2. Fetch Wait
        when S_FETCH_WAIT =>
          inst_ini_o.valid <= '1';
          inst_ini_o.addr  <= pc_r;
          if inst_tgt_i.ready = '1' then
            inst_r <= inst_tgt_i.inst;
            -- synthesis translate_off
            pending_report_r.pc   <= to_bitvector(pc_r);
            pending_report_r.inst <= to_bitvector(inst_tgt_i.inst);
            -- synthesis translate_on
            state_r <= S_DECODE;
            inst_ini_o.valid <= '0';
          end if;

        -- 3. Decode & Execute (Behavioral)
        when S_DECODE =>
          -- synthesis translate_off
          pending_report_r.inst_type <= dec_inst_type;
          pending_report_r.rd    <= to_integer(unsigned(dec_rd_addr));
          pending_report_r.rs1   <= to_integer(unsigned(dec_rs1_addr));
          pending_report_r.rs2   <= to_integer(unsigned(dec_rs2_addr));
          pending_report_r.imm_i <= to_bitvector(dec_imm_i);
          pending_report_r.imm_s <= to_bitvector(dec_imm_s);
          pending_report_r.imm_b <= to_bitvector(dec_imm_b);
          pending_report_r.imm_u <= to_bitvector(dec_imm_u);
          pending_report_r.imm_j <= to_bitvector(dec_imm_j);
          pending_report_r.op1   <= to_bitvector(src_a_val);
          pending_report_r.op2   <= to_bitvector(src_b_val);
          -- synthesis translate_on

          mem_we_r  <= dec_mem_we;
          alu_res_r <= alu_res;

          if dec_is_jal = '1' or dec_is_jalr = '1' then
            alu_res_r <= next_pc_r; -- Link address
            next_pc_r <= alu_res;                         -- Target address
            state_r   <= S_BRANCH_DECISION;

          elsif dec_is_branch = '1' then
            state_r <= S_BRANCH_DECISION;

          elsif dec_mem_req = '1' then
            mem_addr_r <= alu_res;
            -- synthesis translate_off
            pending_report_r.mem_addr <= to_bitvector(alu_res);
            -- synthesis translate_on
            if dec_mem_we = '1' then
              mem_wdata_r <= std_logic_vector(shift_left(unsigned(src_b_val), to_integer(unsigned(alu_res(1 downto 0))) * 8));
              -- synthesis translate_off
              case dec_funct3 is
                when F3_SB  => v_be := std_logic_vector(shift_left(unsigned'("0001"), to_integer(unsigned(alu_res(1 downto 0)))));
                when F3_SH  => v_be := std_logic_vector(shift_left(unsigned'("0011"), to_integer(unsigned(alu_res(1 downto 0)))));
                when F3_SW  => v_be := "1111";
                when others => v_be := "0000";
              end case;
              pending_report_r.mem_be <= to_bitvector(v_be);
              -- synthesis translate_on
              mem_be_r <= v_be;
            end if;
            state_r <= S_MEM_REQ;

          else
             state_r   <= S_WRITEBACK;
          end if;

          -- Update Flags from ALU (Combinatorial inputs valid for this state)
          alu_zero_r  <= alu_zero;
          alu_sign_r  <= alu_sign;
          alu_carry_r <= alu_carry;
          
        -- 3.b Branch Decision
        when S_BRANCH_DECISION =>
           if dec_is_branch = '1' then
              if (dec_funct3 = F3_BEQ  and alu_zero_r = '1') or
                 (dec_funct3 = F3_BNE  and alu_zero_r = '0') or
                 (dec_funct3 = F3_BLT  and alu_sign_r = '1') or
                 (dec_funct3 = F3_BGE  and alu_sign_r = '0') or
                 (dec_funct3 = F3_BLTU and alu_carry_r = '1') or
                 (dec_funct3 = F3_BGEU and alu_carry_r = '0') then
                next_pc_r <= alu_res;
              end if;
           end if;
           state_r <= S_WRITEBACK;

        -- 4. Memory Access
        when S_MEM_REQ | S_MEM_WAIT =>
          mem_valid_r <= '1';
          if sbi_tgt_i.ready = '1' then
             if mem_we_r = '0' then
               alu_res_r <= load_data_formatted;
               -- synthesis translate_off
               pending_report_r.mem_rdata <= to_bitvector(load_data_formatted);
               -- synthesis translate_on
             end if;
             state_r <= S_WRITEBACK;
          else state_r <= S_MEM_WAIT;
          end if;

        -- 5. Writeback
        when S_WRITEBACK =>
          -- synthesis translate_off
          -- Use variables to capture current signal states for accurate logging
          v_report           := pending_report_r;
          v_report.res       := to_bitvector(alu_res_r);
          v_report.npc       := to_bitvector(next_pc_r);
          
          if VERBOSE then
            print_inst(v_report, "exec_fsm.log");
          end if;
          -- synthesis translate_on
          
          pc_r    <= next_pc_r(31 downto 2) & "00"; -- Ensure PC stays word-aligned
          state_r <= S_FETCH_REQ;

      end case;
    end if;
  end process;

  -- Bus Output Assignments
  sbi_ini_o.valid <= mem_valid_r;
  sbi_ini_o.addr  <= mem_addr_r;
  sbi_ini_o.wdata <= mem_wdata_r;
  sbi_ini_o.we    <= mem_we_r;
  sbi_ini_o.be    <= mem_be_r;


end architecture behavioural;