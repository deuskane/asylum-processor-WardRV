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
  signal state_r                    : state_t;
  signal state_r_next               : state_t;

  -- CPU State
  signal pc_r_next                  : std_logic_vector(31 downto 0);
  signal pc_r                       : std_logic_vector(31 downto 0);
  signal pc_seq_r                   : std_logic_vector(31 downto 0); -- PC + 4

  signal branch_taken               : std_logic;
  signal branch_taken_r             : std_logic;

  -- Current Instruction
  signal inst_r                     : std_logic_vector(31 downto 0);

  signal imem_valid_r               : std_logic;
  signal imem_ready                 : std_logic;
  signal imem_addr                  : std_logic_vector(31 downto 0);
  signal imem_rdata                 : std_logic_vector(31 downto 0);

  -- Internal
  signal dmem_valid_r               : std_logic;
  signal dmem_ready                 : std_logic;
  signal dmem_addr                  : std_logic_vector(31 downto 0);
  signal dmem_wdata                 : std_logic_vector(31 downto 0);
  signal dmem_rdata                 : std_logic_vector(31 downto 0);
  signal dmem_rdata_r               : std_logic_vector(31 downto 0);
  signal dmem_we                    : std_logic;
  signal dmem_be                    : std_logic_vector(3 downto 0);

  -- ALU Status (for Branch Decision)
  signal alu_res_r                  : std_logic_vector(31 downto 0);

  -- ALU Interconnect
  signal alu_src_a                  : std_logic_vector(31 downto 0);
  signal alu_src_b                  : std_logic_vector(31 downto 0);
  signal alu_op                     : alu_op_t;
  signal alu_res                    : std_logic_vector(31 downto 0);
  signal alu_carry                  : std_logic;
  signal alu_zero                   : std_logic;
  signal alu_sign                   : std_logic;

  -- Report structure for logging
  signal pending_report_r           : inst_t;

  -- Signals from Decoder
  signal dec_imm_i                  : std_logic_vector(31 downto 0);
  signal dec_imm_s                  : std_logic_vector(31 downto 0);
  signal dec_imm_b                  : std_logic_vector(31 downto 0);
  signal dec_imm_u                  : std_logic_vector(31 downto 0);
  signal dec_imm_j                  : std_logic_vector(31 downto 0);
  signal dec_rd_addr                : std_logic_vector(4 downto 0);
  signal dec_rs1_addr               : std_logic_vector(4 downto 0);
  signal dec_rs2_addr               : std_logic_vector(4 downto 0);
  signal dec_rd_we                  : std_logic;
  signal dec_rd_src                 : rd_src_t;
  signal dec_rs1_re                 : std_logic;
  signal dec_rs2_re                 : std_logic;
  signal dec_alu_op                 : alu_op_t;
  signal dec_alu_src_a_sel          : alu_src_a_sel_t;
  signal dec_alu_src_b_sel          : alu_src_b_sel_t;
  signal dec_dmem_req               : std_logic;
  signal dec_dmem_we                : std_logic;
  signal dec_dmem_be                : std_logic_vector(3 downto 0);
  signal dec_dmem_data_unsigned     : std_logic;
  signal dec_is_branch              : std_logic;
  signal dec_branch_use_flag_zero   : std_logic;
  signal dec_branch_use_flag_carry  : std_logic;
  signal dec_branch_use_flag_sign   : std_logic;
  signal dec_branch_flag_is_set     : std_logic;
  signal dec_pc_sel                 : std_logic_vector(1 downto 0);
  signal dec_inst_type              : inst_type_t;
     
  signal alu_src_a_sel              : alu_src_a_sel_t;
  signal alu_src_b_sel              : alu_src_b_sel_t;
  signal src_a_val                  : std_logic_vector(31 downto 0);
  signal src_b_val                  : std_logic_vector(31 downto 0);
  signal regfile_rd_we              : std_logic;
  signal regfile_rd_wdata           : std_logic_vector(31 downto 0);
begin

  --------------------------------------------------------------------
  -- Program Counter Logic
  --------------------------------------------------------------------

  branch_taken <= '1' when(((alu_zero  and dec_branch_use_flag_zero  ) or
                               (alu_carry and dec_branch_use_flag_carry ) or
                               (alu_sign  and dec_branch_use_flag_sign  )) = dec_branch_flag_is_set)                
                   else '0';

  pc_r_next    <= alu_res_r(31 downto 2) & "00" when (dec_pc_sel = PC_SEL_JUMP) or
                                                     (((dec_pc_sel = PC_SEL_BRANCH) and branch_taken_r = '1')) else
                  pc_seq_r; -- Ensure PC stays word-aligned

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      pc_seq_r       <= (others => '0');
      pc_r           <= RESET_ADDR;
      branch_taken_r <= '0';
    elsif rising_edge(clk_i)
    then
      if (state_r = S_FETCH_REQ)
      then
        pc_seq_r <= alu_res; -- PC + 4 computed by ALU
      end if;

      if (state_r = S_BRANCH_DECISION)
      then
        branch_taken_r <= branch_taken;
      end if;


      if (state_r = S_WRITEBACK)
      then
        pc_r <= pc_r_next;
      end if;

    end if;
  end process;


  --------------------------------------------------------------------
  -- Fetch Request and Wait
  --------------------------------------------------------------------
  inst_ini_o.valid <= imem_valid_r;
  inst_ini_o.addr  <= imem_addr;

  imem_ready       <= inst_tgt_i.ready;
  imem_rdata       <= inst_tgt_i.inst;
  imem_addr        <= pc_r;

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      imem_valid_r     <= '0';
      inst_r           <= (others => '0');
      
    elsif rising_edge(clk_i)
    then
      imem_valid_r     <= '1' when (state_r_next = S_FETCH_REQ ) or 
                                   (state_r_next = S_FETCH_WAIT) else '0';

      if (imem_valid_r = '1') and (imem_ready = '1')
      then
        inst_r       <= imem_rdata;
      end if;
      
    end if;
  end process;
  
  --------------------------------------------------------------------
  -- Decoder Instance
  --------------------------------------------------------------------
  inst_decode : entity work.WardRV_fsm_decode
  port map (
    inst_i                   => inst_r,
    imm_i_o                  => dec_imm_i,
    imm_s_o                  => dec_imm_s,
    imm_b_o                  => dec_imm_b,
    imm_u_o                  => dec_imm_u,
    imm_j_o                  => dec_imm_j,
    rd_addr_o                => dec_rd_addr,
    rs1_addr_o               => dec_rs1_addr,
    rs2_addr_o               => dec_rs2_addr,
    rd_we_o                  => dec_rd_we,
    rd_src_o                 => dec_rd_src,
    rs1_re_o                 => dec_rs1_re,
    rs2_re_o                 => dec_rs2_re,
    alu_op_o                 => dec_alu_op,
    alu_src_a_sel_o          => dec_alu_src_a_sel,
    alu_src_b_sel_o          => dec_alu_src_b_sel,
    mem_req_o                => dec_dmem_req,
    mem_we_o                 => dec_dmem_we,
    mem_be_o                 => dec_dmem_be,
    mem_data_unsigned_o      => dec_dmem_data_unsigned,
    is_branch_o              => dec_is_branch,
    branch_use_flag_zero_o   => dec_branch_use_flag_zero,
    branch_use_flag_carry_o  => dec_branch_use_flag_carry,
    branch_use_flag_sign_o   => dec_branch_use_flag_sign,
    branch_flag_is_set_o     => dec_branch_flag_is_set,
    pc_sel_o                 => dec_pc_sel,
    inst_type_o              => dec_inst_type
  );

  --------------------------------------------------------------------
  -- Register File Instance
  --------------------------------------------------------------------

  -- Write Enable Logic: 
  -- Only write back for instructions that write registers, and never write to x0
  regfile_rd_we    <= '1' when state_r   = S_WRITEBACK and 
                               dec_rd_we = '1' else
                      '0';

  regfile_rd_wdata <= dmem_rdata_r when dec_rd_src = RD_SRC_MEM      else
                      pc_seq_r     when dec_rd_src = RD_SRC_PC_PLUS4 else
                      alu_res_r; --when dec_rd_src = RD_SRC_ALU      else
                      
  inst_regfile : entity work.WardRV_fsm_regfile
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
    rd_wdata_i  => regfile_rd_wdata,
    rd_we_i     => regfile_rd_we
  );

  --------------------------------------------------------------------
  -- ALU Control Process (Combinatorial)
  --------------------------------------------------------------------

  -- ALU is used for PC increment, address calculation, and branch target calculation
  -- The source operands and operation are determined by the current state and decoded instruction fields.
  process(all)
  begin

    case state_r is
      -- Fetch: ALU computes PC + 4 for next instruction
      when S_FETCH_REQ =>
        alu_op        <= ALU_ADD;
        alu_src_a_sel <= ALU_SRC_A_PC;
        alu_src_b_sel <= ALU_SRC_B_IMM_4;

      -- Branch Decision: ALU compute branch destination
      when S_BRANCH_DECISION =>
        alu_op        <= ALU_SUB;
        alu_src_a_sel <= ALU_SRC_A_RS1;
        alu_src_b_sel <= ALU_SRC_B_RS2;
      
      -- Decode/Execute: ALU sources and operation determined by instruction type
      when others => 
      --when S_DECODE =>
        alu_op        <= dec_alu_op;
        alu_src_a_sel <= dec_alu_src_a_sel;
        alu_src_b_sel <= dec_alu_src_b_sel;

    end case;
  end process;

  --------------------------------------------------------------------
  -- ALU Instance
  --------------------------------------------------------------------
  alu_src_a <= pc_r          when alu_src_a_sel = ALU_SRC_A_PC    else 
               src_a_val;  --when alu_src_a_sel = ALU_SRC_A_RS1

  alu_src_b <= dec_imm_i     when alu_src_b_sel = ALU_SRC_B_IMM_I else 
               dec_imm_s     when alu_src_b_sel = ALU_SRC_B_IMM_S else
               dec_imm_u     when alu_src_b_sel = ALU_SRC_B_IMM_U else
               dec_imm_j     when alu_src_b_sel = ALU_SRC_B_IMM_J else
               dec_imm_b     when alu_src_b_sel = ALU_SRC_B_IMM_B else
               x"00000004"   when alu_src_b_sel = ALU_SRC_B_IMM_4 else
               src_b_val;  --when alu_src_b_sel = ALU_SRC_B_RS2 
               
  inst_alu : entity work.WardRV_fsm_alu
  port map (
    src_a_i => alu_src_a,
    src_b_i => alu_src_b,
    op_i    => alu_op,
    res_o   => alu_res,
    carry_o => alu_carry,
    zero_o  => alu_zero,
    sign_o  => alu_sign
  );

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      alu_res_r <= (others => '0');
      elsif rising_edge(clk_i)
    then
      if (state_r = S_DECODE) 
      then
        alu_res_r <= alu_res;
      end if;
    end if;
  end process;  

  --------------------------------------------------------------------
  -- Memory
  --
  -- Data Bus Output Assignments
  --------------------------------------------------------------------
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      dmem_valid_r     <= '0';
    elsif rising_edge(clk_i)
    then
      if (state_r_next = S_MEM_REQ ) or 
         (state_r_next = S_MEM_WAIT) then
        dmem_valid_r <= '1';
      else
        dmem_valid_r <= '0';
      end if;
    end if;
  end process;

  dmem_ready      <= sbi_tgt_i.ready;
  dmem_addr       <= alu_res_r;
  dmem_be         <= std_logic_vector(shift_left(unsigned(dec_dmem_be), to_integer(unsigned(dmem_addr(1 downto 0)))));
  dmem_wdata      <= std_logic_vector(shift_left(unsigned(src_b_val)  , to_integer(unsigned(dmem_addr(1 downto 0))) * 8));
  dmem_we         <= dec_dmem_we;

  sbi_ini_o.valid <= dmem_valid_r;
  sbi_ini_o.addr  <= dmem_addr   ;
  sbi_ini_o.wdata <= dmem_wdata  ;
  sbi_ini_o.we    <= dmem_we     ;
  sbi_ini_o.be    <= dmem_be     ;

  --------------------------------------------------------------------
  -- Memory
  --
  -- Load Data Formatting (Combinatorial)
  --------------------------------------------------------------------
  process(all)
    variable v_shamt : integer;
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    -- Take the relevant byte/half-word from the 32-bit read data
    -- Use shamt to shift the relevant data to the LSBs.
    v_shamt := to_integer(unsigned(dmem_addr(1 downto 0))) * 8;
    v_rdata := std_logic_vector(shift_right(unsigned(sbi_tgt_i.rdata), v_shamt));

    -- Apply sign or zero extension based on instruction type
    case dec_dmem_be is
      when "0001"  => dmem_rdata <= std_logic_vector(resize(  signed(v_rdata( 7 downto 0)), 32)) when dec_dmem_data_unsigned = '0' else 
                                    std_logic_vector(resize(unsigned(v_rdata( 7 downto 0)), 32));
      when "0011"  => dmem_rdata <= std_logic_vector(resize(  signed(v_rdata(15 downto 0)), 32)) when dec_dmem_data_unsigned = '0' else 
                                    std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
      when others  => dmem_rdata <= v_rdata; -- Word access, no formatting needed
    end case;
  end process;

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' 
    then
        dmem_rdata_r <= (others => '0');
    elsif rising_edge(clk_i) 
    then
        if dmem_valid_r = '1' and dmem_ready = '1' 
        then
            dmem_rdata_r <= dmem_rdata;
        end if;
    end if; 
  end process;

  --------------------------------------------------------------------
  -- FSM
  --------------------------------------------------------------------
  process(all)
  begin
    state_r_next <= state_r;

    case state_r is
      when S_FETCH_REQ =>
        state_r_next <= S_FETCH_WAIT;

      when S_FETCH_WAIT =>
        if imem_ready = '1' then
          state_r_next <= S_DECODE;
        end if;

      when S_DECODE =>
          if dec_is_branch = '1' then
              state_r_next <= S_BRANCH_DECISION;
          elsif dec_dmem_req = '1' then
              state_r_next <= S_MEM_REQ;
          else
              state_r_next <= S_WRITEBACK;
          end if;

      when S_BRANCH_DECISION =>

        state_r_next <= S_WRITEBACK;
      when S_MEM_REQ =>
        state_r_next <= S_MEM_WAIT;
      when S_MEM_WAIT =>
        if dmem_ready = '1' then
          state_r_next <= S_WRITEBACK;
        end if;
      when S_WRITEBACK =>
        state_r_next <= S_FETCH_REQ;
      when others =>
    end case;
  end process;

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      state_r          <= S_FETCH_REQ;

      elsif rising_edge(clk_i) then

        state_r <= state_r_next;

    end if;

  end process;

  -- synthesis translate_off

  --------------------------------------------------------------------
  -- 
  --------------------------------------------------------------------

  process(clk_i, arst_b_i)
    variable v_report : inst_t;
  begin
    if arst_b_i = '0' then
      pending_report_r <= INST_UNKNOWN;

      elsif rising_edge(clk_i) then
        case state_r is

          when S_FETCH_WAIT =>
            pending_report_r.pc   <= to_bitvector(imem_addr);
            pending_report_r.inst <= to_bitvector(imem_rdata);

          when S_DECODE =>
            pending_report_r.inst_type <= dec_inst_type;
            pending_report_r.rd        <= to_integer(unsigned(dec_rd_addr));
            pending_report_r.rs1       <= to_integer(unsigned(dec_rs1_addr));
            pending_report_r.rs2       <= to_integer(unsigned(dec_rs2_addr));
            pending_report_r.imm_i     <= to_bitvector(dec_imm_i);
            pending_report_r.imm_s     <= to_bitvector(dec_imm_s);
            pending_report_r.imm_b     <= to_bitvector(dec_imm_b);
            pending_report_r.imm_u     <= to_bitvector(dec_imm_u);
            pending_report_r.imm_j     <= to_bitvector(dec_imm_j);
            pending_report_r.op1       <= to_bitvector(src_a_val);
            pending_report_r.op2       <= to_bitvector(src_b_val);

        when S_MEM_WAIT =>
            pending_report_r.mem_addr <= to_bitvector(dmem_addr);
            pending_report_r.mem_be   <= to_bitvector(dmem_be  );             
            pending_report_r.mem_rdata <= to_bitvector(dmem_rdata);

        when S_WRITEBACK =>

            v_report           := pending_report_r;
            v_report.res       := to_bitvector(regfile_rd_wdata);
            v_report.npc       := to_bitvector(pc_r_next);
          
            if VERBOSE 
            then
              print_inst(v_report, "exec_fsm.log");
            end if;
        when others => null;
      end case;

    end if;

  end process;
      -- synthesis translate_off

end architecture behavioural;