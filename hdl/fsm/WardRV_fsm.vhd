-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_fsm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-- This module implements the main control logic for the WardRV processor.
-- It uses a multi-cycle Finite State Machine (FSM) approach to minimize area 
-- by sharing a single ALU across different execution phases (Fetch, Exec, Mem).
-- The FSM orchestrates the data flow, ensuring that each instruction progresses
-- through the necessary stages (Fetch, Decode, Execute, Memory, Writeback)
-- using shared hardware resources efficiently.
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
  --VENDORID   : std_logic_vector(31 downto 0) := (others => '0');
    HARTID     : std_logic_vector(31 downto 0) := (others => '0');
    RESET_ADDR : std_logic_vector(31 downto 0) := (others => '0');
    VERBOSE    : boolean                       := true
  );
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    -- Instruction Interface
    imem_ini_o : out imem_ini_t;
    imem_tgt_i : in  imem_tgt_t;

    -- Data Interface
    dmem_ini_o  : out dmem_ini_t;
    dmem_tgt_i  : in  dmem_tgt_t;

    -- Interruption Interface
    meip_i     : in  std_logic
  );
end entity WardRV_fsm;

architecture behavioural of WardRV_fsm is
  constant C_IMM_4 : std_logic_vector(31 downto 0) := (2 => '1', others => '0');

  -- FSM states representing the standard RISC-V instruction cycle phases.
  -- This multi-cycle design explicitly separates these phases into distinct states.
  type state_t is (S_FETCH, 
                   S_DECODE, 
                   S_EXECUTE,
                   S_BRANCH_DECISION, 
                   S_MEMORY, 
                   S_WRITEBACK);
                   
  signal state_r                    : state_t;
  signal state_r_next               : state_t;

  -- CPU State
  signal pc_r_next                  : std_logic_vector(31 downto 0);
  signal pc_r_we                    : std_logic;
  signal pc_r                       : std_logic_vector(31 downto 0);
  signal pc_seq_r_we                : std_logic;
  signal pc_seq_r                   : std_logic_vector(31 downto 0); -- PC + 4

  signal branch_taken               : std_logic;
  signal branch_taken_r_we          : std_logic;
  signal branch_taken_r             : std_logic;

  -- Current Instruction
  signal inst_r                     : std_logic_vector(31 downto 0);

  signal imem_valid                 : std_logic;
  signal imem_ready                 : std_logic;
  signal imem_addr                  : std_logic_vector(31 downto 0);
  signal imem_rdata                 : std_logic_vector(31 downto 0);

  -- Internal
  signal dmem_valid                 : std_logic;
  signal dmem_ready                 : std_logic;
  signal dmem_be                    : std_logic_vector(3 downto 0);
  signal dmem_addr                  : std_logic_vector(31 downto 0);
  signal dmem_rdata_r               : std_logic_vector(31 downto 0);
  signal dmem_ini                    : dmem_ini_t;

  -- Intermediate register for ALU result to be used across FSM cycles.
  signal alu_res_r_we               : std_logic;
  signal alu_res_r                  : std_logic_vector(31 downto 0);

  -- ALU operation and flag signals. These control the ALU's function and capture its status.
  signal alu_src_a                  : std_logic_vector(31 downto 0);
  signal alu_src_b                  : std_logic_vector(31 downto 0);
  signal alu_op                     : alu_op_t;
  signal alu_res                    : std_logic_vector(31 downto 0);
  signal alu_carry                  : std_logic;
  signal alu_zero                   : std_logic;
  signal alu_sign                   : std_logic;

  -- Report structure for logging
  signal pending_report_r           : inst_t;

  -- Control signals decoded from the current instruction
  signal dec_imm_i                  : std_logic_vector(31 downto 0);
  signal dec_imm_s                  : std_logic_vector(31 downto 0);
  signal dec_imm_b                  : std_logic_vector(31 downto 0);
  signal dec_imm_u                  : std_logic_vector(31 downto 0);
  signal dec_imm_j                  : std_logic_vector(31 downto 0);
  signal dec_imm_csr                : std_logic_vector(31 downto 0);
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
  signal dec_csr_we                 : std_logic;
  signal dec_csr_re                 : std_logic;
  signal dec_csr_addr               : std_logic_vector(11 downto 0);
  signal dec_inst_type              : inst_type_t;
  signal dec_inst_is_mret           : std_logic;
     
  -- CSR Signals
  signal csr_we                     : std_logic; -- Write enable for CSRs

  signal csr_rdata                  : std_logic_vector(31 downto 0);
  signal csr_mtvec                  : std_logic_vector(31 downto 0);  

  -- Trap Handling (Stubs for now)
  signal trap                       : std_logic;
  signal trap_mirq                  : std_logic;
  signal trap_cause                 : std_logic_vector(31 downto 0);
  signal trap_pc                    : std_logic_vector(31 downto 0);
  signal trap_mtval                 : std_logic_vector(31 downto 0);

  -- Mux selection signals for ALU inputs, determined by the FSM state or decoder.
  signal alu_src_a_sel              : alu_src_a_sel_t;
  signal alu_src_b_sel              : alu_src_b_sel_t;
  signal src_a_val                  : std_logic_vector(31 downto 0);
  signal src_b_val                  : std_logic_vector(31 downto 0);
  signal regfile_we                 : std_logic; -- Global RF write enable from FSM
  signal regfile_rd_we              : std_logic; -- Combined RF write enable
  signal regfile_rd_wdata           : std_logic_vector(31 downto 0); -- Data to write back

begin

  --------------------------------------------------------------------
  -- Instruction Fetch Module
  -- Encapsulates the complexity of the instruction bus handshake.
  --------------------------------------------------------------------
  inst_fetch : entity work.WardRV_fsm_fetch
  port map (
    clk_i        => clk_i,
    arst_b_i     => arst_b_i,
    
    -- Control/Status
    imem_valid_i => imem_valid,
    pc_i         => pc_r,
    imem_ready_o => imem_ready,
    inst_r_o     => inst_r,

    -- Physical Interface
    imem_ini_o   => imem_ini_o,
    imem_tgt_i   => imem_tgt_i
  );

  imem_addr <= pc_r;
  
  --------------------------------------------------------------------
  -- Decoder Instance
  -- This is a purely combinatorial block that interprets the fetched instruction
  -- and generates all necessary control signals and immediate values for the datapath.
  --------------------------------------------------------------------
  inst_decode : entity work.WardRV_fsm_decode
  port map (
    inst_i                   => inst_r,
    imm_i_o                  => dec_imm_i,
    imm_s_o                  => dec_imm_s,
    imm_b_o                  => dec_imm_b,
    imm_u_o                  => dec_imm_u,
    imm_j_o                  => dec_imm_j,
    imm_csr_o                => dec_imm_csr,
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
    csr_we_o                 => dec_csr_we,
    csr_re_o                 => dec_csr_re,
    csr_addr_o               => dec_csr_addr,
    inst_is_mret_o           => dec_inst_is_mret,
    inst_type_o              => dec_inst_type
  );

  --------------------------------------------------------------------
  -- CSR Instance
  --------------------------------------------------------------------
  csr_we                <= dec_csr_we and regfile_we;

  csr_inst : entity work.WardRV_fsm_csr
  generic map (
    HARTID              => HARTID
  )
  port map (
    clk_i               => clk_i,
    arst_b_i            => arst_b_i,
    csr_addr_i          => dec_csr_addr,
    csr_we_i            => csr_we,
    csr_re_i            => dec_csr_re,
    csr_wdata_i         => alu_res_r,
    csr_rdata_o         => csr_rdata,

    csr_mtvec_o         => csr_mtvec,

    trap_i              => trap,
    trap_cause_i        => trap_cause,
    trap_pc_i           => trap_pc,
    trap_mtval_i        => trap_mtval,
  
    inst_is_mret_i      => dec_inst_is_mret,
    
    meip_i              => meip_i,
    trap_mirq_o         => trap_mirq
    );

  --------------------------------------------------------------------
  -- Trap
  --------------------------------------------------------------------
  trap       <= '1'           when trap_mirq = '1' and regfile_we = '1' else '0';
  trap_cause <= x"8000000B";--when trap_mirq = '1' else (others => '0'); -- External Interrupt
  trap_pc    <= pc_r       ;--when trap_mirq = '1' else (others => '0');
  trap_mtval <= (others => '0'); -- No additional info for external interrupts

  --------------------------------------------------------------------
  -- Register File Instance
  --------------------------------------------------------------------

  -- We only perform a Write Back to the Register File if the FSM is in the 
  -- WRITEBACK state AND the instruction actually targets a destination register.
  regfile_rd_we    <= dec_rd_we and regfile_we;

  -- Writeback data multiplexer: 
  -- selects between the ALU result, memory data or the link address (PC+4).
  regfile_rd_wdata <= dmem_rdata_r when dec_rd_src = RD_SRC_MEM      else
                      pc_seq_r     when dec_rd_src = RD_SRC_PC_PLUS4 else
                      csr_rdata    when dec_rd_src = RD_SRC_CSR      else
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
  -- This combinatorial process determines the ALU's operation and input sources
  -- for the current FSM state. This is crucial for sharing the single ALU
  -- across different instruction phases (PC increment, address calculation,
  -- branch comparison, and general arithmetic/logic operations).
  process(all)
  begin

    case state_r is
      -- Fetch: ALU computes PC + 4 for next instruction
      when S_FETCH =>
        alu_op        <= ALU_ADD;
        alu_src_a_sel <= ALU_SRC_A_PC;
        alu_src_b_sel <= ALU_SRC_B_IMM_4;

      -- Branch Decision: ALU is used as a comparator (subtracting RS1 and RS2)
      when S_BRANCH_DECISION =>
        alu_op        <= ALU_SUB;
        alu_src_a_sel <= ALU_SRC_A_RS1;
        alu_src_b_sel <= ALU_SRC_B_RS2;
      
      -- Normal Execution (S_DECODE, S_EXECUTE, S_MEMORY, S_WRITEBACK): ALU sources and operation determined by instruction type
      when others => 
      --when S_EXECUTE =>
        alu_op        <= dec_alu_op;
        alu_src_a_sel <= dec_alu_src_a_sel;
        alu_src_b_sel <= dec_alu_src_b_sel;

    end case;
  end process;

  --------------------------------------------------------------------
  -- ALU Instance
  -- Connects selected sources to the ALU component.
  --------------------------------------------------------------------
  alu_src_a <= pc_r          when alu_src_a_sel = ALU_SRC_A_PC    else 
               dec_imm_csr   when alu_src_a_sel = ALU_SRC_A_IMM_CSR else
               src_a_val;  --when alu_src_a_sel = ALU_SRC_A_RS1

  alu_src_b <= dec_imm_i     when alu_src_b_sel = ALU_SRC_B_IMM_I else 
               dec_imm_s     when alu_src_b_sel = ALU_SRC_B_IMM_S else
               dec_imm_u     when alu_src_b_sel = ALU_SRC_B_IMM_U else
               dec_imm_j     when alu_src_b_sel = ALU_SRC_B_IMM_J else
               dec_imm_b     when alu_src_b_sel = ALU_SRC_B_IMM_B else
               C_IMM_4       when alu_src_b_sel = ALU_SRC_B_IMM_4 else
               csr_rdata     when alu_src_b_sel = ALU_SRC_B_CSR     else
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

  -- The ALU result is buffered in `alu_res_r` to maintain its value
  -- across multiple FSM cycles. For example, a memory address calculated
  -- in S_EXECUTE needs to be held until S_MEMORY, and a jump target
  -- until S_WRITEBACK.
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      alu_res_r <= (others => '0');
      elsif rising_edge(clk_i)
    then
      if (alu_res_r_we = '1')
      then
        alu_res_r <= alu_res;
      end if;
    end if;
  end process;  
  -- Note: alu_res_r_we is asserted only in S_EXECUTE, as other ALU uses
  -- (like PC+4 in S_FETCH) are immediately consumed or registered elsewhere (e.g., pc_seq_r).

  --------------------------------------------------------------------
  -- Memory
  -- This section handles the data memory interface (DMEM) for loads and stores.
  --------------------------------------------------------------------
  --------------------------------------------------------------------
  dmem_inst : entity work.WardRV_fsm_memory
  port map (
    clk_i               => clk_i,
    arst_b_i            => arst_b_i,
    
    dmem_valid_i        => dmem_valid,
    addr_i              => alu_res_r,
    wdata_i             => src_b_val,
    we_i                => dec_dmem_we,
    be_i                => dec_dmem_be,
    data_unsigned_i     => dec_dmem_data_unsigned,
    
    dmem_ready_o        => dmem_ready,
    dmem_rdata_r_o      => dmem_rdata_r,
    
    dmem_ini_o           => dmem_ini,
    dmem_tgt_i           => dmem_tgt_i
  );

  dmem_ini_o <= dmem_ini;

  --------------------------------------------------------------------
  -- Program Counter Logic
  -- This section manages the Program Counter (PC) updates, including
  -- sequential execution, branches, and jumps.
  --------------------------------------------------------------------

  -- Evaluate if a branch should be taken based on ALU flags (zero, carry, sign)
  -- and the decoded branch condition from the instruction.
  branch_taken <= '1' when(((alu_zero  and dec_branch_use_flag_zero  ) or
                            (alu_carry and dec_branch_use_flag_carry ) or
                            (alu_sign  and dec_branch_use_flag_sign  )) = dec_branch_flag_is_set)                
                   else '0';
  -- Next PC Mux: Sequential (+4), Branch target, or Jump target
  pc_r_next    <= csr_mtvec                     when trap = '1' else
                  csr_rdata                     when (dec_pc_sel = PC_SEL_XEPC) else
                  alu_res_r(31 downto 2) & "00" when (dec_pc_sel = PC_SEL_JUMP) or
                                                     (((dec_pc_sel = PC_SEL_BRANCH) and branch_taken_r = '1')) else
                  pc_seq_r; -- pc_seq_r is already PC+4

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      pc_seq_r       <= (others => '0');
      pc_r           <= RESET_ADDR;
      branch_taken_r <= '0';
    elsif rising_edge(clk_i)
    then
      if (pc_seq_r_we = '1')
      then
        pc_seq_r <= alu_res; -- PC + 4 computed by ALU
      end if;

      if (branch_taken_r_we = '1')
      then
        branch_taken_r <= branch_taken;
      end if;

      if (pc_r_we = '1')
      then
        pc_r <= pc_r_next;
      end if;

    end if;
  end process;

  --------------------------------------------------------------------
  -- FSM Control Path
  -- This section defines the FSM's outputs (control signals) and state
  -- transition logic, acting as the "orchestrator" of the processor.
  --------------------------------------------------------------------
  imem_valid         <= '1' when state_r_next = S_FETCH             else '0';
  dmem_valid         <= '1' when state_r_next = S_MEMORY            else '0';
  regfile_we         <= '1' when state_r      = S_WRITEBACK         else '0';
  alu_res_r_we       <= '1' when state_r      = S_EXECUTE           else '0';
  pc_seq_r_we        <= '1' when state_r      = S_FETCH             else '0';
  branch_taken_r_we  <= '1' when state_r      = S_BRANCH_DECISION   else '0';
  pc_r_we            <= '1' when state_r      = S_WRITEBACK         else '0';

  -- State Transition Logic: Defines how the FSM moves from one state to another.
  -- Each state's transition depends on the instruction type and external ready signals.
  process(all)
  begin
    state_r_next <= state_r;

    case state_r is
      -- S_FETCH: Request instruction from memory. Transition to S_DECODE when memory is ready.
      when S_FETCH =>
        if imem_ready = '1' then
          state_r_next <= S_DECODE;
        end if;

      when S_DECODE =>
        state_r_next <= S_EXECUTE;
        -- S_DECODE is a single-cycle combinatorial state where the instruction
        -- is interpreted and control signals are generated.

      when S_EXECUTE =>
          -- Fork the execution flow based on instruction requirements
          if dec_is_branch = '1' then
              state_r_next <= S_BRANCH_DECISION;
          elsif dec_dmem_req = '1' then
              state_r_next <= S_MEMORY;
          else
              state_r_next <= S_WRITEBACK;
          end if;

      -- S_BRANCH_DECISION: Evaluate branch condition. Always transitions to S_WRITEBACK.
      when S_BRANCH_DECISION =>
        state_r_next <= S_WRITEBACK;

      -- S_MEMORY: Request data from memory (load/store). Wait for memory ready.
      when S_MEMORY =>
        if dmem_ready = '1' then
          state_r_next <= S_WRITEBACK;
        end if;

      -- S_WRITEBACK: Write result to register file and update PC. Always transitions back to S_FETCH.
      when S_WRITEBACK =>
        state_r_next <= S_FETCH;
      when others =>
    end case;
  end process;

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      state_r          <= S_FETCH;

      elsif rising_edge(clk_i) then

        state_r <= state_r_next;

    end if;

  end process;

  -- synthesis translate_off

  --------------------------------------------------------------------
  -- Simulation-Only Reporting Process
  -- This block accumulates instruction data throughout the cycles 
  -- and prints a summary during Writeback.
  --------------------------------------------------------------------
  process(clk_i, arst_b_i)
    variable v_report : inst_t;
  begin
    if arst_b_i = '0' then

      elsif rising_edge(clk_i) then
        case state_r is

          when S_WRITEBACK =>

            v_report           := INST_UNKNOWN;
            v_report.pc        := to_bitvector(imem_addr);
            v_report.inst      := to_bitvector(imem_rdata);
            v_report.inst_type := dec_inst_type;
            v_report.rd        := to_integer(unsigned(dec_rd_addr));
            v_report.rs1       := to_integer(unsigned(dec_rs1_addr));
            v_report.rs2       := to_integer(unsigned(dec_rs2_addr));
            v_report.imm_i     := to_bitvector(dec_imm_i);
            v_report.imm_s     := to_bitvector(dec_imm_s);
            v_report.imm_b     := to_bitvector(dec_imm_b);
            v_report.imm_u     := to_bitvector(dec_imm_u);
            v_report.imm_j     := to_bitvector(dec_imm_j);
            v_report.op1       := to_bitvector(src_a_val);
            v_report.op2       := to_bitvector(src_b_val);
            v_report.mem_addr  := to_bitvector(dmem_ini.addr   );
            v_report.mem_be    := to_bitvector(dmem_ini.be     );             
            v_report.mem_rdata := to_bitvector(dmem_rdata_r);
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