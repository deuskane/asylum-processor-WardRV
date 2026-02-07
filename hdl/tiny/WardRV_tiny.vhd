library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;
use work.WardRV_tiny_pkg.all;

-- =============================================================================
-- 5. TOP LEVEL (WardRV_tiny)
-- =============================================================================
entity WardRV_tiny is
  generic (
    RESET_ADDR : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;
    inst_ini_o : out inst_ini_t;
    inst_tgt_i : in  inst_tgt_t;
    sbi_ini_o  : out sbi_ini_t;
    sbi_tgt_i  : in  sbi_tgt_t
  );
end entity;

architecture rtl of WardRV_tiny is

  -- Pipeline Signals (Stage 1 -> Stage 2)
  signal s1_pc, s1_inst : std_logic_vector(31 downto 0);
  signal s1_rs1, s1_rs2, s1_imm : std_logic_vector(31 downto 0);
  signal s1_rd : std_logic_vector(4 downto 0);
  signal s1_opcode : std_logic_vector(6 downto 0);
  signal s1_funct3 : std_logic_vector(2 downto 0);
  signal s1_funct7 : std_logic_vector(6 downto 0);

  signal pipe_reg : pipe_reg_t;

  -- Stage 2 Signals
  signal s2_alu_res, s2_mem_rdata : std_logic_vector(31 downto 0);
  signal s2_branch_req : std_logic;
  signal s2_branch_tgt : std_logic_vector(31 downto 0);
  signal s2_busy : std_logic;
  signal s2_wb_data : std_logic_vector(31 downto 0);
  signal s2_wb_we : std_logic;

  -- Control
  signal stall_fetch : std_logic;
  signal flush_pipe  : std_logic;
  signal flush_pending : std_logic;
  signal fetch_pc    : std_logic_vector(31 downto 0);

begin

  -- 1. PC Module
  u_pc : entity work.WardRV_tiny_pc
    generic map (RESET_ADDR => RESET_ADDR)
    port map (
      clk_i        => clk_i,
      arst_b_i     => arst_b_i,
      stall_i      => stall_fetch,
      branch_req_i => s2_branch_req,
      branch_tgt_i => s2_branch_tgt,
      pc_o         => s1_pc
    );

  -- Fetch Interface
  inst_ini_o.valid <= '1';
  inst_ini_o.addr  <= s1_pc;
  s1_inst          <= inst_tgt_i.inst;

  -- Fetch PC Pipeline Register (to align PC with Instruction in Decode)
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      fetch_pc <= RESET_ADDR;
    elsif rising_edge(clk_i) then
      if stall_fetch = '0' then
        fetch_pc <= s1_pc;
      end if;
    end if;
  end process;

  -- 2. Decode Module
  u_decode : entity work.WardRV_tiny_decode
    port map (
      clk_i      => clk_i,
      arst_b_i   => arst_b_i,
      pc_i       => fetch_pc,
      inst_i     => s1_inst,
      wb_we_i    => s2_wb_we,
      wb_addr_i  => pipe_reg.rd,
      wb_data_i  => s2_wb_data,
      fwd_we_i   => s2_wb_we,   -- Forward from EX/WB stage
      fwd_addr_i => pipe_reg.rd,
      fwd_data_i => s2_wb_data,
      rs1_data_o => s1_rs1,
      rs2_data_o => s1_rs2,
      imm_o      => s1_imm,
      rd_o       => s1_rd,
      opcode_o   => s1_opcode,
      funct3_o   => s1_funct3,
      funct7_o   => s1_funct7,
      pc_o       => open
    );

  -- Pipeline Register Process
  stall_fetch <= s2_busy or (not inst_tgt_i.ready);
  flush_pipe  <= s2_branch_req;

  -- Flush Pending Logic (Handle 2-cycle flush for branches)
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      flush_pending <= '0';
    elsif rising_edge(clk_i) then
      if s2_busy = '0' then
        if s2_branch_req = '1' then
          flush_pending <= '1';
        else
          flush_pending <= '0';
        end if;
      end if;
    end if;
  end process;

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      pipe_reg.valid <= '0';
      pipe_reg.rd    <= (others => '0');
      pipe_reg.opcode <= (others => '0');
    elsif rising_edge(clk_i) then
      if s2_busy = '0' then
        if flush_pipe = '1' or flush_pending = '1' then
          pipe_reg.valid <= '0';
          pipe_reg.opcode <= (others => '0'); -- NOP
        elsif inst_tgt_i.ready = '1' then
          pipe_reg.pc       <= fetch_pc;
          pipe_reg.rs1_data <= s1_rs1;
          pipe_reg.rs2_data <= s1_rs2;
          pipe_reg.imm      <= s1_imm;
          pipe_reg.rd       <= s1_rd;
          pipe_reg.opcode   <= s1_opcode;
          pipe_reg.funct3   <= s1_funct3;
          pipe_reg.funct7   <= s1_funct7;
          pipe_reg.valid    <= '1';
        else
          -- Insert bubble if fetch not ready
          pipe_reg.valid <= '0';
          pipe_reg.opcode <= (others => '0');
        end if;
      end if;
    end if;
  end process;

  -- 3. Execute Module
  u_execute : entity work.WardRV_tiny_execute
    port map (
      pc_i         => pipe_reg.pc,
      rs1_data_i   => pipe_reg.rs1_data,
      rs2_data_i   => pipe_reg.rs2_data,
      imm_i        => pipe_reg.imm,
      opcode_i     => pipe_reg.opcode,
      funct3_i     => pipe_reg.funct3,
      funct7_i     => pipe_reg.funct7,
      alu_res_o    => s2_alu_res,
      branch_req_o => s2_branch_req,
      branch_tgt_o => s2_branch_tgt
    );

  -- 4. Memory Module (LSU)
  u_lsu : entity work.WardRV_tiny_lsu_8b
    port map (
      clk_i      => clk_i,
      arst_b_i   => arst_b_i,
      alu_res_i   => s2_alu_res,
      rs2_data_i  => pipe_reg.rs2_data,
      opcode_i    => pipe_reg.opcode,
      funct3_i    => pipe_reg.funct3,
      sbi_ini_o   => sbi_ini_o,
      sbi_tgt_i   => sbi_tgt_i,
      mem_rdata_o => s2_mem_rdata,
      busy_o      => s2_busy
    );

  -- Writeback Logic (Combinatorial in Stage 2)
  s2_wb_data <= std_logic_vector(unsigned(pipe_reg.pc) + 4) when (pipe_reg.opcode = OPC_JAL or pipe_reg.opcode = OPC_JALR) else
                s2_mem_rdata when pipe_reg.opcode = OPC_LOAD else
                s2_alu_res;

  s2_wb_we   <= '1' when (pipe_reg.valid = '1' and 
                          (pipe_reg.opcode = OPC_LUI or 
                           pipe_reg.opcode = OPC_AUIPC or
                           pipe_reg.opcode = OPC_JAL or
                           pipe_reg.opcode = OPC_JALR or
                           pipe_reg.opcode = OPC_LOAD or
                           pipe_reg.opcode = OPC_OP_IMM or
                           pipe_reg.opcode = OPC_OP)) else '0';

end architecture;
