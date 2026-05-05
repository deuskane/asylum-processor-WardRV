-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_csr.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-- This module implements the Control and Status Registers (CSR) for the 
-- WardRV processor, handling traps, interrupts, and performance counters.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-05-01  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;
use     asylum.WardRV_decode_pkg.all;

entity WardRV_fsm_csr is
  generic (
    HARTID     : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    clk_i               : in  std_logic;
    arst_b_i            : in  std_logic;

    -- Decoder / Pipeline Interface
    csr_addr_i          : in  std_logic_vector(11 downto 0);
    csr_we_i            : in  std_logic;
    csr_re_i            : in  std_logic;
    csr_wdata_i         : in  std_logic_vector(31 downto 0);
    csr_rdata_o         : out std_logic_vector(31 downto 0);

    -- CSR Outputs
    csr_mtvec_o         : out std_logic_vector(31 downto 0);

    -- Trap Management (Exceptions/Interrupts)
    trap_i              : in  std_logic;
    trap_cause_i        : in  std_logic_vector(31 downto 0);
    trap_pc_i           : in  std_logic_vector(31 downto 0);
    trap_mtval_i        : in  std_logic_vector(31 downto 0);

    -- MRET Instruction
    inst_is_mret_i      : in  std_logic;

    irq_i               : in  std_logic;
    trap_mirq_o         : out std_logic
  );
end entity WardRV_fsm_csr;

architecture behavioural of WardRV_fsm_csr is

  -- Register Storage (Machine Mode Subset)
  signal mhartid_q  : std_logic_vector(31 downto 0) := HARTID;
  signal mstatus_q  : std_logic_vector(31 downto 0);
  signal mtvec_q    : std_logic_vector(31 downto 0);
  signal mepc_q     : std_logic_vector(31 downto 0);
  signal mcause_q   : std_logic_vector(31 downto 0);
  signal mtval_q    : std_logic_vector(31 downto 0);
  signal mscratch_q : std_logic_vector(31 downto 0);
  signal mie_q      : std_logic_vector(31 downto 0);
  signal mip_q      : std_logic_vector(31 downto 0);

begin

  -- Read Logic
  process(all)
  begin
    csr_rdata_o <= (others => '0');
    case csr_addr_i is
      when CSR_MHARTID  => csr_rdata_o <= mhartid_q ;
      when CSR_MSTATUS  => csr_rdata_o <= mstatus_q ;
      when CSR_MIE      => csr_rdata_o <= mie_q     ;
      when CSR_MIP      => csr_rdata_o <= mip_q     ;
      when CSR_MTVEC    => csr_rdata_o <= mtvec_q   ;
      when CSR_MSCRATCH => csr_rdata_o <= mscratch_q;
      when CSR_MEPC     => csr_rdata_o <= mepc_q    ;
      when CSR_MCAUSE   => csr_rdata_o <= mcause_q  ;
      when CSR_MTVAL    => csr_rdata_o <= mtval_q   ;
      when others       => csr_rdata_o <= (others => '0');
    end case;
  end process;

  -- Write and Update Logic
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' 
    then
      mstatus_q  <= x"00001800"; -- MPP = 11 (Machine mode) by default
      mie_q      <= (others => '0');
      mip_q      <= (others => '0');
      mtvec_q    <= (others => '0');
      mepc_q     <= (others => '0');
      mcause_q   <= (others => '0');
      mtval_q    <= (others => '0');
      mscratch_q <= (others => '0');
    elsif rising_edge(clk_i) 
    then

      mip_q(11) <= irq_i; -- MEIP (Machine External Interrupt Pending)  

      if trap_i = '1' 
      then
        mstatus_q(7) <= mstatus_q(3); -- MPIE <= MIE
        mstatus_q(3) <= '0';          -- MIE  <= 0
        mepc_q       <= trap_pc_i;
        mcause_q     <= trap_cause_i;
        mtval_q      <= trap_mtval_i;
      elsif inst_is_mret_i = '1' 
      then
        mstatus_q(3) <= mstatus_q(7); -- MIE  <= MPIE
        mstatus_q(7) <= '1';          -- MPIE <= 1
      elsif csr_we_i = '1' 
      then
        case csr_addr_i is
          when CSR_MSTATUS  => mstatus_q  <= csr_wdata_i;
          when CSR_MIE      => mie_q      <= csr_wdata_i;
        --when CSR_MIP      => mip_q      <= csr_wdata_i;
          when CSR_MTVEC    => mtvec_q    <= csr_wdata_i;
          when CSR_MSCRATCH => mscratch_q <= csr_wdata_i;
          when CSR_MEPC     => mepc_q     <= csr_wdata_i;
          when CSR_MCAUSE   => mcause_q   <= csr_wdata_i;
          when CSR_MTVAL    => mtval_q    <= csr_wdata_i;
          when others       => null;
        end case;
      end if;
    end if;
  end process;

  -- Trap signal output for the pipeline
  trap_mirq_o <= '1' when mip_q(11) = '1' and mie_q(11) = '1' and mstatus_q(3) = '1' else '0';

  -- CSR Outputs
  csr_mtvec_o <= mtvec_q;
end architecture behavioural;