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

    meip_i              : in  std_logic;
    trap_mirq_o         : out std_logic
  );
end entity WardRV_fsm_csr;

architecture behavioural of WardRV_fsm_csr is

  -- Register Storage (Machine Mode Subset)
  signal mhartid_r  : std_logic_vector(31 downto 0) := HARTID;
  signal mstatus_r  : std_logic_vector(31 downto 0);
  signal mtvec_r    : std_logic_vector(31 downto 0);
  signal mepc_r     : std_logic_vector(31 downto 0);
  signal mcause_r   : std_logic_vector(31 downto 0);
  signal mtval_r    : std_logic_vector(31 downto 0);
  signal mscratch_r : std_logic_vector(31 downto 0);
  signal mie_r      : std_logic_vector(31 downto 0);
  signal mip_r      : std_logic_vector(31 downto 0);

begin

  -- Read Logic
  process(all)
  begin
    csr_rdata_o <= (others => '0');
    case csr_addr_i is
      when CSR_MHARTID  => csr_rdata_o <= mhartid_r ;
      when CSR_MSTATUS  => csr_rdata_o <= mstatus_r ;
      when CSR_MIE      => csr_rdata_o <= mie_r     ;
      when CSR_MIP      => csr_rdata_o <= mip_r     ;
      when CSR_MTVEC    => csr_rdata_o <= mtvec_r   ;
      when CSR_MSCRATCH => csr_rdata_o <= mscratch_r;
      when CSR_MEPC     => csr_rdata_o <= mepc_r    ;
      when CSR_MCAUSE   => csr_rdata_o <= mcause_r  ;
      when CSR_MTVAL    => csr_rdata_o <= mtval_r   ;
      when others       => csr_rdata_o <= (others => '0');
    end case;
  end process;

  -- Write and Update Logic
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' 
    then
      mstatus_r  <= x"00001800"; -- MPP = 11 (Machine mode) by default
      mie_r      <= (others => '0');
      mip_r      <= (others => '0');
      mtvec_r    <= (others => '0');
      mepc_r     <= (others => '0');
      mcause_r   <= (others => '0');
      mtval_r    <= (others => '0');
      mscratch_r <= (others => '0');
    elsif rising_edge(clk_i) 
    then

      mip_r(11) <= meip_i; -- MEIP (Machine External Interrupt Pending)  

      if trap_i = '1' 
      then
        mstatus_r(7) <= mstatus_r(3); -- MPIE <= MIE
        mstatus_r(3) <= '0';          -- MIE  <= 0
        mepc_r       <= trap_pc_i;
        mcause_r     <= trap_cause_i;
        mtval_r      <= trap_mtval_i;
      elsif inst_is_mret_i = '1' 
      then
        mstatus_r(3) <= mstatus_r(7); -- MIE  <= MPIE
        mstatus_r(7) <= '1';          -- MPIE <= 1
      elsif csr_we_i = '1' 
      then
        case csr_addr_i is
          when CSR_MSTATUS  => mstatus_r  <= csr_wdata_i;
          when CSR_MIE      => mie_r      <= csr_wdata_i;
        --when CSR_MIP      => mip_r      <= csr_wdata_i;
          when CSR_MTVEC    => mtvec_r    <= csr_wdata_i;
          when CSR_MSCRATCH => mscratch_r <= csr_wdata_i;
          when CSR_MEPC     => mepc_r     <= csr_wdata_i;
          when CSR_MCAUSE   => mcause_r   <= csr_wdata_i;
          when CSR_MTVAL    => mtval_r    <= csr_wdata_i;
          when others       => null;
        end case;
      end if;
    end if;
  end process;

  -- Trap signal output for the pipeline
  trap_mirq_o <= '1' when mip_r(11) = '1' and mie_r(11) = '1' and mstatus_r(3) = '1' and inst_is_mret_i = '0' else '0';

  -- CSR Outputs
  csr_mtvec_o <= mtvec_r;
end architecture behavioural;