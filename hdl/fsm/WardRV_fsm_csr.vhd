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
-- 2026-10-09  1.1      mrosiere mepc[1:0] read-only zero (IALIGN=32)
-- 2026-11-17  1.2      mrosiere inst_is_mret_i : one cycle pulse (WRITEBACK)
-- 2026-11-20  1.3      mrosiere Only the implemented bits (WARL) : mstatus MIE,
--                               MPIE, MPP = 3 ; mie MEIE ; mcause Interrupt and
--                               Exception Code (4:0) ; mtval read-only zero
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

    -- MRET Instruction : one cycle pulse at the end of the instruction
    -- (WRITEBACK), MIE <= MPIE and MPIE <= 1 applied once
    inst_is_mret_i      : in  std_logic;

    meip_i              : in  std_logic;
    trap_mirq_o         : out std_logic
  );
end entity WardRV_fsm_csr;

architecture behavioural of WardRV_fsm_csr is

  -- Register Storage (Machine Mode Subset) : only the implemented bits, the
  -- other bits of each CSR read as 0 and ignore writes
  --   mstatus : MIE (3), MPIE (7), MPP (12:11) read-only 3 (machine mode only)
  --   mie     : MEIE (11)          mip    : MEIP (11), read-only
  --   mcause  : Interrupt (31) and Exception Code (4:0, WLRL : 0 to 31)
  --   mtval   : read-only zero (no trap writes a non-zero value)
  signal mhartid_r      : std_logic_vector(31 downto 0) := HARTID;
  signal mstatus_mie_r  : std_logic;
  signal mstatus_mpie_r : std_logic;
  signal mie_meie_r     : std_logic;
  signal mip_meip_r     : std_logic;
  signal mtvec_r        : std_logic_vector(31 downto 0);
  signal mepc_r         : std_logic_vector(31 downto 0);
  signal mcause_int_r   : std_logic;
  signal mcause_code_r  : std_logic_vector( 4 downto 0);
  signal mscratch_r     : std_logic_vector(31 downto 0);

  -- Interrupt enables after the CSR write of the current instruction
  signal mstatus_mie    : std_logic;
  signal mie_meie       : std_logic;

begin

  -- Read Logic
  process(all)
  begin
    csr_rdata_o <= (others => '0');
    case csr_addr_i is
      when CSR_MHARTID  => csr_rdata_o               <= mhartid_r ;
      when CSR_MSTATUS  => csr_rdata_o(12 downto 11) <= "11";  -- MPP = 3 (M-mode only)
                           csr_rdata_o( 7)           <= mstatus_mpie_r;
                           csr_rdata_o( 3)           <= mstatus_mie_r;
      when CSR_MIE      => csr_rdata_o(11)           <= mie_meie_r;
      when CSR_MIP      => csr_rdata_o(11)           <= mip_meip_r;
      when CSR_MTVEC    => csr_rdata_o               <= mtvec_r   ;
      when CSR_MSCRATCH => csr_rdata_o               <= mscratch_r;
      when CSR_MEPC     => csr_rdata_o               <= mepc_r    ;
      when CSR_MCAUSE   => csr_rdata_o(31)           <= mcause_int_r;
                           csr_rdata_o( 4 downto  0) <= mcause_code_r;
      when others       => null;                         -- mtval : 0
    end case;
  end process;

  -- Write and Update Logic
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' 
    then
      mstatus_mie_r  <= '0';
      mstatus_mpie_r <= '0';
      mie_meie_r     <= '0';
      mip_meip_r     <= '0';
      mtvec_r        <= (others => '0');
      mepc_r         <= (others => '0');
      mcause_int_r   <= '0';
      mcause_code_r  <= (others => '0');
      mscratch_r     <= (others => '0');
    elsif rising_edge(clk_i) 
    then

      mip_meip_r <= meip_i; -- MEIP (Machine External Interrupt Pending)  

      if trap_i = '1' 
      then
        mstatus_mpie_r <= mstatus_mie_r; -- MPIE <= MIE
        mstatus_mie_r  <= '0';           -- MIE  <= 0
        mepc_r         <= trap_pc_i;
        mcause_int_r   <= trap_cause_i(31);
        mcause_code_r  <= trap_cause_i(4 downto 0);
      elsif inst_is_mret_i = '1' 
      then
        mstatus_mie_r  <= mstatus_mpie_r; -- MIE  <= MPIE
        mstatus_mpie_r <= '1';            -- MPIE <= 1
      elsif csr_we_i = '1' 
      then
        case csr_addr_i is
          when CSR_MSTATUS  => mstatus_mie_r  <= csr_wdata_i(3);
                               mstatus_mpie_r <= csr_wdata_i(7);
          when CSR_MIE      => mie_meie_r     <= csr_wdata_i(11);
          when CSR_MTVEC    => mtvec_r        <= csr_wdata_i(31 downto 2) & "00"; -- MODE = 0 (direct only, WARL)
          when CSR_MSCRATCH => mscratch_r     <= csr_wdata_i;
          when CSR_MEPC     => mepc_r         <= csr_wdata_i(31 downto 2) & "00"; -- IALIGN=32
          when CSR_MCAUSE   => mcause_int_r   <= csr_wdata_i(31);
                               mcause_code_r  <= csr_wdata_i(4 downto 0);
          when others       => null;
        end case;
      end if;
    end if;
  end process;

  -- Interrupt enables seen by the trap request : an explicit write of
  -- mstatus / mie (WRITEBACK) is taken into account at once, as the trap
  -- conditions must be evaluated immediately after it (priv spec 3.1.9) :
  -- no trap after the instruction that clears MIE / MEIE, a trap right after
  -- the instruction that sets them while the interrupt is pending
  mstatus_mie <= csr_wdata_i(3)  when csr_we_i = '1' and csr_addr_i = CSR_MSTATUS else mstatus_mie_r;
  mie_meie    <= csr_wdata_i(11) when csr_we_i = '1' and csr_addr_i = CSR_MIE     else mie_meie_r;

  -- Trap signal output for the pipeline
  trap_mirq_o <= '1' when mip_meip_r = '1' and mie_meie = '1' and mstatus_mie = '1' and inst_is_mret_i = '0' else '0';

  -- CSR Outputs
  csr_mtvec_o <= mtvec_r;
end architecture behavioural;
