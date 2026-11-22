-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_debug.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Debug support of the WardRV FSM core (Debug Spec 1.0 chapter 4, Sdext) :
-- what the core does for the Debug Module (WardRV_dbg_dm).
--  * Debug mode (4.1) : the core executes the debug ROM, the abstract
--    commands and the program buffer at 0xFFFF_F800. Interrupts are masked,
--    triggers are off, ebreak goes back to 0xFFFF_F800, dret leaves debug
--    mode at dpc.
--  * CSRs (4.9), in debug mode only : dcsr (debugver 4, ebreakm, stepie,
--    cause, step, prv 3), dpc, dscratch0.
--  * Debug mode entry, pc <= 0xFFFF_F800 (cause, dpc) :
--      before an instruction (EXECUTE, the instruction is abandoned) :
--        trigger (2), ebreak with dcsr.ebreakm (1)                  dpc = pc
--      before the first instruction after the reset (RESET) :
--        resethaltreq (5), haltreq (3)                               dpc = pc
--      after an instruction (WRITEBACK, or TRAP when it takes an interrupt) :
--        haltreq (3), then dcsr.step (4)                             dpc = next pc
--  * dcsr.step : DRET runs one instruction, then debug mode. Interrupts are
--    masked during the step if dcsr.stepie = 0.
--  * Breakpoint exception (EBREAK_EXCEPTION) : ebreak without dcsr.ebreakm,
--    or a trigger of M-mode, traps in EXECUTE (mcause 3, mepc = pc).
--  * Triggers (NB_TRIGGERS, chapter 5) : WardRV_fsm_trigger, address ranges
--    with TRIGGER_RANGE_ENABLE.
-- With DEBUG_ENABLE and EBREAK_EXCEPTION false, every output is '0'.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-- 2026-11-18  1.1      mrosiere TRIGGER_RANGE_ENABLE
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_fsm_debug is
  generic (
    DEBUG_ENABLE         : boolean := false                        -- Debug mode
   ;NB_TRIGGERS          : natural := 0                            -- Triggers (with DEBUG_ENABLE)
   ;EBREAK_EXCEPTION     : boolean := false                        -- Breakpoint exception
   ;RESETHALTREQ_ENABLE  : boolean := false                        -- Halt on reset request
   ;TRIGGER_RANGE_ENABLE : boolean := false                        -- Triggers : match >= / < and chain
  );
  port (
    clk_i               : in  std_logic
   ;arst_b_i            : in  std_logic
    -- FSM state
   ;state_is_reset_i    : in  std_logic
   ;state_is_execute_i  : in  std_logic
   ;state_is_writeback_i: in  std_logic
   ;state_is_trap_i     : in  std_logic
    -- Instruction
   ;pc_i                : in  std_logic_vector(32-1 downto 0)     -- RESET_ADDR in RESET
   ;pc_next_i           : in  std_logic_vector(32-1 downto 0)     -- Next pc (WRITEBACK, TRAP)
   ;inst_is_ebreak_i    : in  std_logic
   ;inst_is_dret_i      : in  std_logic
   ;mem_req_i           : in  std_logic                           -- Load / store
   ;mem_we_i            : in  std_logic
   ;mem_addr_i          : in  std_logic_vector(32-1 downto 0)     -- Address (EXECUTE)
   ;irq_i               : in  std_logic                           -- Interrupt taken after WRITEBACK
    -- CSR
   ;csr_addr_i          : in  std_logic_vector(12-1 downto 0)
   ;csr_we_i            : in  std_logic
   ;csr_wdata_i         : in  std_logic_vector(32-1 downto 0)
   ;csr_rdata_o         : out std_logic_vector(32-1 downto 0)     -- 0 : not a debug CSR
    -- Debug Module
   ;dbg_hart_req_i      : in  dbg_hart_req_t
    -- To the FSM
   ;debug_mode_o        : out std_logic
   ;irq_mask_o          : out std_logic                           -- Interrupts masked
   ;exception_o         : out std_logic                           -- EXECUTE : breakpoint exception
   ;trap_bkpt_o         : out std_logic                           -- TRAP : the trap is a breakpoint
   ;pc_we_o             : out std_logic                           -- pc <= pc_o
   ;pc_o                : out std_logic_vector(32-1 downto 0)
  );
end entity WardRV_fsm_debug;

architecture rtl of WardRV_fsm_debug is

  signal   debug_mode_r      : std_logic;
  signal   step_pending_r    : std_logic;                         -- DRET with dcsr.step
  signal   dcsr_ebreakm_r    : std_logic;
  signal   dcsr_stepie_r     : std_logic;
  signal   dcsr_step_r       : std_logic;
  signal   dcsr_cause_r      : std_logic_vector(2 downto 0);
  signal   dpc_r             : std_logic_vector(32-1 downto 0);
  signal   dscratch0_r       : std_logic_vector(32-1 downto 0);
  signal   exception         : std_logic;
  signal   trap_bkpt_r       : std_logic;
  signal   csr_rdata_debug   : std_logic_vector(32-1 downto 0);
  signal   csr_rdata_trigger : std_logic_vector(32-1 downto 0);

  signal   haltreq           : std_logic;
  signal   resethaltreq      : std_logic;
  signal   trig_hit          : std_logic;
  signal   trig_hit_debug    : std_logic;

  -- Debug mode entries and exits
  signal   trigger_enter     : std_logic;
  signal   ebreak_enter      : std_logic;
  signal   reset_enter       : std_logic;
  signal   after_inst        : std_logic;                         -- End of an instruction
  signal   halt_enter        : std_logic;
  signal   step_enter        : std_logic;
  signal   enter             : std_logic;
  signal   ebreak_reenter    : std_logic;                         -- ebreak in debug mode
  signal   dret              : std_logic;

begin  -- architecture rtl

  haltreq      <= dbg_hart_req_i.haltreq      when DEBUG_ENABLE                        else '0';
  resethaltreq <= dbg_hart_req_i.resethaltreq when DEBUG_ENABLE and RESETHALTREQ_ENABLE else '0';

  -----------------------------------------------------------------------------
  -- Triggers
  -----------------------------------------------------------------------------
  gen_trigger : if DEBUG_ENABLE and NB_TRIGGERS > 0
  generate
    ins_trigger : entity asylum.WardRV_fsm_trigger
      generic map
      (NB_TRIGGERS          => NB_TRIGGERS
      ,EBREAK_EXCEPTION     => EBREAK_EXCEPTION
      ,TRIGGER_RANGE_ENABLE => TRIGGER_RANGE_ENABLE
      )
      port map
      (clk_i                => clk_i
      ,arst_b_i             => arst_b_i
      ,debug_mode_i         => debug_mode_r
      ,csr_addr_i           => csr_addr_i
      ,csr_we_i             => csr_we_i
      ,csr_wdata_i          => csr_wdata_i
      ,csr_rdata_o          => csr_rdata_trigger
      ,pc_i                 => pc_i
      ,mem_req_i            => mem_req_i
      ,mem_we_i             => mem_we_i
      ,mem_addr_i           => mem_addr_i
      ,hit_o                => trig_hit
      ,hit_debug_o          => trig_hit_debug
      );
  end generate gen_trigger;

  gen_trigger_b : if not (DEBUG_ENABLE and NB_TRIGGERS > 0)
  generate
    csr_rdata_trigger <= (others => '0');
    trig_hit          <= '0';
    trig_hit_debug    <= '0';
  end generate gen_trigger_b;

  -----------------------------------------------------------------------------
  -- Debug mode entries (priority : the first one wins)
  -----------------------------------------------------------------------------
  -- Before the instruction (EXECUTE)
  trigger_enter  <= state_is_execute_i and trig_hit and trig_hit_debug;
  ebreak_enter   <= state_is_execute_i and inst_is_ebreak_i and dcsr_ebreakm_r and not trig_hit and not debug_mode_r;
  -- Before the first instruction (RESET)
  reset_enter    <= state_is_reset_i   and (haltreq or resethaltreq);
  -- After the instruction : end of WRITEBACK, or TRAP when an interrupt is taken
  after_inst     <= ((state_is_writeback_i and not irq_i) or state_is_trap_i) and not debug_mode_r;
  halt_enter     <= after_inst and haltreq;
  step_enter     <= after_inst and step_pending_r;

  enter          <= trigger_enter or ebreak_enter or reset_enter or halt_enter or step_enter;

  -- In debug mode : ebreak parks the hart in the debug ROM again, dret leaves
  ebreak_reenter <= state_is_execute_i and inst_is_ebreak_i and debug_mode_r;
  dret           <= state_is_execute_i and inst_is_dret_i   and debug_mode_r;

  pc_we_o        <= enter or ebreak_reenter or dret;
  pc_o           <= dpc_r when dret = '1' else DBG_MEM_BASE;

  -----------------------------------------------------------------------------
  -- Breakpoint exception (EXECUTE), its cause is used in TRAP
  -----------------------------------------------------------------------------
  exception      <= state_is_execute_i and not debug_mode_r and
                    ((trig_hit and not trig_hit_debug) or
                     (inst_is_ebreak_i and not dcsr_ebreakm_r and not trig_hit)) when EBREAK_EXCEPTION else
                    '0';

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' or not EBREAK_EXCEPTION
    then
      trap_bkpt_r <= '0';
    elsif rising_edge(clk_i)
    then
      if state_is_execute_i = '1'
      then
        trap_bkpt_r <= exception;
      end if;
    end if;
  end process;

  trap_bkpt_o    <= trap_bkpt_r;

  -----------------------------------------------------------------------------
  -- Debug mode and debug CSRs
  -----------------------------------------------------------------------------
  -- Without DEBUG_ENABLE the registers keep their reset value
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' or not DEBUG_ENABLE
    then
      debug_mode_r   <= '0';
      step_pending_r <= '0';
      dcsr_ebreakm_r <= '0';
      dcsr_stepie_r  <= '0';
      dcsr_step_r    <= '0';
      dcsr_cause_r   <= (others => '0');
      dpc_r          <= (others => '0');
      dscratch0_r    <= (others => '0');
    elsif rising_edge(clk_i)
    then
      if enter = '1'
      then
        debug_mode_r   <= '1';
        step_pending_r <= '0';
        if trigger_enter = '1' or ebreak_enter = '1' or reset_enter = '1'
        then
          dpc_r        <= pc_i;
        else
          dpc_r        <= pc_next_i;
        end if;
        if    trigger_enter = '1'
        then
          dcsr_cause_r <= DCSR_CAUSE_TRIGGER;
        elsif ebreak_enter  = '1'
        then
          dcsr_cause_r <= DCSR_CAUSE_EBREAK;
        elsif resethaltreq  = '1' and reset_enter = '1'
        then
          dcsr_cause_r <= DCSR_CAUSE_RESETHALTREQ;
        elsif haltreq       = '1'
        then
          dcsr_cause_r <= DCSR_CAUSE_HALTREQ;
        else
          dcsr_cause_r <= DCSR_CAUSE_STEP;
        end if;
      elsif dret = '1'
      then
        debug_mode_r   <= '0';
        step_pending_r <= dcsr_step_r;
      end if;

      if csr_we_i = '1' and debug_mode_r = '1'
      then
        case csr_addr_i is
          when CSR_DCSR      =>
            dcsr_ebreakm_r <= csr_wdata_i(15);
            dcsr_stepie_r  <= csr_wdata_i(11);
            dcsr_step_r    <= csr_wdata_i(2);
          when CSR_DPC       => dpc_r       <= csr_wdata_i(32-1 downto 2) & "00";  -- IALIGN = 32
          when CSR_DSCRATCH0 => dscratch0_r <= csr_wdata_i;
          when others        => null;
        end case;
      end if;
    end if;
  end process;

  process(all)
  begin
    csr_rdata_debug <= (others => '0');
    if debug_mode_r = '1'
    then
      case csr_addr_i is
        when CSR_DCSR      =>
          csr_rdata_debug(31 downto 28) <= "0100";                -- debugver 4
          csr_rdata_debug(15)           <= dcsr_ebreakm_r;
          csr_rdata_debug(11)           <= dcsr_stepie_r;
          csr_rdata_debug( 8 downto  6) <= dcsr_cause_r;
          csr_rdata_debug(2)            <= dcsr_step_r;
          csr_rdata_debug( 1 downto  0) <= "11";                  -- prv : M-mode
        when CSR_DPC       => csr_rdata_debug <= dpc_r;
        when CSR_DSCRATCH0 => csr_rdata_debug <= dscratch0_r;
        when others        => null;
      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Outputs
  -----------------------------------------------------------------------------
  csr_rdata_o  <= csr_rdata_debug or csr_rdata_trigger;
  debug_mode_o <= debug_mode_r;
  exception_o  <= exception;
  irq_mask_o   <= debug_mode_r or (step_pending_r and not dcsr_stepie_r);

end architecture rtl;
