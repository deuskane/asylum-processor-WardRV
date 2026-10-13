-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_debug.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Debug mode of the WardRV FSM core (RISC-V Debug Spec 1.0, Sdext,
-- execution-based : the hart executes the debug ROM, the abstract commands
-- and the program buffer of the Debug Module).
--  * CSRs (debug mode only) : dcsr (debugver=4, ebreakm, stepie, step,
--    cause, prv=3), dpc, dscratch0, dscratch1
--  * Entry in debug mode (pc <- 0xFFFF_F800, debug_mode <- 1), priorities :
--      trigger (2, before)  : end of WRITEBACK, instruction killed, dpc = pc
--      ebreak  (1)          : end of WRITEBACK, dcsr.ebreakm=1,  dpc = pc
--      resethaltreq (5)     : S_RESET (before the first instruction), dpc = pc
--      haltreq (3)          : end of WRITEBACK, dpc = next pc
--                             (S_RESET : before the first instruction)
--      step (4)             : end of WRITEBACK, dpc = next pc
--    An enabled interrupt pending at the end of WRITEBACK is taken first
--    (S_TRAP), then haltreq / step enter debug mode with dpc = mtvec.
--  * In debug mode : interrupts masked, triggers disabled, EBREAK goes back
--    to the halt entry (dcsr / dpc unchanged), DRET : pc <- dpc and
--    debug_mode <- 0 (step pending if dcsr.step). The core has no other
--    exception source, so the exception entry (0xFFFF_F808) of the ROM is
--    never used by the core itself.
--  * dcsr.step : one instruction is executed after DRET, then debug mode
--    with cause 4. Interrupts masked during the step when dcsr.stepie=0.
--  * EBREAK_EXCEPTION : EBREAK with dcsr.ebreakm=0 (or without debug) raises
--    a breakpoint exception (mcause=3, mepc = pc, mtval=0) through S_TRAP.
--  * NB_TRIGGERS > 0 : WardRV_fsm_trigger (mcontrol6).
-- All the outputs are '0' when the corresponding generics are disabled.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_fsm_debug is
  generic (
    DEBUG_ENABLE        : boolean := false;
    NB_TRIGGERS         : natural := 0;
    EBREAK_EXCEPTION    : boolean := false;
    RESETHALTREQ_ENABLE : boolean := false
  );
  port (
    clk_i                : in  std_logic;
    arst_b_i             : in  std_logic;

    -- FSM state
    state_is_reset_i     : in  std_logic;
    state_is_execute_i   : in  std_logic;
    state_is_writeback_i : in  std_logic;
    state_is_trap_i      : in  std_logic;

    -- Current instruction
    pc_i                 : in  std_logic_vector(31 downto 0); -- PC of the instruction (RESET_ADDR in S_RESET)
    pc_next_i            : in  std_logic_vector(31 downto 0); -- Next PC without debug
    mtvec_i              : in  std_logic_vector(31 downto 0);
    inst_is_ebreak_i     : in  std_logic;
    inst_is_dret_i       : in  std_logic;
    mem_req_i            : in  std_logic;
    mem_we_i             : in  std_logic;
    mem_addr_i           : in  std_logic_vector(31 downto 0); -- Load / store address (EXECUTE)
    irq_i                : in  std_logic;                     -- Interrupt pending and enabled (mstatus / mie)

    -- CSR interface
    csr_addr_i           : in  std_logic_vector(11 downto 0);
    csr_we_i             : in  std_logic;
    csr_wdata_i          : in  std_logic_vector(31 downto 0);
    csr_rdata_o          : out std_logic_vector(31 downto 0);

    -- Debug Module requests
    dbg_hart_req_i       : in  dbg_hart_req_t;

    -- Control of the FSM
    debug_mode_o         : out std_logic;  -- Hart in debug mode
    irq_mask_o           : out std_logic;  -- Interrupts masked
    kill_execute_o       : out std_logic;  -- EXECUTE : skip the memory access, go to WRITEBACK
    kill_o               : out std_logic;  -- WRITEBACK : no register / CSR write, no MRET effect
    exception_o          : out std_logic;  -- WRITEBACK : breakpoint exception, go to S_TRAP, PC unchanged
    trap_bkpt_o          : out std_logic;  -- S_TRAP : the trap is a breakpoint exception
    pc_sel_o             : out std_logic;  -- Next PC is pc_o
    pc_we_o              : out std_logic;  -- PC write in S_RESET
    pc_o                 : out std_logic_vector(31 downto 0)
  );
end entity WardRV_fsm_debug;

architecture rtl of WardRV_fsm_debug is

  -- Debug mode state
  signal debug_mode_r      : std_logic;
  signal step_pending_r    : std_logic;
  signal dcsr_ebreakm_r    : std_logic;
  signal dcsr_stepie_r     : std_logic;
  signal dcsr_step_r       : std_logic;
  signal dcsr_cause_r      : std_logic_vector(2 downto 0);
  signal dpc_r             : std_logic_vector(31 downto 0);
  signal dscratch0_r       : std_logic_vector(31 downto 0);
  signal dscratch1_r       : std_logic_vector(31 downto 0);
  signal dcsr              : std_logic_vector(31 downto 0);
  signal csr_rdata_dbg     : std_logic_vector(31 downto 0);
  signal csr_rdata_trig    : std_logic_vector(31 downto 0);

  -- Triggers
  signal trig_hit          : std_logic;
  signal trig_hit_debug    : std_logic;
  signal kill_execute      : std_logic;
  signal kill_r            : std_logic;
  signal kill_debug_r      : std_logic;

  -- Decisions
  signal haltreq           : std_logic;
  signal resethaltreq      : std_logic;
  signal irq_mask          : std_logic;
  signal ebreak_debug      : std_logic;
  signal ebreak_exc        : std_logic;
  signal trig_debug        : std_logic;
  signal trig_exception    : std_logic;
  signal halt_debug        : std_logic;
  signal step_debug        : std_logic;
  signal wb_enter          : std_logic;
  signal wb_exception      : std_logic;
  signal wb_reenter        : std_logic;
  signal wb_dret           : std_logic;
  signal trap_enter        : std_logic;
  signal reset_enter       : std_logic;
  signal enter             : std_logic;
  signal enter_cause       : std_logic_vector(2 downto 0);
  signal enter_dpc         : std_logic_vector(31 downto 0);
  signal trap_bkpt_r       : std_logic;

begin

  --------------------------------------------------------------------
  -- Triggers
  --------------------------------------------------------------------
  gen_trigger : if DEBUG_ENABLE and NB_TRIGGERS > 0 generate
    ins_trigger : entity work.WardRV_fsm_trigger
      generic map (
        NB_TRIGGERS      => NB_TRIGGERS,
        EBREAK_EXCEPTION => EBREAK_EXCEPTION
      )
      port map (
        clk_i            => clk_i,
        arst_b_i         => arst_b_i,
        debug_mode_i     => debug_mode_r,
        csr_addr_i       => csr_addr_i,
        csr_we_i         => csr_we_i,
        csr_wdata_i      => csr_wdata_i,
        csr_rdata_o      => csr_rdata_trig,
        pc_i             => pc_i,
        mem_req_i        => mem_req_i,
        mem_we_i         => mem_we_i,
        mem_addr_i       => mem_addr_i,
        hit_o            => trig_hit,
        hit_debug_o      => trig_hit_debug
      );
  end generate gen_trigger;

  gen_trigger_n : if not (DEBUG_ENABLE and NB_TRIGGERS > 0) generate
    csr_rdata_trig <= (others => '0');
    trig_hit       <= '0';
    trig_hit_debug <= '0';
  end generate gen_trigger_n;

  -- A trigger fires before the instruction : the instruction is killed
  kill_execute   <= state_is_execute_i and trig_hit;
  kill_execute_o <= kill_execute;
  kill_o         <= kill_r;

  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0'
    then
      kill_r       <= '0';
      kill_debug_r <= '0';
    elsif rising_edge(clk_i)
    then
      if state_is_execute_i = '1'
      then
        kill_r       <= kill_execute;
        kill_debug_r <= trig_hit_debug;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Debug requests
  --------------------------------------------------------------------
  haltreq      <= dbg_hart_req_i.haltreq      when DEBUG_ENABLE                        else '0';
  resethaltreq <= dbg_hart_req_i.resethaltreq when DEBUG_ENABLE and RESETHALTREQ_ENABLE else '0';

  -- Interrupts masked in debug mode, and during a step when stepie = 0
  irq_mask     <= debug_mode_r or (step_pending_r and not dcsr_stepie_r);
  irq_mask_o   <= irq_mask;

  --------------------------------------------------------------------
  -- Decisions at the end of WRITEBACK (priority order)
  --------------------------------------------------------------------
  trig_debug       <= kill_r and     kill_debug_r;
  trig_exception   <= kill_r and not kill_debug_r;
  ebreak_debug     <= not kill_r and not debug_mode_r and inst_is_ebreak_i and     dcsr_ebreakm_r;
  ebreak_exc       <= not kill_r and not debug_mode_r and inst_is_ebreak_i and not dcsr_ebreakm_r when EBREAK_EXCEPTION else '0';
  halt_debug       <= not debug_mode_r and haltreq        and not irq_i;
  step_debug       <= not debug_mode_r and step_pending_r and not irq_i;

  wb_enter         <= state_is_writeback_i and (trig_debug or ebreak_debug or
                                                ((halt_debug or step_debug) and not (trig_exception or ebreak_exc)));
  wb_exception     <= state_is_writeback_i and (trig_exception or ebreak_exc);
  wb_reenter       <= state_is_writeback_i and debug_mode_r and inst_is_ebreak_i;
  wb_dret          <= state_is_writeback_i and debug_mode_r and inst_is_dret_i;

  -- After a trap (interrupt or exception) : halt / step at the trap handler
  trap_enter       <= state_is_trap_i  and not debug_mode_r and (haltreq or step_pending_r);

  -- Before the first instruction after the hart reset
  reset_enter      <= state_is_reset_i and (haltreq or resethaltreq);

  enter            <= wb_enter or trap_enter or reset_enter;

  process(all)
  begin
    if    reset_enter = '1'
    then
      enter_dpc   <= pc_i;
      enter_cause <= DCSR_CAUSE_RESETHALTREQ when resethaltreq = '1' else DCSR_CAUSE_HALTREQ;
    elsif trap_enter = '1'
    then
      enter_dpc   <= mtvec_i;
      enter_cause <= DCSR_CAUSE_HALTREQ      when haltreq      = '1' else DCSR_CAUSE_STEP;
    elsif trig_debug = '1'
    then
      enter_dpc   <= pc_i;
      enter_cause <= DCSR_CAUSE_TRIGGER;
    elsif ebreak_debug = '1'
    then
      enter_dpc   <= pc_i;
      enter_cause <= DCSR_CAUSE_EBREAK;
    elsif halt_debug = '1'
    then
      enter_dpc   <= pc_next_i;
      enter_cause <= DCSR_CAUSE_HALTREQ;
    else
      enter_dpc   <= pc_next_i;
      enter_cause <= DCSR_CAUSE_STEP;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Program counter
  --------------------------------------------------------------------
  pc_sel_o    <= enter or wb_reenter or wb_dret;
  pc_we_o     <= reset_enter;
  pc_o        <= dpc_r when wb_dret = '1' else DBG_ADDR_HALT;
  exception_o <= wb_exception;
  trap_bkpt_o <= trap_bkpt_r;
  debug_mode_o<= debug_mode_r;

  --------------------------------------------------------------------
  -- Debug mode state and debug CSRs
  --------------------------------------------------------------------
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0'
    then
      debug_mode_r   <= '0';
      step_pending_r <= '0';
      dcsr_ebreakm_r <= '0';
      dcsr_stepie_r  <= '0';
      dcsr_step_r    <= '0';
      dcsr_cause_r   <= (others => '0');
      dpc_r          <= (others => '0');
      dscratch0_r    <= (others => '0');
      dscratch1_r    <= (others => '0');
      trap_bkpt_r    <= '0';
    elsif rising_edge(clk_i)
    then
      if state_is_writeback_i = '1'
      then
        trap_bkpt_r <= wb_exception;
      end if;

      if DEBUG_ENABLE
      then
        if enter = '1'
        then
          debug_mode_r   <= '1';
          step_pending_r <= '0';
          dcsr_cause_r   <= enter_cause;
          dpc_r          <= enter_dpc;
        elsif wb_dret = '1'
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
            when CSR_DPC       => dpc_r       <= csr_wdata_i(31 downto 2) & "00"; -- IALIGN=32
            when CSR_DSCRATCH0 => dscratch0_r <= csr_wdata_i;
            when CSR_DSCRATCH1 => dscratch1_r <= csr_wdata_i;
            when others        => null;
          end case;
        end if;
      end if;
    end if;
  end process;

  dcsr <= "0100"            & -- debugver = 4 (Debug Spec 1.0)
          "000000000000"    & -- extcause, cetrig, pelp, ebreakvs, ebreakvu
          dcsr_ebreakm_r    & -- ebreakm
          "000"             & -- ebreaks, ebreaku
          dcsr_stepie_r     & -- stepie
          "00"              & -- stopcount, stoptime
          dcsr_cause_r      & -- cause
          "000"             & -- v, mprven, nmip
          dcsr_step_r       & -- step
          "11";               -- prv = M

  process(all)
  begin
    csr_rdata_dbg <= (others => '0');
    if DEBUG_ENABLE and debug_mode_r = '1'
    then
      case csr_addr_i is
        when CSR_DCSR      => csr_rdata_dbg <= dcsr;
        when CSR_DPC       => csr_rdata_dbg <= dpc_r;
        when CSR_DSCRATCH0 => csr_rdata_dbg <= dscratch0_r;
        when CSR_DSCRATCH1 => csr_rdata_dbg <= dscratch1_r;
        when others        => null;
      end case;
    end if;
  end process;

  csr_rdata_o <= csr_rdata_dbg or csr_rdata_trig;

end architecture rtl;
