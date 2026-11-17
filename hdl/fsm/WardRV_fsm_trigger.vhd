-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_trigger.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Trigger module (Debug Spec 1.0 chapter 5, Sdtrig) : NB_TRIGGERS address
-- triggers of type mcontrol6 (5.5.11), the hardware breakpoints and
-- watchpoints of a debugger.
--  * CSRs : tselect, tdata1, tdata2, tdata3 (0), tinfo (mcontrol6 only)
--  * A trigger fires on the address of the instruction (execute) or of a
--    load / store, before the instruction : match equal, M-mode (m), no
--    chain. The other fields are read-only zero.
--  * action is dmode : a trigger of the debugger (dmode = 1, written in
--    debug mode only) enters debug mode, a trigger of M-mode software
--    (dmode = 0) raises a breakpoint exception, which needs
--    EBREAK_EXCEPTION (otherwise its m bit stays 0 : it never fires).
--  * A trigger with dmode = 1 is writable in debug mode only. Triggers
--    never fire in debug mode.
-- hit_o is valid in the EXECUTE state (load / store address on mem_addr_i).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;

entity WardRV_fsm_trigger is
  generic (
    NB_TRIGGERS      : positive := 1
   ;EBREAK_EXCEPTION : boolean  := false                          -- Action 0 (breakpoint exception) available
  );
  port (
    clk_i            : in  std_logic
   ;arst_b_i         : in  std_logic
   ;debug_mode_i     : in  std_logic
    -- CSR
   ;csr_addr_i       : in  std_logic_vector(12-1 downto 0)
   ;csr_we_i         : in  std_logic
   ;csr_wdata_i      : in  std_logic_vector(32-1 downto 0)
   ;csr_rdata_o      : out std_logic_vector(32-1 downto 0)
    -- Instruction in EXECUTE
   ;pc_i             : in  std_logic_vector(32-1 downto 0)
   ;mem_req_i        : in  std_logic
   ;mem_we_i         : in  std_logic
   ;mem_addr_i       : in  std_logic_vector(32-1 downto 0)
    -- A trigger fires (before the instruction), its action
   ;hit_o            : out std_logic
   ;hit_debug_o      : out std_logic                              -- 1 : debug mode, 0 : breakpoint exception
  );
end entity WardRV_fsm_trigger;

architecture rtl of WardRV_fsm_trigger is

  type     addrs_t   is array (0 to NB_TRIGGERS-1) of std_logic_vector(32-1 downto 0);

  signal   tselect_r : natural range 0 to NB_TRIGGERS-1;
  signal   dmode_r   : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal   m_r       : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal   execute_r : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal   store_r   : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal   load_r    : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal   tdata2_r  : addrs_t;

  signal   tdata1    : std_logic_vector(32-1 downto 0);
  signal   writable  : std_logic;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- CSR read
  -----------------------------------------------------------------------------
  tdata1 <= "0110"                   & -- type 6 : mcontrol6
            dmode_r(tselect_r)       &
            "00000000000"            & -- uncertain, hit1, vs, vu, hit0, select, size
            "000" & dmode_r(tselect_r) & -- action : 1 = debug mode
            '0'                      & -- chain
            "0000"                   & -- match : equal
            m_r(tselect_r)           &
            "000"                    & -- uncertainen, s, u
            execute_r(tselect_r)     &
            store_r(tselect_r)       &
            load_r(tselect_r);

  csr_rdata_o <= std_logic_vector(to_unsigned(tselect_r, 32)) when csr_addr_i = CSR_TSELECT else
                 tdata1                                        when csr_addr_i = CSR_TDATA1  else
                 tdata2_r(tselect_r)                           when csr_addr_i = CSR_TDATA2  else
                 x"01000040"                                   when csr_addr_i = CSR_TINFO   else -- version 1, mcontrol6
                 (others => '0');

  -----------------------------------------------------------------------------
  -- CSR write
  -----------------------------------------------------------------------------
  writable <= debug_mode_i or not dmode_r(tselect_r);

  process(clk_i, arst_b_i)
    variable v_dmode : std_logic;
  begin
    if arst_b_i = '0'
    then
      tselect_r <= 0;
      dmode_r   <= (others => '0');
      m_r       <= (others => '0');
      execute_r <= (others => '0');
      store_r   <= (others => '0');
      load_r    <= (others => '0');
      tdata2_r  <= (others => (others => '0'));
    elsif rising_edge(clk_i)
    then
      if csr_we_i = '1'
      then
        if csr_addr_i = CSR_TSELECT and unsigned(csr_wdata_i) < NB_TRIGGERS
        then
          -- An index out of range is ignored : the debugger counts the triggers
          tselect_r <= to_integer(unsigned(csr_wdata_i));
        elsif csr_addr_i = CSR_TDATA1 and writable = '1'
        then
          v_dmode              := csr_wdata_i(27) and debug_mode_i;
          dmode_r  (tselect_r) <= v_dmode;
          if v_dmode = '1' or EBREAK_EXCEPTION
          then
            m_r    (tselect_r) <= csr_wdata_i(6);
          else
            m_r    (tselect_r) <= '0';                            -- No breakpoint exception
          end if;
          execute_r(tselect_r) <= csr_wdata_i(2);
          store_r  (tselect_r) <= csr_wdata_i(1);
          load_r   (tselect_r) <= csr_wdata_i(0);
        elsif csr_addr_i = CSR_TDATA2 and writable = '1'
        then
          tdata2_r (tselect_r) <= csr_wdata_i;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Match, the lowest index has the priority
  -----------------------------------------------------------------------------
  process(all)
  begin
    hit_o       <= '0';
    hit_debug_o <= '0';
    for i in NB_TRIGGERS-1 downto 0 loop
      if debug_mode_i = '0' and m_r(i) = '1' and
         ((execute_r(i) = '1' and pc_i = tdata2_r(i)) or
          (mem_req_i = '1' and mem_addr_i = tdata2_r(i) and
           ((store_r(i) = '1' and mem_we_i = '1') or (load_r(i) = '1' and mem_we_i = '0'))))
      then
        hit_o       <= '1';
        hit_debug_o <= dmode_r(i);
      end if;
    end loop;
  end process;

end architecture rtl;
