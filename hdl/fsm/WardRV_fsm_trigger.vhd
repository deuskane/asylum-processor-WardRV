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
--  * A trigger matches the address of the instruction (execute) or of a
--    load / store, before the instruction, in M-mode (m). Without
--    TRIGGER_RANGE_ENABLE the match is "equal" and there is no chain.
--  * TRIGGER_RANGE_ENABLE : match 2 (>=) and 3 (<), any other value is 0
--    (equal), and chain : a chain of triggers (chain = 1 on all but the
--    last one) fires only when all its triggers match, with the action of
--    its last trigger. Two triggers ">= A" (chain) and "< B" form the
--    range [A, B). chain stays 0 on the last trigger and when a trigger
--    of M-mode (dmode = 0) would chain into a trigger of the debugger ; a
--    write that sets dmode after a trigger of M-mode with chain is ignored.
--  * action is dmode : a trigger of the debugger (dmode = 1, written in
--    debug mode only) enters debug mode, a trigger of M-mode software
--    (dmode = 0) raises a breakpoint exception, which needs
--    EBREAK_EXCEPTION (otherwise its m bit stays 0 : it never fires).
--  * A trigger with dmode = 1 is writable in debug mode only. Triggers
--    never fire in debug mode. The hit bits are not implemented (0).
-- hit_o is valid in the EXECUTE state (load / store address on mem_addr_i).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-- 2026-11-18  1.1      mrosiere TRIGGER_RANGE_ENABLE : match >= / < and chain
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;

entity WardRV_fsm_trigger is
  generic (
    NB_TRIGGERS          : positive := 1
   ;EBREAK_EXCEPTION     : boolean  := false                      -- Action 0 (breakpoint exception) available
   ;TRIGGER_RANGE_ENABLE : boolean  := false                      -- Match >= / < and chain
  );
  port (
    clk_i                : in  std_logic
   ;arst_b_i             : in  std_logic
   ;debug_mode_i         : in  std_logic
    -- CSR
   ;csr_addr_i           : in  std_logic_vector(12-1 downto 0)
   ;csr_we_i             : in  std_logic
   ;csr_wdata_i          : in  std_logic_vector(32-1 downto 0)
   ;csr_rdata_o          : out std_logic_vector(32-1 downto 0)
    -- Instruction in EXECUTE
   ;pc_i                 : in  std_logic_vector(32-1 downto 0)
   ;mem_req_i            : in  std_logic
   ;mem_we_i             : in  std_logic
   ;mem_addr_i           : in  std_logic_vector(32-1 downto 0)
    -- A trigger fires (before the instruction), its action
   ;hit_o                : out std_logic
   ;hit_debug_o          : out std_logic                          -- 1 : debug mode, 0 : breakpoint exception
  );
end entity WardRV_fsm_trigger;

architecture rtl of WardRV_fsm_trigger is

  type     addrs_t   is array (0 to NB_TRIGGERS-1) of std_logic_vector(32-1 downto 0);
  type     matches_t is array (0 to NB_TRIGGERS-1) of std_logic_vector(2-1 downto 0);

  -- tdata1.match (bits 8:7, bits 10:9 are 0) : equal, >= or <
  constant MATCH_EQ  : std_logic_vector(2-1 downto 0) := "00";
  constant MATCH_GE  : std_logic_vector(2-1 downto 0) := "10";
  constant MATCH_LT  : std_logic_vector(2-1 downto 0) := "11";

  -- Address comparison of a trigger
  function addr_match(addr, tdata2 : std_logic_vector(32-1 downto 0);
                      match        : std_logic_vector(2-1 downto 0)) return boolean is
  begin
    case match is
      when MATCH_GE => return unsigned(addr) >= unsigned(tdata2);
      when MATCH_LT => return unsigned(addr) <  unsigned(tdata2);
      when others   => return addr = tdata2;
    end case;
  end function addr_match;

  signal   tselect_r  : natural range 0 to NB_TRIGGERS-1;
  signal   dmode_r    : std_logic_vector(0 to NB_TRIGGERS-1);
  signal   m_r        : std_logic_vector(0 to NB_TRIGGERS-1);
  signal   execute_r  : std_logic_vector(0 to NB_TRIGGERS-1);
  signal   store_r    : std_logic_vector(0 to NB_TRIGGERS-1);
  signal   load_r     : std_logic_vector(0 to NB_TRIGGERS-1);
  signal   tdata2_r   : addrs_t;
  signal   match_r    : matches_t;                                -- TRIGGER_RANGE_ENABLE
  signal   chain_r    : std_logic_vector(0 to NB_TRIGGERS-1);     -- TRIGGER_RANGE_ENABLE

  signal   tdata1     : std_logic_vector(32-1 downto 0);
  signal   wr_trigger : std_logic;                                -- Write of tdata1 / tdata2 allowed
  signal   wr_tdata1  : std_logic;                                -- Write of tdata1 allowed
  signal   wr_dmode   : std_logic;                                -- dmode written in tdata1

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- CSR read
  -----------------------------------------------------------------------------
  tdata1 <= "0110"                     & -- type 6 : mcontrol6
            dmode_r(tselect_r)         &
            "00000000000"              & -- uncertain, hit1, vs, vu, hit0, select, size
            "000" & dmode_r(tselect_r) & -- action : 1 = debug mode
            chain_r(tselect_r)         &
            "00" & match_r(tselect_r)  &
            m_r(tselect_r)             &
            "000"                      & -- uncertainen, s, u
            execute_r(tselect_r)       &
            store_r(tselect_r)         &
            load_r(tselect_r);

  csr_rdata_o <= std_logic_vector(to_unsigned(tselect_r, 32)) when csr_addr_i = CSR_TSELECT else
                 tdata1                                        when csr_addr_i = CSR_TDATA1  else
                 tdata2_r(tselect_r)                           when csr_addr_i = CSR_TDATA2  else
                 x"01000040"                                   when csr_addr_i = CSR_TINFO   else -- version 1, mcontrol6
                 (others => '0');

  -----------------------------------------------------------------------------
  -- CSR write
  -----------------------------------------------------------------------------
  -- A trigger of the debugger (dmode = 1) is writable in debug mode only ;
  -- dmode is not set after a trigger of M-mode chained to this one
  wr_dmode   <= csr_wdata_i(27) and debug_mode_i;
  wr_trigger <= '0' when dmode_r(tselect_r) = '1' and debug_mode_i = '0'                  else
                '1';
  wr_tdata1  <= '0' when tselect_r > 0 and wr_dmode = '1' and chain_r(maximum(tselect_r-1, 0)) = '1' and
                         dmode_r(maximum(tselect_r-1, 0)) = '0'                           else
                wr_trigger;

  process(clk_i, arst_b_i)
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
        elsif csr_addr_i = CSR_TDATA1 and wr_tdata1 = '1'
        then
          dmode_r  (tselect_r) <= wr_dmode;
          if wr_dmode = '1' or EBREAK_EXCEPTION
          then
            m_r    (tselect_r) <= csr_wdata_i(6);
          else
            m_r    (tselect_r) <= '0';                            -- No breakpoint exception
          end if;
          execute_r(tselect_r) <= csr_wdata_i(2);
          store_r  (tselect_r) <= csr_wdata_i(1);
          load_r   (tselect_r) <= csr_wdata_i(0);
        elsif csr_addr_i = CSR_TDATA2 and wr_trigger = '1'
        then
          tdata2_r (tselect_r) <= csr_wdata_i;
        end if;
      end if;
    end if;
  end process;

  -- match and chain : registers held in their reset value (equal, no
  -- chain) without TRIGGER_RANGE_ENABLE
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' or not TRIGGER_RANGE_ENABLE
    then
      match_r   <= (others => MATCH_EQ);
      chain_r   <= (others => '0');
    elsif rising_edge(clk_i)
    then
      if csr_we_i = '1' and csr_addr_i = CSR_TDATA1 and wr_tdata1 = '1'
      then
        -- match 2 or 3 (bits 10:7 = "001x"), else equal
        if csr_wdata_i(10 downto 8) = "001"
        then
          match_r(tselect_r) <= csr_wdata_i(8 downto 7);
        else
          match_r(tselect_r) <= MATCH_EQ;
        end if;
        -- No chain from the last trigger, nor from a trigger of M-mode into
        -- a trigger of the debugger
        if tselect_r < NB_TRIGGERS-1 and (wr_dmode = '1' or dmode_r(minimum(tselect_r+1, NB_TRIGGERS-1)) = '0')
        then
          chain_r(tselect_r) <= csr_wdata_i(11);
        else
          chain_r(tselect_r) <= '0';
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Match : a chain fires when all its triggers match, the lowest index
  -- has the priority
  -----------------------------------------------------------------------------
  process(all)
    variable v_match : boolean;
    variable v_chain : boolean;                                   -- The triggers chained before match
    variable v_hit   : boolean;
  begin
    hit_o       <= '0';
    hit_debug_o <= '0';
    v_chain     := true;
    v_hit       := false;
    for i in 0 to NB_TRIGGERS-1 loop
      v_match := debug_mode_i = '0' and m_r(i) = '1' and
                 ((execute_r(i) = '1' and addr_match(pc_i, tdata2_r(i), match_r(i))) or
                  (mem_req_i = '1' and addr_match(mem_addr_i, tdata2_r(i), match_r(i)) and
                   ((store_r(i) = '1' and mem_we_i = '1') or (load_r(i) = '1' and mem_we_i = '0'))));
      if chain_r(i) = '1'
      then
        v_chain := v_chain and v_match;
      else
        if v_chain and v_match and not v_hit
        then
          hit_o       <= '1';
          hit_debug_o <= dmode_r(i);
          v_hit       := true;
        end if;
        v_chain := true;
      end if;
    end loop;
  end process;

end architecture rtl;
