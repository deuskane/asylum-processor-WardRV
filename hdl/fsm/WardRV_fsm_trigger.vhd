-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_trigger.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Trigger module (RISC-V Debug Spec 1.0, Sdtrig) : NB_TRIGGERS address match
-- triggers of type mcontrol6.
--  * CSRs : tselect (WARL 0..NB_TRIGGERS-1), tdata1, tdata2, tdata3 (0),
--           tinfo (version 1, type 6 only)
--  * mcontrol6 : match on the address of the executed instruction
--    (execute) or of a load / store (load, store), match = (0), >= (2) or
--    < (3), timing "before" (the instruction has no effect), m only.
--  * action (WARL) : 1 (enter debug mode) only with dmode=1 (the spec forbids
--    dmode=0 with action=1) ; 0 (breakpoint exception) only when
--    EBREAK_EXCEPTION is true. dmode=1 : action 1, or 0 if requested and
--    EBREAK_EXCEPTION. dmode=0 : action 0, and m is forced to 0 without
--    EBREAK_EXCEPTION (the trigger cannot fire).
--  * dmode is writable only in debug mode. A trigger with dmode=1 can only
--    be written in debug mode.
--  * Triggers never fire in debug mode.
-- hit_o is combinational and valid in the EXECUTE state of the FSM (address
-- of the load / store computed by the ALU).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere Review : no dmode=0 with action=1 (reset
--                               action = 0, WARL on the tdata1 write)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;

entity WardRV_fsm_trigger is
  generic (
    NB_TRIGGERS      : positive := 1;
    EBREAK_EXCEPTION : boolean  := false
  );
  port (
    clk_i               : in  std_logic;
    arst_b_i            : in  std_logic;

    debug_mode_i        : in  std_logic;

    -- CSR interface
    csr_addr_i          : in  std_logic_vector(11 downto 0);
    csr_we_i            : in  std_logic;
    csr_wdata_i         : in  std_logic_vector(31 downto 0);
    csr_rdata_o         : out std_logic_vector(31 downto 0);

    -- Match interface (valid in EXECUTE)
    pc_i                : in  std_logic_vector(31 downto 0);
    mem_req_i           : in  std_logic;
    mem_we_i            : in  std_logic;
    mem_addr_i          : in  std_logic_vector(31 downto 0);

    hit_o               : out std_logic;  -- A trigger fires (before the instruction)
    hit_debug_o         : out std_logic   -- Action of the trigger : 1 debug mode, 0 breakpoint exception
  );
end entity WardRV_fsm_trigger;

architecture rtl of WardRV_fsm_trigger is

  constant MATCH_EQ   : std_logic_vector(1 downto 0) := "00"; -- tdata1.match = 0
  constant MATCH_GE   : std_logic_vector(1 downto 0) := "10"; -- tdata1.match = 2
  constant MATCH_LT   : std_logic_vector(1 downto 0) := "11"; -- tdata1.match = 3

  constant TINFO      : std_logic_vector(31 downto 0) := x"01000040"; -- version 1, mcontrol6

  type addr_array_t  is array (0 to NB_TRIGGERS-1) of std_logic_vector(31 downto 0);
  type match_array_t is array (0 to NB_TRIGGERS-1) of std_logic_vector(1 downto 0);

  signal tselect_r    : natural range 0 to NB_TRIGGERS-1;
  signal dmode_r      : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal action_r     : std_logic_vector(NB_TRIGGERS-1 downto 0); -- 1 : debug mode
  signal match_r      : match_array_t;
  signal m_r          : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal execute_r    : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal store_r      : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal load_r       : std_logic_vector(NB_TRIGGERS-1 downto 0);
  signal tdata2_r     : addr_array_t;

  signal tdata1       : std_logic_vector(31 downto 0);
  signal wr_allowed   : std_logic;

  function addr_match(addr, tdata2 : std_logic_vector(31 downto 0);
                      match        : std_logic_vector(1 downto 0)) return boolean is
  begin
    case match is
      when MATCH_GE => return unsigned(addr) >= unsigned(tdata2);
      when MATCH_LT => return unsigned(addr) <  unsigned(tdata2);
      when others   => return addr = tdata2;
    end case;
  end function;

begin

  --------------------------------------------------------------------
  -- CSR read
  --------------------------------------------------------------------
  tdata1 <= "0110"                                           & -- type = 6 (mcontrol6)
            dmode_r(tselect_r)                               & -- dmode
            "00000000000"                                    & -- uncertain, hit1, vs, vu, hit0, select, 0, size
            "000" & action_r(tselect_r)                      & -- action
            '0'                                              & -- chain
            "00" & match_r(tselect_r)                        & -- match
            m_r(tselect_r)                                   & -- m
            "000"                                            & -- uncertainen, s, u
            execute_r(tselect_r)                             &
            store_r(tselect_r)                               &
            load_r(tselect_r);

  process(all)
  begin
    case csr_addr_i is
      when CSR_TSELECT => csr_rdata_o <= std_logic_vector(to_unsigned(tselect_r, 32));
      when CSR_TDATA1  => csr_rdata_o <= tdata1;
      when CSR_TDATA2  => csr_rdata_o <= tdata2_r(tselect_r);
      when CSR_TINFO   => csr_rdata_o <= TINFO;
      when others      => csr_rdata_o <= (others => '0');
    end case;
  end process;

  --------------------------------------------------------------------
  -- CSR write
  --------------------------------------------------------------------
  -- A trigger owned by debug mode (dmode=1) is writable only in debug mode
  wr_allowed <= debug_mode_i or not dmode_r(tselect_r);

  process(clk_i, arst_b_i)
    variable v_dmode  : std_logic;
    variable v_action : std_logic;
  begin
    if arst_b_i = '0'
    then
      tselect_r <= 0;
      dmode_r   <= (others => '0');
      action_r  <= (others => '0');
      match_r   <= (others => MATCH_EQ);
      m_r       <= (others => '0');
      execute_r <= (others => '0');
      store_r   <= (others => '0');
      load_r    <= (others => '0');
      tdata2_r  <= (others => (others => '0'));
    elsif rising_edge(clk_i)
    then
      if csr_we_i = '1'
      then
        case csr_addr_i is
          when CSR_TSELECT =>
            -- WARL : out of range index ignored (the debugger reads back to count the triggers)
            if unsigned(csr_wdata_i) < NB_TRIGGERS
            then
              tselect_r <= to_integer(unsigned(csr_wdata_i));
            end if;

          when CSR_TDATA1 =>
            if wr_allowed = '1'
            then
              -- dmode=0 with action=1 is not a legal value (WARL)
              v_dmode  := csr_wdata_i(27) and debug_mode_i;
              if v_dmode = '1' and not (csr_wdata_i(15 downto 12) = "0000" and EBREAK_EXCEPTION)
              then
                v_action := '1';
              else
                v_action := '0';
              end if;
              dmode_r  (tselect_r) <= v_dmode;
              action_r (tselect_r) <= v_action;
              case csr_wdata_i(10 downto 7) is
                when "0010" => match_r(tselect_r) <= MATCH_GE;
                when "0011" => match_r(tselect_r) <= MATCH_LT;
                when others => match_r(tselect_r) <= MATCH_EQ;
              end case;
              -- action 0 without EBREAK_EXCEPTION : the trigger is disabled
              if v_action = '1' or EBREAK_EXCEPTION
              then
                m_r    (tselect_r) <= csr_wdata_i(6);
              else
                m_r    (tselect_r) <= '0';
              end if;
              execute_r(tselect_r) <= csr_wdata_i(2);
              store_r  (tselect_r) <= csr_wdata_i(1);
              load_r   (tselect_r) <= csr_wdata_i(0);
            end if;

          when CSR_TDATA2 =>
            if wr_allowed = '1'
            then
              tdata2_r(tselect_r) <= csr_wdata_i;
            end if;

          when others =>
            null;
        end case;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Match (timing before), lowest index has the priority
  --------------------------------------------------------------------
  process(all)
    variable v_hit   : std_logic;
    variable v_debug : std_logic;
  begin
    v_hit   := '0';
    v_debug := '1';
    for i in NB_TRIGGERS-1 downto 0 loop
      if debug_mode_i = '0' and m_r(i) = '1' and
         ((execute_r(i) = '1' and addr_match(pc_i, tdata2_r(i), match_r(i))) or
          (mem_req_i = '1' and ((load_r (i) = '1' and mem_we_i = '0') or
                                (store_r(i) = '1' and mem_we_i = '1')) and
           addr_match(mem_addr_i, tdata2_r(i), match_r(i))))
      then
        v_hit   := '1';
        v_debug := action_r(i);
      end if;
    end loop;
    hit_o       <= v_hit;
    hit_debug_o <= v_debug;
  end process;

end architecture rtl;
