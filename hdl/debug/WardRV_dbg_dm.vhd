-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Debug Module (Debug Spec 1.0 chapter 3), one hart, clk domain.
-- Execution-based : the hart executes the debug ROM, the abstract commands
-- and the program buffer from the debug memory (WardRV_dbg_dm_mem).
-- DMI registers (chapter 3.14), the other addresses read 0 :
--   data0..        : DATA_COUNT data registers
--   dmcontrol      : haltreq, resumereq, ackhavereset, set/clrresethaltreq
--                    (RESETHALTREQ_ENABLE), ndmreset, dmactive
--   dmstatus       : version 1.0, authenticated, halted / running / unavail,
--                    resumeack, havereset, impebreak (an ebreak always follows
--                    the program buffer), hasresethaltreq
--   hartinfo       : 1 dscratch, data registers in memory (dataaddr)
--   abstractcs     : progbufsize, busy, cmderr, datacount
--   command        : Access Register (3.7.1.1) of a GPR (0x1000-0x101F) or
--                    a CSR (0x0000-0x0FFF), aarsize 2, transfer, write,
--                    postexec. cmderr 2 : other cmdtype, aarsize,
--                    aarpostincrement, postexec without program buffer ;
--                    3 : register that does not exist ; 4 : hart not
--                    halted ; 1 : access during a command
--   abstractauto   : autoexecdata, autoexecprogbuf
--   progbuf0..     : PROGBUF_SIZE words
--   sbcs, sbaddress0, sbdata0 : System Bus Access (SBA_ENABLE)
--   haltsum0
-- The DMI read data is given in the cycle of the request.
-- dmactive = 0 resets the DM (not the hart state). The DM is reset by the
-- power-on reset arst_b_i only, never by ndmreset.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-- 2026-11-18  1.1      mrosiere impebreak always 1 (IMPEBREAK generic removed)
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;
use     asylum.techmap_pkg.all;

entity WardRV_dbg_dm is
  generic (
    PROGBUF_SIZE        : natural  := 2                           -- Program buffer (0..16 words)
   ;DATA_COUNT          : positive := 2                           -- Data registers (1..12)
   ;SBA_ENABLE          : boolean  := false                       -- System Bus Access
   ;RESETHALTREQ_ENABLE : boolean  := false                       -- Halt on reset request
  );
  port (
    -- Clock and Reset (power-on reset of the debug unit)
    clk_i               : in  std_logic
   ;arst_b_i            : in  std_logic
    -- DMI
   ;dmi_req_i           : in  dmi_req_t
   ;dmi_rdata_o         : out std_logic_vector(32-1 downto 0)
    -- Hart
   ;hart_rst_b_i        : in  std_logic                           -- Reset of the hart
   ;dbg_hart_req_o      : out dbg_hart_req_t
   ;dbg_mem_ini_i       : in  dmem_ini_t                          -- Debug memory
   ;dbg_mem_tgt_o       : out dmem_tgt_t
    -- SoC
   ;ndmreset_o          : out std_logic                           -- Reset of the system without the DM
   ;sba_dmem_ini_o      : out dmem_ini_t                          -- System Bus Access
   ;sba_dmem_tgt_i      : in  dmem_tgt_t := DMEM_TGT_NULL
  );
end entity WardRV_dbg_dm;

architecture rtl of WardRV_dbg_dm is

  -- Program buffer registers : at least one word (never selected when
  -- PROGBUF_SIZE = 0), ghdl --synth does not accept null arrays
  constant PROGBUF_WORDS     : positive := maximum(PROGBUF_SIZE, 1);

  -- hartinfo.dataaddr : x0-relative address of data0
  constant DATAADDR          : std_logic_vector(12-1 downto 0) := std_logic_vector(to_signed(DBG_DATA - 16#800#, 12));

  -- cmderr of a command : CMDERR_NONE if the DM can execute it
  function cmd_error(cmd : std_logic_vector(32-1 downto 0)) return std_logic_vector is
    constant REGNO : natural := to_integer(unsigned(cmd(15 downto 0)));
  begin
    if cmd(31 downto 24) /= x"00"                                 or -- cmdtype : Access Register only
       cmd(19) = '1'                                              or -- aarpostincrement
      (cmd(18) = '1' and PROGBUF_SIZE = 0)                        or -- postexec
      (cmd(17) = '1' and cmd(22 downto 20) /= "010")                 -- transfer of 32 bits only
    then
      return CMDERR_NOTSUP;
    elsif cmd(17) = '1' and REGNO > 16#101F#
    then
      return CMDERR_EXCEPTION;                                       -- Not a CSR nor a GPR
    end if;
    return CMDERR_NONE;
  end function cmd_error;

  function to_sl(b : boolean) return std_logic is
  begin
    if b
    then
      return '1';
    end if;
    return '0';
  end function to_sl;

  -- DMI
  signal   dmi_write         : std_logic;
  signal   dmi_addr          : natural range 0 to 2**DMI_ABITS-1;
  signal   data_sel          : boolean;                           -- data0..
  signal   progbuf_sel       : boolean;                           -- progbuf0..
  signal   abstract_access   : boolean;                           -- Not allowed during a command
  signal   data_idx          : natural range 0 to DATA_COUNT-1;
  signal   progbuf_idx       : natural range 0 to PROGBUF_WORDS-1;
  signal   sba_rdata         : std_logic_vector(32-1 downto 0);

  -- dmcontrol
  signal   dmactive_r        : std_logic;
  signal   ndmreset_r        : std_logic;
  signal   haltreq_r         : std_logic;
  signal   resethaltreq_r    : std_logic;

  -- Hart state
  signal   hart_rst_b        : std_logic;
  signal   havereset_r       : std_logic;
  signal   halted_r          : std_logic;                         -- Parked in the debug ROM
  signal   resumeack_r       : std_logic;
  signal   halted            : std_logic;
  signal   running           : std_logic;

  -- Abstract commands
  signal   go_r              : std_logic;                         -- Command in progress (abstractcs.busy)
  signal   resume_r          : std_logic;                         -- Resume requested
  signal   cmderr_r          : std_logic_vector(2 downto 0);
  signal   cmd_r             : std_logic_vector(32-1 downto 0);
  signal   autoexecdata_r    : std_logic_vector(DATA_COUNT-1 downto 0);
  signal   autoexecprogbuf_r : std_logic_vector(PROGBUF_WORDS-1 downto 0);
  signal   data_r            : dbg_words_t(0 to DATA_COUNT-1);
  signal   progbuf_r         : dbg_words_t(0 to PROGBUF_WORDS-1);

  -- Writes of the hart in the debug memory
  signal   mem_halted        : std_logic;
  signal   mem_resuming      : std_logic;
  signal   mem_data_we       : std_logic;
  signal   mem_data_idx      : natural range 0 to DATA_COUNT-1;
  signal   mem_data_wdata    : std_logic_vector(32-1 downto 0);

begin  -- architecture rtl

  assert PROGBUF_SIZE <= DBG_PROGBUF_MAX and DATA_COUNT <= DBG_DATA_MAX
    report "WardRV_dbg_dm : PROGBUF_SIZE 0..16, DATA_COUNT 1..12" severity failure;

  -----------------------------------------------------------------------------
  -- Hart state (hart reset synchronized)
  -----------------------------------------------------------------------------
  ins_hart_rst_sync : sync2dffrn
    port map
    (clk_i       => clk_i
    ,arst_b_i    => arst_b_i
    ,d_i         => hart_rst_b_i
    ,q_o         => hart_rst_b
    );

  halted  <=     halted_r and hart_rst_b;
  running <= not halted_r and hart_rst_b;

  -----------------------------------------------------------------------------
  -- DMI decode and read
  -----------------------------------------------------------------------------
  dmi_write       <= dmi_req_i.valid and dmi_req_i.we;
  dmi_addr        <= to_integer(unsigned(dmi_req_i.addr));
  data_sel        <= dmi_addr >= 16#04# and dmi_addr < 16#04# + DATA_COUNT;
  progbuf_sel     <= dmi_addr >= 16#20# and dmi_addr < 16#20# + PROGBUF_SIZE;
  data_idx        <= dmi_addr - 16#04# when dmi_addr >= 16#04# and dmi_addr < 16#04# + DATA_COUNT   else 0;
  progbuf_idx     <= dmi_addr - 16#20# when dmi_addr >= 16#20# and dmi_addr < 16#20# + PROGBUF_SIZE else 0;
  abstract_access <= data_sel or progbuf_sel or
                     (dmi_req_i.we = '1' and (dmi_req_i.addr = DM_COMMAND    or
                                              dmi_req_i.addr = DM_ABSTRACTCS or
                                              dmi_req_i.addr = DM_ABSTRACTAUTO));

  process(all)
  begin
    dmi_rdata_o <= (others => '0');
    if data_sel
    then
      dmi_rdata_o <= data_r(data_idx);
    elsif progbuf_sel
    then
      dmi_rdata_o <= progbuf_r(progbuf_idx);
    else
      case dmi_req_i.addr is
        when DM_DMCONTROL    =>
          dmi_rdata_o(1 downto 0) <= ndmreset_r & dmactive_r;
        when DM_DMSTATUS     =>
          dmi_rdata_o(22)           <= '1';                             -- impebreak
          dmi_rdata_o(19 downto 18) <= havereset_r    & havereset_r;    -- all / any
          dmi_rdata_o(17 downto 16) <= resumeack_r    & resumeack_r;
          dmi_rdata_o(13 downto 12) <= not hart_rst_b & not hart_rst_b; -- unavail
          dmi_rdata_o(11 downto 10) <= running        & running;
          dmi_rdata_o( 9 downto  8) <= halted         & halted;
          dmi_rdata_o(7)            <= '1';                             -- authenticated
          dmi_rdata_o(5)            <= to_sl(RESETHALTREQ_ENABLE);      -- hasresethaltreq
          dmi_rdata_o( 3 downto  0) <= "0011";                          -- version 1.0
        when DM_HARTINFO     =>
          dmi_rdata_o(23 downto 20) <= "0001";                          -- nscratch
          dmi_rdata_o(16)           <= '1';                             -- dataaccess : memory
          dmi_rdata_o(15 downto 12) <= std_logic_vector(to_unsigned(DATA_COUNT, 4));
          dmi_rdata_o(11 downto  0) <= DATAADDR;
        when DM_ABSTRACTCS   =>
          dmi_rdata_o(28 downto 24) <= std_logic_vector(to_unsigned(PROGBUF_SIZE, 5));
          dmi_rdata_o(12)           <= go_r;                            -- busy
          dmi_rdata_o(10 downto  8) <= cmderr_r;
          dmi_rdata_o( 3 downto  0) <= std_logic_vector(to_unsigned(DATA_COUNT, 4));
        when DM_ABSTRACTAUTO =>
          for i in 0 to PROGBUF_SIZE-1 loop
            dmi_rdata_o(16+i)       <= autoexecprogbuf_r(i);
          end loop;
          dmi_rdata_o(DATA_COUNT-1 downto 0) <= autoexecdata_r;
        when DM_HALTSUM0     =>
          dmi_rdata_o(0)            <= halted;
        when others          =>
          dmi_rdata_o               <= sba_rdata;
      end case;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- DM state
  -----------------------------------------------------------------------------
  process(clk_i, arst_b_i)

    -- Start an abstract command : the hart reads GO in the ROM
    procedure start(cmd : std_logic_vector(32-1 downto 0)) is
    begin
      if cmd_error(cmd) /= CMDERR_NONE
      then
        cmderr_r <= cmd_error(cmd);
      elsif halted = '0'
      then
        cmderr_r <= CMDERR_HALTRESUME;
      else
        go_r     <= '1';
      end if;
    end procedure;

  begin
    if arst_b_i = '0'
    then
      dmactive_r        <= '0';
      ndmreset_r        <= '0';
      haltreq_r         <= '0';
      resethaltreq_r    <= '0';
      havereset_r       <= '1';
      halted_r          <= '0';
      resumeack_r       <= '0';
      go_r              <= '0';
      resume_r          <= '0';
      cmderr_r          <= CMDERR_NONE;
      cmd_r             <= (others => '0');
      autoexecdata_r    <= (others => '0');
      autoexecprogbuf_r <= (others => '0');
      data_r            <= (others => (others => '0'));
      progbuf_r         <= (others => (others => '0'));
    elsif rising_edge(clk_i)
    then
      -------------------------------------------------------------------------
      -- Hart : debug memory writes, reset
      -------------------------------------------------------------------------
      if mem_data_we = '1'
      then
        data_r(mem_data_idx) <= mem_data_wdata;
      end if;

      if mem_halted = '1'
      then
        -- Parked in the ROM : debug mode entry or end of the command
        halted_r    <= '1';
        go_r        <= '0';
      end if;

      if mem_resuming = '1'
      then
        halted_r    <= '0';
        resume_r    <= '0';
        resumeack_r <= '1';
      end if;

      if hart_rst_b = '0'
      then
        -- The reset takes the hart out of debug mode
        havereset_r <= '1';
        halted_r    <= '0';
        go_r        <= '0';
        resume_r    <= '0';
      end if;

      -------------------------------------------------------------------------
      -- DMI
      -------------------------------------------------------------------------
      if dmi_write = '1' and dmi_req_i.addr = DM_DMCONTROL
      then
        dmactive_r <= dmi_req_i.wdata(0);
        if dmactive_r = '1'
        then
          ndmreset_r <= dmi_req_i.wdata(1);
          haltreq_r  <= dmi_req_i.wdata(31);
          -- resumereq : halted hart, no command, no haltreq
          if dmi_req_i.wdata(31 downto 30) = "01" and halted = '1' and go_r = '0'
          then
            resume_r    <= '1';
            resumeack_r <= '0';
          end if;
          if dmi_req_i.wdata(28) = '1'                            -- ackhavereset
          then
            havereset_r <= '0';
          end if;
          if RESETHALTREQ_ENABLE and dmi_req_i.wdata(3) = '1'     -- setresethaltreq
          then
            resethaltreq_r <= '1';
          elsif dmi_req_i.wdata(2) = '1'                          -- clrresethaltreq
          then
            resethaltreq_r <= '0';
          end if;
        end if;

      elsif dmi_req_i.valid = '1' and dmactive_r = '1' and abstract_access
      then
        if go_r = '1'
        then
          -- Access during a command
          if cmderr_r = CMDERR_NONE
          then
            cmderr_r <= CMDERR_BUSY;
          end if;
        elsif data_sel
        then
          if dmi_req_i.we = '1'
          then
            data_r(data_idx) <= dmi_req_i.wdata;
          end if;
          if autoexecdata_r(data_idx) = '1' and cmderr_r = CMDERR_NONE
          then
            start(cmd_r);
          end if;
        elsif progbuf_sel
        then
          if dmi_req_i.we = '1'
          then
            progbuf_r(progbuf_idx) <= dmi_req_i.wdata;
          end if;
          if autoexecprogbuf_r(progbuf_idx) = '1' and cmderr_r = CMDERR_NONE
          then
            start(cmd_r);
          end if;
        elsif dmi_req_i.addr = DM_COMMAND
        then
          if cmderr_r = CMDERR_NONE
          then
            cmd_r <= dmi_req_i.wdata;
            start(dmi_req_i.wdata);
          end if;
        elsif dmi_req_i.addr = DM_ABSTRACTCS
        then
          cmderr_r <= cmderr_r and not dmi_req_i.wdata(10 downto 8);  -- W1C
        else -- DM_ABSTRACTAUTO
          for i in 0 to PROGBUF_SIZE-1 loop
            autoexecprogbuf_r(i) <= dmi_req_i.wdata(16+i);
          end loop;
          autoexecdata_r    <= dmi_req_i.wdata(DATA_COUNT-1 downto 0);
        end if;
      end if;

      -------------------------------------------------------------------------
      -- dmactive = 0 : reset values, except the hart state
      -------------------------------------------------------------------------
      if dmactive_r = '0'
      then
        ndmreset_r        <= '0';
        haltreq_r         <= '0';
        resethaltreq_r    <= '0';
        go_r              <= '0';
        resume_r          <= '0';
        cmderr_r          <= CMDERR_NONE;
        cmd_r             <= (others => '0');
        autoexecdata_r    <= (others => '0');
        autoexecprogbuf_r <= (others => '0');
        data_r            <= (others => (others => '0'));
        progbuf_r         <= (others => (others => '0'));
      end if;
    end if;
  end process;

  dbg_hart_req_o.haltreq      <= haltreq_r;
  dbg_hart_req_o.resethaltreq <= resethaltreq_r;
  ndmreset_o                  <= ndmreset_r;

  -----------------------------------------------------------------------------
  -- Debug memory
  -----------------------------------------------------------------------------
  ins_dm_mem : entity work.WardRV_dbg_dm_mem
    generic map
    (PROGBUF_SIZE    => PROGBUF_SIZE
    ,DATA_COUNT      => DATA_COUNT
    )
    port map
    (dbg_mem_ini_i   => dbg_mem_ini_i
    ,dbg_mem_tgt_o   => dbg_mem_tgt_o
    ,cmd_i           => cmd_r
    ,progbuf_i       => progbuf_r
    ,data_i          => data_r
    ,go_i            => go_r
    ,resume_i        => resume_r
    ,halted_o        => mem_halted
    ,resuming_o      => mem_resuming
    ,data_we_o       => mem_data_we
    ,data_idx_o      => mem_data_idx
    ,data_wdata_o    => mem_data_wdata
    );

  -----------------------------------------------------------------------------
  -- System Bus Access
  -----------------------------------------------------------------------------
  gen_sba : if SBA_ENABLE
  generate
    ins_sba : entity work.WardRV_dbg_sba
      port map
      (clk_i           => clk_i
      ,arst_b_i        => arst_b_i
      ,dmactive_i      => dmactive_r
      ,dmi_req_i       => dmi_req_i
      ,dmi_rdata_o     => sba_rdata
      ,sba_dmem_ini_o  => sba_dmem_ini_o
      ,sba_dmem_tgt_i  => sba_dmem_tgt_i
      );
  end generate gen_sba;

  gen_sba_b : if not SBA_ENABLE
  generate
    sba_rdata      <= (others => '0');
    sba_dmem_ini_o <= (valid => '0', addr => (others => '0'), wdata => (others => '0'), we => '0', be => (others => '0'));
  end generate gen_sba_b;

end architecture rtl;
