-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dm.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Debug Module (RISC-V Debug Spec 1.0), clk domain, one hart, execution
-- based : the hart executes the debug ROM, the abstract commands and the
-- program buffer from the debug memory (WardRV_dbg_dm_mem, SBI target).
-- DMI registers :
--   0x04-0x0F data0..DATA_COUNT-1 (abstractauto)
--   0x10 dmcontrol  : haltreq, resumereq, ackhavereset, ackunavail,
--                     set/clrresethaltreq (RESETHALTREQ_ENABLE),
--                     set/clrkeepalive (no effect), ndmreset, dmactive.
--                     hartsel (hartsellen=0), hartreset and hasel WARL 0
--   0x11 dmstatus   : version=3, authenticated=1, impebreak,
--                     hasresethaltreq, all/any halted, running, resumeack,
--                     havereset, unavail, nonexistent
--   0x12 hartinfo   : nscratch=2, dataaccess=1, datasize, dataaddr
--   0x16 abstractcs : progbufsize, busy, cmderr (W1C), datacount
--   0x17 command    : Access Register (cmdtype 0) : aarsize <= 2 (32-bit
--                     access), GPR 0x1000-0x101F, CSR 0x0000-0x0FFF,
--                     transfer, write, postexec, aarpostincrement.
--                     Other cmdtype / aarsize, postexec without program
--                     buffer : cmderr=2. regno that does not exist :
--                     cmderr=3. Hart not halted : cmderr=4. Exception : 3.
--                     Access to command / abstractcs / abstractauto /
--                     data / progbuf while busy : cmderr=1 once the
--                     command completes.
--   0x18 abstractauto : autoexecdata, autoexecprogbuf
--   0x1D nextdm = 0
--   0x20-0x2F progbuf0..PROGBUF_SIZE-1
--   0x38-0x3D sbcs, sbaddress0, sbdata0 (SBA_ENABLE, WardRV_dbg_sba)
--   0x40 haltsum0
-- Any other address reads 0. The DMI response is given in the request
-- cycle (ready = valid = request valid).
-- dmactive=0 holds the DM in reset (except dmactive). The DM is not reset by
-- ndmreset. havereset / unavail come from the hart reset (hart_rst_b_i).
-- haltreq, resumereq, ackhavereset and set/clrresethaltreq writes are
-- ignored while an abstract command is executing.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere Review (Debug Spec 1.0) : regno that does
--                               not exist -> cmderr 3, postexec without
--                               program buffer -> cmderr 2, busy error
--                               reported when the command completes,
--                               dmcontrol hart requests ignored while busy
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;
use     asylum.sbi_pkg.all;
use     asylum.WardRV_debug_pkg.all;
use     asylum.techmap_pkg.all;

entity WardRV_dbg_dm is
  generic (
    PROGBUF_SIZE        : natural                       := 2;      -- 0..16 words
    DATA_COUNT          : natural                       := 2;      -- 1..12 words
    IMPEBREAK           : boolean                       := true;   -- Implicit ebreak after the program buffer
    SBA_ENABLE          : boolean                       := false;  -- System Bus Access
    SBA_ADDR_WIDTH      : positive                      := 8;      -- SoC SBI address width
    RESETHALTREQ_ENABLE : boolean                       := false;  -- Halt on reset request
    HARTID              : std_logic_vector(31 downto 0) := (others => '0') -- Reserved (one hart)
  );
  port (
    clk_i          : in  std_logic;
    arst_b_i       : in  std_logic;   -- Power-on reset of the debug unit (never ndmreset)

    -- DMI (from WardRV_dbg_jtag or any other DTM)
    dmi_req_i      : in  dmi_req_t;
    dmi_rsp_o      : out dmi_rsp_t;

    -- Hart
    hart_rst_b_i   : in  std_logic;   -- Reset of the hart (active low) : havereset, unavail
    dbg_hart_req_o : out dbg_hart_req_t;
    dbg_sbi_ini_i  : in  dbg_sbi_ini_t;
    dbg_sbi_tgt_o  : out dbg_sbi_tgt_t;

    -- SoC
    ndmreset_o     : out std_logic;   -- Non-debug-module reset request

    -- System Bus Access (SBA_ENABLE)
    sba_sbi_ini_o  : out sbi_ini_t(addr (SBA_ADDR_WIDTH-1 downto 0),
                                   wdata(7 downto 0));
    sba_sbi_tgt_i  : in  sbi_tgt_t(rdata(7 downto 0)) := (ready => '0', rdata => (others => '0'), info => (name => (others => ' ')))
  );
end entity WardRV_dbg_dm;

architecture rtl of WardRV_dbg_dm is

  -- dmstatus.version = 3 : Debug Spec 1.0
  constant DM_VERSION    : std_logic_vector(3 downto 0) := "0011";

  -- x0-relative address of data0 (hartinfo.dataaddr, abstract commands)
  constant DATA0_IMM     : integer := dbg_imm(DBG_OFFSET_DATA);

  function to_sl(b : boolean) return std_logic is
  begin
    if b then return '1'; else return '0'; end if;
  end function;

  signal dmi_we          : std_logic;
  signal dmi_re          : std_logic;
  signal dmi_addr        : unsigned(DMI_ABITS-1 downto 0);
  signal dmi_rdata       : std_logic_vector(31 downto 0);
  signal sba_rdata       : std_logic_vector(31 downto 0);

  -- dmcontrol
  signal dmactive_r      : std_logic;
  signal ndmreset_r      : std_logic;
  signal haltreq_r       : std_logic;
  signal resethaltreq_r  : std_logic;

  -- Hart status
  signal hart_rst_b      : std_logic;
  signal havereset_r     : std_logic;
  signal halted_r        : std_logic;
  signal resumeack_r     : std_logic;
  signal hart_halted     : std_logic;
  signal hart_running    : std_logic;
  signal hart_unavail    : std_logic;

  -- Abstract commands
  signal flag_go_r       : std_logic;  -- Command pending, the hart must execute it
  signal flag_resume_r   : std_logic;  -- Resume request pending
  signal cmd_exec_r      : std_logic;  -- Command executed by the hart
  signal busy            : std_logic;
  signal cmderr_r        : std_logic_vector(2 downto 0);
  signal busyerr_r       : std_logic;  -- Access while busy, cmderr=1 when the command completes
  signal cmd_r           : std_logic_vector(31 downto 0);
  signal autoexecdata_r  : std_logic_vector(DBG_DATA_MAX-1 downto 0);
  signal autoexecpbuf_r  : std_logic_vector(DBG_PROGBUF_MAX-1 downto 0);
  signal data_r          : dbg_words_t(0 to DBG_DATA_MAX-1);
  signal progbuf_r       : dbg_words_t(0 to DBG_PROGBUF_MAX-1);
  signal abstract        : dbg_words_t(0 to DBG_ABSTRACT_SIZE-1);

  -- Debug memory (core writes)
  signal mem_halted      : std_logic;
  signal mem_going       : std_logic;
  signal mem_resuming    : std_logic;
  signal mem_exception   : std_logic;
  signal mem_data_we     : std_logic;
  signal mem_data_idx    : natural range 0 to DBG_DATA_MAX-1;
  signal mem_data_wdata  : std_logic_vector(31 downto 0);

  signal dmstatus        : std_logic_vector(31 downto 0);
  signal hartinfo        : std_logic_vector(31 downto 0);
  signal abstractcs      : std_logic_vector(31 downto 0);

  -- Abstract command program (Access Register)
  --   slots 0..3 : transfer (GPR : sw/lw xN, data0(x0) ;
  --                CSR : s0 saved in dscratch1, csrr/csrw through s0)
  --   slot  4    : jal PROGBUF (postexec) or ebreak
  --   slots 5..7 : ebreak
  function abstract_program(cmd : std_logic_vector(31 downto 0)) return dbg_words_t is
    variable p     : dbg_words_t(0 to DBG_ABSTRACT_SIZE-1) := (others => RV_EBREAK);
    variable regno : std_logic_vector(15 downto 0);
    variable gpr   : std_logic_vector(4 downto 0);
    variable csr   : std_logic_vector(11 downto 0);
  begin
    regno := cmd(15 downto 0);
    gpr   := regno(4 downto 0);
    csr   := regno(11 downto 0);
    p(0 to 3) := (others => RV_NOP);
    if cmd(17) = '1' -- transfer
    then
      if regno(15 downto 12) = x"1"
      then
        -- GPR
        if cmd(16) = '1' then p(0) := rv_lw(gpr, RV_X0, DATA0_IMM);
        else                  p(0) := rv_sw(gpr, RV_X0, DATA0_IMM);
        end if;
      else
        -- CSR
        p(0) := rv_csrrw(RV_X0, CSR_DSCRATCH1, RV_S0);
        if cmd(16) = '1'
        then
          p(1) := rv_lw   (RV_S0, RV_X0, DATA0_IMM);
          p(2) := rv_csrrw(RV_X0, csr  , RV_S0);
        else
          p(1) := rv_csrrs(RV_S0, csr  , RV_X0);
          p(2) := rv_sw   (RV_S0, RV_X0, DATA0_IMM);
        end if;
        p(3) := rv_csrrs(RV_S0, CSR_DSCRATCH1, RV_X0);
      end if;
    end if;
    if cmd(18) = '1' -- postexec
    then
      p(4) := rv_jal(RV_X0, DBG_OFFSET_PROGBUF - (DBG_OFFSET_ABSTRACT + 16));
    end if;
    return p;
  end function;

  -- Command checks : 0 ok, otherwise cmderr
  function cmd_check(cmd : std_logic_vector(31 downto 0)) return std_logic_vector is
    variable regno : unsigned(15 downto 0);
  begin
    regno := unsigned(cmd(15 downto 0));
    if cmd(31 downto 24) /= x"00"                    -- Access Register only
    then
      return CMDERR_NOTSUP;
    end if;
    if cmd(18) = '1' and PROGBUF_SIZE = 0            -- postexec without program buffer
    then
      return CMDERR_NOTSUP;
    end if;
    if cmd(17) = '1'                                 -- transfer
    then
      if unsigned(cmd(22 downto 20)) > 2             -- aarsize > 32 bits
      then
        return CMDERR_NOTSUP;
      end if;
      if not (regno <= 16#0FFF# or (regno >= 16#1000# and regno <= 16#101F#))
      then
        return CMDERR_EXCEPTION;                     -- the register does not exist
      end if;
    end if;
    return CMDERR_NONE;
  end function;

begin

  assert PROGBUF_SIZE /= 1 or IMPEBREAK
    report "WardRV_dbg_dm : PROGBUF_SIZE = 1 requires IMPEBREAK" severity failure;

  --------------------------------------------------------------------
  -- Hart reset (synchronized)
  --------------------------------------------------------------------
  ins_hart_rst_sync : sync2dffrn
    port map (
      clk_i    => clk_i,
      arst_b_i => arst_b_i,
      d_i      => hart_rst_b_i,
      q_o      => hart_rst_b
    );

  hart_unavail <= not hart_rst_b;
  hart_halted  <=     halted_r and hart_rst_b;
  hart_running <= not halted_r and hart_rst_b;

  --------------------------------------------------------------------
  -- DMI
  --------------------------------------------------------------------
  dmi_we   <= dmi_req_i.valid when dmi_req_i.op = DMI_OP_WRITE else '0';
  dmi_re   <= dmi_req_i.valid when dmi_req_i.op = DMI_OP_READ  else '0';
  dmi_addr <= unsigned(dmi_req_i.addr);

  dmi_rsp_o.ready <= dmi_req_i.valid;
  dmi_rsp_o.valid <= dmi_req_i.valid;
  dmi_rsp_o.op    <= DMI_RSP_SUCCESS;
  dmi_rsp_o.data  <= dmi_rdata;

  busy <= flag_go_r or cmd_exec_r;

  dmstatus   <= (31 downto 25 => '0')          &
                '0'                            & -- ndmresetpending
                '0'                            & -- stickyunavail
                to_sl(IMPEBREAK)               & -- impebreak
                "00"                           &
                havereset_r  & havereset_r     & -- allhavereset, anyhavereset
                resumeack_r  & resumeack_r     & -- allresumeack, anyresumeack
                '0'          & '0'             & -- allnonexistent, anynonexistent
                hart_unavail & hart_unavail    & -- allunavail, anyunavail
                hart_running & hart_running    & -- allrunning, anyrunning
                hart_halted  & hart_halted     & -- allhalted, anyhalted
                '1'                            & -- authenticated
                '0'                            & -- authbusy
                to_sl(RESETHALTREQ_ENABLE)     & -- hasresethaltreq
                '0'                            & -- confstrptrvalid
                DM_VERSION;

  hartinfo   <= (31 downto 24 => '0')          &
                "0010"                         & -- nscratch = 2 (dscratch0/1 free during progbuf)
                "000"                          &
                '1'                            & -- dataaccess : data registers in memory
                std_logic_vector(to_unsigned(DATA_COUNT, 4)) & -- datasize
                std_logic_vector(to_signed(DATA0_IMM, 12));    -- dataaddr

  abstractcs <= "000"                          &
                std_logic_vector(to_unsigned(PROGBUF_SIZE, 5)) &
                (23 downto 13 => '0')          &
                busy                           &
                '0'                            & -- relaxedpriv
                cmderr_r                       &
                "0000"                         &
                std_logic_vector(to_unsigned(DATA_COUNT, 4));

  process(all)
    variable v_idx : natural;
  begin
    dmi_rdata <= (others => '0');
    if    dmi_addr >= unsigned(DM_DATA0) and dmi_addr < unsigned(DM_DATA0) + DATA_COUNT
    then
      v_idx     := to_integer(dmi_addr - unsigned(DM_DATA0));
      dmi_rdata <= data_r(v_idx);
    elsif dmi_addr >= unsigned(DM_PROGBUF0) and dmi_addr < unsigned(DM_PROGBUF0) + PROGBUF_SIZE
    then
      v_idx     := to_integer(dmi_addr - unsigned(DM_PROGBUF0));
      dmi_rdata <= progbuf_r(v_idx);
    else
      case dmi_req_i.addr is
        when DM_DMCONTROL    =>
          dmi_rdata(0)  <= dmactive_r;
          dmi_rdata(1)  <= ndmreset_r;
        when DM_DMSTATUS     => dmi_rdata <= dmstatus;
        when DM_HARTINFO     => dmi_rdata <= hartinfo;
        when DM_ABSTRACTCS   => dmi_rdata <= abstractcs;
        when DM_ABSTRACTAUTO =>
          dmi_rdata(16+PROGBUF_SIZE-1 downto 16) <= autoexecpbuf_r(PROGBUF_SIZE-1 downto 0);
          dmi_rdata(DATA_COUNT-1      downto  0) <= autoexecdata_r(DATA_COUNT-1   downto 0);
        when DM_HALTSUM0     => dmi_rdata(0) <= hart_halted;
        when DM_SBCS | DM_SBADDRESS0 | DM_SBDATA0 =>
          if SBA_ENABLE then
            dmi_rdata <= sba_rdata;
          end if;
        when others          => null;
      end case;
    end if;
  end process;

  --------------------------------------------------------------------
  -- DM state
  --------------------------------------------------------------------
  process(clk_i, arst_b_i)

    -- Start an abstract command (not busy, cmderr = 0)
    procedure execute(cmd : std_logic_vector(31 downto 0)) is
      variable v_err : std_logic_vector(2 downto 0);
    begin
      v_err := cmd_check(cmd);
      if v_err /= CMDERR_NONE
      then
        cmderr_r  <= v_err;
      elsif hart_halted = '0'
      then
        cmderr_r  <= CMDERR_HALTRESUME;
      else
        flag_go_r <= '1';
      end if;
    end procedure;

    variable v_idx     : natural;
    variable v_autoexec: boolean;
    variable v_busyerr : boolean;  -- Access while busy (pending or in this cycle)
    variable v_cmd_end : boolean;  -- The abstract command ends in this cycle
  begin
    if arst_b_i = '0'
    then
      dmactive_r     <= '0';
      ndmreset_r     <= '0';
      haltreq_r      <= '0';
      resethaltreq_r <= '0';
      havereset_r    <= '1';
      halted_r       <= '0';
      resumeack_r    <= '0';
      flag_go_r      <= '0';
      flag_resume_r  <= '0';
      cmd_exec_r     <= '0';
      cmderr_r       <= CMDERR_NONE;
      busyerr_r      <= '0';
      cmd_r          <= (others => '0');
      autoexecdata_r <= (others => '0');
      autoexecpbuf_r <= (others => '0');
      data_r         <= (others => (others => '0'));
      progbuf_r      <= (others => (others => '0'));
    elsif rising_edge(clk_i)
    then
      v_autoexec := false;
      v_busyerr  := busyerr_r = '1';
      v_cmd_end  := false;

      ------------------------------------------------------------
      -- Hart (debug memory writes)
      ------------------------------------------------------------
      if mem_data_we = '1'
      then
        data_r(mem_data_idx) <= mem_data_wdata;
      end if;

      if mem_going = '1'
      then
        flag_go_r  <= '0';
        cmd_exec_r <= '1';
      end if;

      if mem_exception = '1' and cmd_exec_r = '1' and cmderr_r = CMDERR_NONE
      then
        cmderr_r <= CMDERR_EXCEPTION;
      end if;

      if mem_halted = '1'
      then
        halted_r <= '1';
        if cmd_exec_r = '1'
        then
          -- End of the abstract command
          cmd_exec_r <= '0';
          v_cmd_end  := true;
          if cmd_r(19) = '1' and cmd_r(17) = '1' and cmderr_r = CMDERR_NONE and mem_exception = '0'
          then
            -- aarpostincrement
            cmd_r(15 downto 0) <= std_logic_vector(unsigned(cmd_r(15 downto 0)) + 1);
          end if;
        end if;
      end if;

      if mem_resuming = '1'
      then
        halted_r      <= '0';
        resumeack_r   <= '1';
        flag_resume_r <= '0';
      end if;

      ------------------------------------------------------------
      -- Hart reset : the hart leaves debug mode
      ------------------------------------------------------------
      if hart_rst_b = '0'
      then
        if busy = '1' then
          v_cmd_end   := true;   -- command abandoned
        end if;
        havereset_r   <= '1';
        halted_r      <= '0';
        flag_go_r     <= '0';
        flag_resume_r <= '0';
        cmd_exec_r    <= '0';
      end if;

      ------------------------------------------------------------
      -- DMI
      ------------------------------------------------------------
      if dmi_we = '1' and dmi_req_i.addr = DM_DMCONTROL
      then
        dmactive_r <= dmi_req_i.data(0);
        if dmi_req_i.data(0) = '1'
        then
          ndmreset_r <= dmi_req_i.data(1);
          -- Hart requests : ignored while an abstract command is executing
          if busy = '0'
          then
            haltreq_r  <= dmi_req_i.data(31);
            -- resumereq, ignored with haltreq, only for a halted hart
            if dmi_req_i.data(30) = '1' and dmi_req_i.data(31) = '0' and hart_halted = '1'
            then
              resumeack_r   <= '0';
              flag_resume_r <= '1';
            end if;
            -- ackhavereset
            if dmi_req_i.data(28) = '1' and hart_rst_b = '1'
            then
              havereset_r <= '0';
            end if;
            -- setresethaltreq / clrresethaltreq
            if RESETHALTREQ_ENABLE
            then
              if    dmi_req_i.data(2) = '1' then resethaltreq_r <= '0';
              elsif dmi_req_i.data(3) = '1' then resethaltreq_r <= '1';
              end if;
            end if;
          end if;
        end if;

      elsif dmactive_r = '1' and (dmi_we = '1' or dmi_re = '1')
      then
        if    dmi_addr >= unsigned(DM_DATA0) and dmi_addr < unsigned(DM_DATA0) + DATA_COUNT
        then
          v_idx := to_integer(dmi_addr - unsigned(DM_DATA0));
          if busy = '1'
          then
            v_busyerr := true;
          else
            if dmi_we = '1' then
              data_r(v_idx) <= dmi_req_i.data;
            end if;
            v_autoexec := autoexecdata_r(v_idx) = '1';
          end if;

        elsif dmi_addr >= unsigned(DM_PROGBUF0) and dmi_addr < unsigned(DM_PROGBUF0) + PROGBUF_SIZE
        then
          v_idx := to_integer(dmi_addr - unsigned(DM_PROGBUF0));
          if busy = '1'
          then
            v_busyerr := true;
          else
            if dmi_we = '1' then
              progbuf_r(v_idx) <= dmi_req_i.data;
            end if;
            v_autoexec := autoexecpbuf_r(v_idx) = '1';
          end if;

        elsif dmi_we = '1'
        then
          case dmi_req_i.addr is
            when DM_ABSTRACTCS =>
              if busy = '1'
              then
                v_busyerr := true;
              else
                cmderr_r <= cmderr_r and not dmi_req_i.data(10 downto 8);  -- W1C
              end if;

            when DM_COMMAND =>
              if busy = '1'
              then
                v_busyerr := true;
              elsif cmderr_r = CMDERR_NONE
              then
                cmd_r <= dmi_req_i.data;
                execute(dmi_req_i.data);
              end if;

            when DM_ABSTRACTAUTO =>
              if busy = '1'
              then
                v_busyerr := true;
              else
                autoexecpbuf_r <= (others => '0');
                autoexecdata_r <= (others => '0');
                autoexecpbuf_r(PROGBUF_SIZE-1 downto 0) <= dmi_req_i.data(16+PROGBUF_SIZE-1 downto 16);
                autoexecdata_r(DATA_COUNT-1   downto 0) <= dmi_req_i.data(DATA_COUNT-1      downto  0);
              end if;

            when others => null;
          end case;
        end if;

        -- abstractauto : execute the last command again
        if v_autoexec and cmderr_r = CMDERR_NONE
        then
          execute(cmd_r);
        end if;
      end if;

      ------------------------------------------------------------
      -- Access while busy : cmderr=1 once the command completes
      -- (only if cmderr is still 0)
      ------------------------------------------------------------
      if v_cmd_end
      then
        if v_busyerr and cmderr_r = CMDERR_NONE
        then
          cmderr_r <= CMDERR_BUSY;
        end if;
        busyerr_r <= '0';
      elsif v_busyerr
      then
        busyerr_r <= '1';
      end if;

      ------------------------------------------------------------
      -- dmactive = 0 : DM in reset (the hart state is kept)
      ------------------------------------------------------------
      if (dmactive_r = '0' and not (dmi_we = '1' and dmi_req_i.addr = DM_DMCONTROL and dmi_req_i.data(0) = '1')) or
         (dmi_we = '1' and dmi_req_i.addr = DM_DMCONTROL and dmi_req_i.data(0) = '0')
      then
        ndmreset_r     <= '0';
        haltreq_r      <= '0';
        resethaltreq_r <= '0';
        flag_go_r      <= '0';
        flag_resume_r  <= '0';
        cmd_exec_r     <= '0';
        cmderr_r       <= CMDERR_NONE;
        busyerr_r      <= '0';
        cmd_r          <= (others => '0');
        autoexecdata_r <= (others => '0');
        autoexecpbuf_r <= (others => '0');
        data_r         <= (others => (others => '0'));
        progbuf_r      <= (others => (others => '0'));
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Outputs
  --------------------------------------------------------------------
  dbg_hart_req_o.haltreq      <= haltreq_r      and dmactive_r;
  dbg_hart_req_o.resethaltreq <= resethaltreq_r and dmactive_r when RESETHALTREQ_ENABLE else '0';
  ndmreset_o                  <= ndmreset_r;

  --------------------------------------------------------------------
  -- Debug memory
  --------------------------------------------------------------------
  abstract <= abstract_program(cmd_r);

  ins_dm_mem : entity work.WardRV_dbg_dm_mem
    generic map (
      PROGBUF_SIZE  => PROGBUF_SIZE,
      DATA_COUNT    => DATA_COUNT,
      IMPEBREAK     => IMPEBREAK
    )
    port map (
      dbg_sbi_ini_i => dbg_sbi_ini_i,
      dbg_sbi_tgt_o => dbg_sbi_tgt_o,
      abstract_i    => abstract,
      progbuf_i     => progbuf_r,
      data_i        => data_r,
      flag_go_i     => flag_go_r,
      flag_resume_i => flag_resume_r,
      halted_o      => mem_halted,
      going_o       => mem_going,
      resuming_o    => mem_resuming,
      exception_o   => mem_exception,
      data_we_o     => mem_data_we,
      data_idx_o    => mem_data_idx,
      data_wdata_o  => mem_data_wdata
    );

  --------------------------------------------------------------------
  -- System Bus Access
  --------------------------------------------------------------------
  gen_sba : if SBA_ENABLE generate
    ins_sba : entity work.WardRV_dbg_sba
      generic map (
        SBA_ADDR_WIDTH => SBA_ADDR_WIDTH
      )
      port map (
        clk_i         => clk_i,
        arst_b_i      => arst_b_i,
        dmactive_i    => dmactive_r,
        dmi_we_i      => dmi_we,
        dmi_re_i      => dmi_re,
        dmi_addr_i    => dmi_req_i.addr,
        dmi_wdata_i   => dmi_req_i.data,
        dmi_rdata_o   => sba_rdata,
        sba_sbi_ini_o => sba_sbi_ini_o,
        sba_sbi_tgt_i => sba_sbi_tgt_i
      );
  end generate gen_sba;

  gen_sba_n : if not SBA_ENABLE generate
    sba_rdata           <= (others => '0');
    sba_sbi_ini_o.cs    <= '0';
    sba_sbi_ini_o.re    <= '0';
    sba_sbi_ini_o.we    <= '0';
    sba_sbi_ini_o.addr  <= (others => '0');
    sba_sbi_ini_o.wdata <= (others => '0');
  end generate gen_sba_n;

end architecture rtl;
