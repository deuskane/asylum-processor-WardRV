-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_sba.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- System Bus Access of the Debug Module (RISC-V Debug Spec 1.0) :
--  * sbcs : sbversion=1, sbbusyerror (W1C), sbbusy, sbreadonaddr, sbaccess
--           (8/16/32 bits), sbautoincrement, sbreadondata, sberror (W1C),
--           sbasize = SBA_ADDR_WIDTH, sbaccess8/16/32
--  * sbaddress0, sbdata0
-- The access engine produces a dmem request (valid, addr, wdata, we, be)
-- converted to the 8-bit SoC SBI bus by dmem2sbi.
-- sberror : 3 (misaligned) and 4 (unsupported size) are detected, the bus
-- never reports an error (dmem2sbi).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-10-09  1.0      mrosiere Created
-- 2026-10-09  1.1      mrosiere Review : alignment checked on the new
--                               sbaddress0 (sbreadonaddr), sbdata0 not
--                               written while sberror / sbbusyerror is set
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_sba is
  generic (
    SBA_ADDR_WIDTH : positive := 32   -- SoC SBI address width (sbasize)
  );
  port (
    clk_i         : in  std_logic;
    arst_b_i      : in  std_logic;

    dmactive_i    : in  std_logic;    -- 0 : SBA registers in reset

    -- DMI accesses (one cycle strobes)
    dmi_we_i      : in  std_logic;
    dmi_re_i      : in  std_logic;
    dmi_addr_i    : in  std_logic_vector(DMI_ABITS-1 downto 0);
    dmi_wdata_i   : in  std_logic_vector(31 downto 0);
    dmi_rdata_o   : out std_logic_vector(31 downto 0);

    -- SoC bus (SBI initiator)
    sba_sbi_ini_o : out sbi_ini_t(addr (SBA_ADDR_WIDTH-1 downto 0),
                                  wdata(7 downto 0));
    sba_sbi_tgt_i : in  sbi_tgt_t(rdata(7 downto 0))
  );
end entity WardRV_dbg_sba;

architecture rtl of WardRV_dbg_sba is

  constant SBACCESS_8   : std_logic_vector(2 downto 0) := "000";
  constant SBACCESS_16  : std_logic_vector(2 downto 0) := "001";
  constant SBACCESS_32  : std_logic_vector(2 downto 0) := "010";
  constant SBERR_ALIGN  : std_logic_vector(2 downto 0) := "011";
  constant SBERR_SIZE   : std_logic_vector(2 downto 0) := "100";

  type state_t is (SB_IDLE, SB_REQ, SB_WAIT);
  signal state_r         : state_t;

  signal sbaddr_r        : std_logic_vector(31 downto 0);
  signal sbdata_r        : std_logic_vector(31 downto 0);
  signal sbaccess_r      : std_logic_vector(2 downto 0);
  signal sbautoincr_r    : std_logic;
  signal sbreadonaddr_r  : std_logic;
  signal sbreadondata_r  : std_logic;
  signal sbbusyerror_r   : std_logic;
  signal sberror_r       : std_logic_vector(2 downto 0);
  signal we_r            : std_logic;

  signal sbbusy          : std_logic;
  signal sbasize         : std_logic_vector(6 downto 0);
  signal lane            : natural range 0 to 3;

  signal dmem_ini        : dmem_ini_t;
  signal dmem_tgt        : dmem_tgt_t;

begin

  sbbusy  <= '0' when state_r = SB_IDLE else '1';
  sbasize <= std_logic_vector(to_unsigned(SBA_ADDR_WIDTH, 7));
  lane    <= to_integer(unsigned(sbaddr_r(1 downto 0)));

  --------------------------------------------------------------------
  -- DMI read
  --------------------------------------------------------------------
  process(all)
  begin
    case dmi_addr_i is
      when DM_SBCS       =>
        dmi_rdata_o <= "001"           & -- sbversion
                       "000000"        &
                       sbbusyerror_r   &
                       sbbusy          &
                       sbreadonaddr_r  &
                       sbaccess_r      &
                       sbautoincr_r    &
                       sbreadondata_r  &
                       sberror_r       &
                       sbasize         &
                       "00111";          -- sbaccess128/64/32/16/8
      when DM_SBADDRESS0 => dmi_rdata_o <= sbaddr_r;
      when DM_SBDATA0    => dmi_rdata_o <= sbdata_r;
      when others        => dmi_rdata_o <= (others => '0');
    end case;
  end process;

  --------------------------------------------------------------------
  -- Registers and access engine
  --------------------------------------------------------------------
  process(clk_i, arst_b_i)

    -- Start an access (no error pending) at the address whose 2 LSB are addr
    -- (new value of sbaddress0 for sbreadonaddr)
    procedure start(we : std_logic; addr : std_logic_vector(1 downto 0)) is
    begin
      if sbaccess_r /= SBACCESS_8 and sbaccess_r /= SBACCESS_16 and sbaccess_r /= SBACCESS_32
      then
        sberror_r <= SBERR_SIZE;
      elsif (sbaccess_r = SBACCESS_16 and addr(0) /= '0') or
            (sbaccess_r = SBACCESS_32 and addr /= "00")
      then
        sberror_r <= SBERR_ALIGN;
      else
        we_r      <= we;
        state_r   <= SB_REQ;
      end if;
    end procedure;

    variable v_noerr : boolean;
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    if arst_b_i = '0'
    then
      state_r        <= SB_IDLE;
      sbaddr_r       <= (others => '0');
      sbdata_r       <= (others => '0');
      sbaccess_r     <= SBACCESS_32;
      sbautoincr_r   <= '0';
      sbreadonaddr_r <= '0';
      sbreadondata_r <= '0';
      sbbusyerror_r  <= '0';
      sberror_r      <= (others => '0');
      we_r           <= '0';
    elsif rising_edge(clk_i)
    then
      if dmactive_i = '0'
      then
        state_r        <= SB_IDLE;
        sbaddr_r       <= (others => '0');
        sbdata_r       <= (others => '0');
        sbaccess_r     <= SBACCESS_32;
        sbautoincr_r   <= '0';
        sbreadonaddr_r <= '0';
        sbreadondata_r <= '0';
        sbbusyerror_r  <= '0';
        sberror_r      <= (others => '0');
        we_r           <= '0';
      else
        v_noerr := sbbusyerror_r = '0' and sberror_r = "000";

        ------------------------------------------------------------
        -- Access engine
        ------------------------------------------------------------
        case state_r is
          when SB_REQ  =>
            state_r <= SB_WAIT;
          when SB_WAIT =>
            if dmem_tgt.ready = '1'
            then
              if we_r = '0'
              then
                v_rdata := std_logic_vector(shift_right(unsigned(dmem_tgt.rdata), 8*lane));
                case sbaccess_r is
                  when SBACCESS_8  => sbdata_r <= x"000000" & v_rdata(7 downto 0);
                  when SBACCESS_16 => sbdata_r <= x"0000"   & v_rdata(15 downto 0);
                  when others      => sbdata_r <= v_rdata;
                end case;
              end if;
              if sbautoincr_r = '1'
              then
                case sbaccess_r is
                  when SBACCESS_8  => sbaddr_r <= std_logic_vector(unsigned(sbaddr_r) + 1);
                  when SBACCESS_16 => sbaddr_r <= std_logic_vector(unsigned(sbaddr_r) + 2);
                  when others      => sbaddr_r <= std_logic_vector(unsigned(sbaddr_r) + 4);
                end case;
              end if;
              state_r <= SB_IDLE;
            end if;
          when others  =>
            null;
        end case;

        ------------------------------------------------------------
        -- DMI accesses
        ------------------------------------------------------------
        if dmi_we_i = '1'
        then
          case dmi_addr_i is
            when DM_SBCS =>
              if dmi_wdata_i(22) = '1' then
                sbbusyerror_r <= '0';
              end if;
              sbreadonaddr_r <= dmi_wdata_i(20);
              sbaccess_r     <= dmi_wdata_i(19 downto 17);
              sbautoincr_r   <= dmi_wdata_i(16);
              sbreadondata_r <= dmi_wdata_i(15);
              sberror_r      <= sberror_r and not dmi_wdata_i(14 downto 12);

            when DM_SBADDRESS0 =>
              if sbbusy = '1'
              then
                sbbusyerror_r <= '1';
              else
                sbaddr_r <= dmi_wdata_i;
                if v_noerr and sbreadonaddr_r = '1'
                then
                  start('0', dmi_wdata_i(1 downto 0));
                end if;
              end if;

            when DM_SBDATA0 =>
              -- sberror / sbbusyerror set : the access does nothing
              if sbbusy = '1'
              then
                sbbusyerror_r <= '1';
              elsif v_noerr
              then
                sbdata_r <= dmi_wdata_i;
                start('1', sbaddr_r(1 downto 0));
              end if;

            when others => null;
          end case;
        end if;

        if dmi_re_i = '1' and dmi_addr_i = DM_SBDATA0
        then
          if sbbusy = '1'
          then
            sbbusyerror_r <= '1';
          elsif v_noerr and sbreadondata_r = '1'
          then
            start('0', sbaddr_r(1 downto 0));
          end if;
        end if;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- dmem request -> 8-bit SoC SBI bus
  --------------------------------------------------------------------
  dmem_ini.valid <= '1' when state_r = SB_REQ else '0';
  dmem_ini.addr  <= sbaddr_r;
  dmem_ini.we    <= we_r;
  dmem_ini.wdata <= std_logic_vector(shift_left(unsigned(sbdata_r), 8*lane));
  dmem_ini.be    <= std_logic_vector(shift_left(unsigned'("0001"), lane)) when sbaccess_r = SBACCESS_8  else
                    std_logic_vector(shift_left(unsigned'("0011"), lane)) when sbaccess_r = SBACCESS_16 else
                    "1111";

  ins_dmem2sbi : entity work.dmem2sbi
    port map (
      clk_i      => clk_i,
      arst_b_i   => arst_b_i,
      dmem_ini_i => dmem_ini,
      dmem_tgt_o => dmem_tgt,
      sbi_ini_o  => sba_sbi_ini_o,
      sbi_tgt_i  => sba_sbi_tgt_i
    );

end architecture rtl;
