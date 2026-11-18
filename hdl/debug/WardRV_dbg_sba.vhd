-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_sba.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- System Bus Access of the Debug Module (Debug Spec 1.0 chapter 3.10,
-- registers sbcs / sbaddress0 / sbdata0 of chapter 3.14) : memory accesses
-- of the debugger without the hart, also while it runs.
--  * 8, 16 and 32-bit accesses, sbautoincrement, sbreadonaddr, sbreadondata
--  * sbbusyerror : sbaddress0 / sbdata0 accessed during an access
--  * sberror 3 (misaligned) and 4 (size), the bus itself never fails
-- The access is a 32-bit dmem request (valid until ready, byte enables) :
-- dmem2sbi connects it to a SoC SBI bus.
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
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_dbg_sba is
  port (
    -- Clock and Reset
    clk_i          : in  std_logic
   ;arst_b_i       : in  std_logic
   ;dmactive_i     : in  std_logic                                -- 0 : registers in reset
    -- DMI (decoded here : sbcs, sbaddress0, sbdata0)
   ;dmi_req_i      : in  dmi_req_t
   ;dmi_rdata_o    : out std_logic_vector(32-1 downto 0)
    -- System bus
   ;sba_dmem_ini_o : out dmem_ini_t
   ;sba_dmem_tgt_i : in  dmem_tgt_t
  );
end entity WardRV_dbg_sba;

architecture rtl of WardRV_dbg_sba is

  constant SBACCESS_8  : std_logic_vector(2 downto 0) := "000";
  constant SBACCESS_16 : std_logic_vector(2 downto 0) := "001";
  constant SBACCESS_32 : std_logic_vector(2 downto 0) := "010";

  signal   busy_r            : std_logic;                         -- Access on the bus
  signal   we_r              : std_logic;
  signal   sbaddress_r       : std_logic_vector(32-1 downto 0);
  signal   sbdata_r          : std_logic_vector(32-1 downto 0);
  signal   sbaccess_r        : std_logic_vector(2 downto 0);
  signal   sbautoincrement_r : std_logic;
  signal   sbreadonaddr_r    : std_logic;
  signal   sbreadondata_r    : std_logic;
  signal   sbbusyerror_r     : std_logic;
  signal   sberror_r         : std_logic_vector(2 downto 0);

  signal   sbcs              : std_logic_vector(32-1 downto 0);
  signal   lane              : natural range 0 to 3;              -- Byte of the address in the word
  signal   size              : natural range 1 to 4;              -- Bytes of an access

begin  -- architecture rtl

  sbcs <= "001"                & -- sbversion 1
          "000000"             &
          sbbusyerror_r        &
          busy_r               & -- sbbusy
          sbreadonaddr_r       &
          sbaccess_r           &
          sbautoincrement_r    &
          sbreadondata_r       &
          sberror_r            &
          "0100000"            & -- sbasize 32
          "00111";               -- sbaccess32, 16, 8

  dmi_rdata_o <= sbcs        when dmi_req_i.addr = DM_SBCS       else
                 sbaddress_r when dmi_req_i.addr = DM_SBADDRESS0 else
                 sbdata_r    when dmi_req_i.addr = DM_SBDATA0    else
                 (others => '0');

  lane <= to_integer(unsigned(sbaddress_r(1 downto 0)));
  size <= 1 when sbaccess_r = SBACCESS_8  else
          2 when sbaccess_r = SBACCESS_16 else
          4;

  -----------------------------------------------------------------------------
  -- Registers and accesses
  -----------------------------------------------------------------------------
  process(clk_i, arst_b_i)

    -- Start a read or a write at addr (new sbaddress0 for sbreadonaddr)
    procedure start(we : std_logic; addr : std_logic_vector(1 downto 0)) is
    begin
      if sbaccess_r /= SBACCESS_8 and sbaccess_r /= SBACCESS_16 and sbaccess_r /= SBACCESS_32
      then
        sberror_r <= "100";                                       -- Size not supported
      elsif (sbaccess_r = SBACCESS_16 and addr(0) = '1') or
            (sbaccess_r = SBACCESS_32 and addr /= "00")
      then
        sberror_r <= "011";                                       -- Misaligned
      else
        busy_r    <= '1';
        we_r      <= we;
      end if;
    end procedure;

    variable v_noerror : boolean;
    variable v_rdata   : std_logic_vector(32-1 downto 0);
  begin
    if arst_b_i = '0'
    then
      busy_r            <= '0';
      we_r              <= '0';
      sbaddress_r       <= (others => '0');
      sbdata_r          <= (others => '0');
      sbaccess_r        <= SBACCESS_32;
      sbautoincrement_r <= '0';
      sbreadonaddr_r    <= '0';
      sbreadondata_r    <= '0';
      sbbusyerror_r     <= '0';
      sberror_r         <= "000";
    elsif rising_edge(clk_i)
    then
      -- End of the access on the bus
      if busy_r = '1' and sba_dmem_tgt_i.ready = '1'
      then
        busy_r <= '0';
        if we_r = '0'
        then
          v_rdata  := std_logic_vector(shift_right(unsigned(sba_dmem_tgt_i.rdata), 8*lane));
          case sbaccess_r is
            when SBACCESS_8  => sbdata_r <= x"000000" & v_rdata( 8-1 downto 0);
            when SBACCESS_16 => sbdata_r <= x"0000"   & v_rdata(16-1 downto 0);
            when others      => sbdata_r <= v_rdata;
          end case;
        end if;
        if sbautoincrement_r = '1'
        then
          sbaddress_r <= std_logic_vector(unsigned(sbaddress_r) + size);
        end if;
      end if;

      -- DMI : no new access while an error is pending
      v_noerror := sbbusyerror_r = '0' and sberror_r = "000";
      if dmi_req_i.valid = '1'
      then
        if (dmi_req_i.addr = DM_SBADDRESS0 and dmi_req_i.we = '1') or dmi_req_i.addr = DM_SBDATA0
        then
          if busy_r = '1'
          then
            sbbusyerror_r <= '1';
          elsif dmi_req_i.addr = DM_SBADDRESS0
          then
            sbaddress_r <= dmi_req_i.wdata;
            if v_noerror and sbreadonaddr_r = '1'
            then
              start('0', dmi_req_i.wdata(1 downto 0));
            end if;
          elsif dmi_req_i.we = '1'
          then
            if v_noerror
            then
              sbdata_r <= dmi_req_i.wdata;
              start('1', sbaddress_r(1 downto 0));
            end if;
          elsif v_noerror and sbreadondata_r = '1'
          then
            -- The read returns sbdata0, then the next read starts
            start('0', sbaddress_r(1 downto 0));
          end if;
        elsif dmi_req_i.addr = DM_SBCS and dmi_req_i.we = '1'
        then
          sbbusyerror_r     <= sbbusyerror_r and not dmi_req_i.wdata(22);    -- W1C
          sbreadonaddr_r    <= dmi_req_i.wdata(20);
          sbaccess_r        <= dmi_req_i.wdata(19 downto 17);
          sbautoincrement_r <= dmi_req_i.wdata(16);
          sbreadondata_r    <= dmi_req_i.wdata(15);
          sberror_r         <= sberror_r and not dmi_req_i.wdata(14 downto 12); -- W1C
        end if;
      end if;

      -- dmactive = 0 : reset values (an access in progress completes)
      if dmactive_i = '0'
      then
        sbaddress_r       <= (others => '0');
        sbdata_r          <= (others => '0');
        sbaccess_r        <= SBACCESS_32;
        sbautoincrement_r <= '0';
        sbreadonaddr_r    <= '0';
        sbreadondata_r    <= '0';
        sbbusyerror_r     <= '0';
        sberror_r         <= "000";
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Bus request : data and byte enables aligned on the byte lane
  -----------------------------------------------------------------------------
  sba_dmem_ini_o.valid <= busy_r;
  sba_dmem_ini_o.addr  <= sbaddress_r;
  sba_dmem_ini_o.we    <= we_r;
  sba_dmem_ini_o.wdata <= std_logic_vector(shift_left(unsigned(sbdata_r), 8*lane));
  sba_dmem_ini_o.be    <= std_logic_vector(shift_left(unsigned'("0001"), lane)) when sbaccess_r = SBACCESS_8  else
                          std_logic_vector(shift_left(unsigned'("0011"), lane)) when sbaccess_r = SBACCESS_16 else
                          "1111";

end architecture rtl;
