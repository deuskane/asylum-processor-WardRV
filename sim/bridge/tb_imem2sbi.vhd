-------------------------------------------------------------------------------
-- Title      : imem2sbi Testbench
-- Project    :
-------------------------------------------------------------------------------
-- File       : tb_imem2sbi.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: imem2sbi fetching from a sbi_ram, with asynchronous and
--              synchronous read. The testbench fills the RAM through SBI, then
--              the bridge fetches words and the instruction is checked
--              (little endian). On the bridge SBI port, cs must be driven and
--              equal to re.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

entity tb_imem2sbi is
end entity tb_imem2sbi;

architecture tb of tb_imem2sbi is

  constant C_CLK_PERIOD : time    := 10 ns;
  constant C_DEPTH      : natural := 2**SBI_ADDR_WIDTH;
  constant C_TIMEOUT    : natural := 100; -- cycles per access

  type booleans_t is array (natural range <>) of boolean;
  type addrs_t    is array (natural range <>) of std_logic_vector(31 downto 0);

  constant C_SYNC_READ  : booleans_t := (false, true);
  constant C_ADDRS      : addrs_t    := (x"00000000", x"00000004", x"00000008", x"00000040",
                                         x"000000FC", x"80000010", x"80000013", x"00000004");

  -- Byte stored at a RAM address
  function ram_byte(addr : natural) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned((addr * 37 + 11) mod 256, 8));
  end function;

  signal clk    : std_logic := '0';
  signal arst_b : std_logic := '0';
  signal done   : std_logic_vector(C_SYNC_READ'range) := (others => '0');

begin

  clock_generator(clk, C_CLK_PERIOD);
  arst_b <= '1' after 5 * C_CLK_PERIOD;

  g_cfg : for i in C_SYNC_READ'range generate
    signal load        : std_logic := '1'; -- '1' : the testbench owns the RAM SBI port
    signal imem_ini    : imem_ini_t;
    signal imem_tgt    : imem_tgt_t;
    signal tb_sbi_ini  : sbi_ini_t(addr(SBI_ADDR_WIDTH-1 downto 0), wdata(SBI_DATA_WIDTH-1 downto 0));
    signal brg_sbi_ini : sbi_ini_t(addr(SBI_ADDR_WIDTH-1 downto 0), wdata(SBI_DATA_WIDTH-1 downto 0));
    signal ram_sbi_ini : sbi_ini_t(addr(SBI_ADDR_WIDTH-1 downto 0), wdata(SBI_DATA_WIDTH-1 downto 0));
    signal ram_sbi_tgt : sbi_tgt_t(rdata(SBI_DATA_WIDTH-1 downto 0));
  begin

    dut : entity asylum.imem2sbi
      port map (
        clk_i      => clk,
        arst_b_i   => arst_b,
        imem_ini_i => imem_ini,
        imem_tgt_o => imem_tgt,
        sbi_ini_o  => brg_sbi_ini,
        sbi_tgt_i  => ram_sbi_tgt
      );

    ram_sbi_ini <= tb_sbi_ini when load = '1' else brg_sbi_ini;

    ram : entity asylum.sbi_ram
      generic map (
        DEPTH     => C_DEPTH,
        SYNC_READ => C_SYNC_READ(i)
      )
      port map (
        clk_i     => clk,
        arst_b_i  => arst_b,
        sbi_ini_i => ram_sbi_ini,
        sbi_tgt_o => ram_sbi_tgt
      );

    -- Bridge SBI port : cs driven, and asserted with re
    p_monitor : process
    begin
      wait until rising_edge(clk);
      if arst_b = '1' and load = '0'
      then
        check_value(brg_sbi_ini.cs, brg_sbi_ini.re, ERROR, "imem2sbi (SYNC_READ=" & to_string(C_SYNC_READ(i)) & ") : sbi cs = re");
      end if;
    end process;

    p_test : process
      variable v_addr  : natural;
      variable v_exp   : std_logic_vector(31 downto 0);
      variable v_cycle : natural;
    begin
      imem_ini.valid   <= '0';
      imem_ini.addr    <= (others => '0');
      tb_sbi_ini.cs    <= '0';
      tb_sbi_ini.re    <= '0';
      tb_sbi_ini.we    <= '0';
      tb_sbi_ini.addr  <= (others => '0');
      tb_sbi_ini.wdata <= (others => '0');

      wait until arst_b = '1';
      wait until rising_edge(clk);

      -- Fill the RAM through SBI
      for a in 0 to C_DEPTH-1 loop
        tb_sbi_ini.cs    <= '1';
        tb_sbi_ini.we    <= '1';
        tb_sbi_ini.addr  <= std_logic_vector(to_unsigned(a, SBI_ADDR_WIDTH));
        tb_sbi_ini.wdata <= ram_byte(a);
        wait until rising_edge(clk) and ram_sbi_tgt.ready = '1';
      end loop;
      tb_sbi_ini.cs    <= '0';
      tb_sbi_ini.we    <= '0';
      wait until rising_edge(clk);
      load <= '0';
      wait until rising_edge(clk);

      -- Fetch through the bridge
      for k in C_ADDRS'range loop
        v_addr := to_integer(unsigned(C_ADDRS(k)(SBI_ADDR_WIDTH-1 downto 2))) * 4;
        v_exp  := ram_byte(v_addr+3) & ram_byte(v_addr+2) & ram_byte(v_addr+1) & ram_byte(v_addr);

        imem_ini.valid <= '1';
        imem_ini.addr  <= C_ADDRS(k);
        v_cycle        := 0;
        loop
          wait until rising_edge(clk);
          exit when imem_tgt.ready = '1' or v_cycle = C_TIMEOUT;
          v_cycle := v_cycle + 1;
        end loop;
        imem_ini.valid <= '0';

        if imem_tgt.ready = '1'
        then
          check_value(imem_tgt.inst, v_exp, ERROR, "imem2sbi (SYNC_READ=" & to_string(C_SYNC_READ(i)) & ") : fetch @ 0x" & to_hstring(C_ADDRS(k)));
        else
          alert(ERROR, "imem2sbi (SYNC_READ=" & to_string(C_SYNC_READ(i)) & ") : fetch @ 0x" & to_hstring(C_ADDRS(k)) & " timeout");
        end if;
      end loop;

      done(i) <= '1';
      wait;
    end process;

  end generate g_cfg;

  -- Main Sequencer
  p_main : process
  begin
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    log(ID_LOG_HDR, "Starting Simulation of imem2sbi");

    wait until (and done) = '1' for 100 us;
    if (and done) /= '1'
    then
      alert(TB_ERROR, "Simulation Timeout");
    end if;

    report_alert_counters(FINAL);
    std.env.stop;
  end process;

end architecture tb;
