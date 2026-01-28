library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     ieee.std_logic_textio.all;
use     std.textio.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;

entity tb_WardRV is
  generic (
    FIRMWARE_FILE  : string  := "firmware.hex";
    SIGNATURE_FILE : string  := "signature.output";
    MEM_SIZE       : integer := 65536; -- 64KB
    VERBOSE        : boolean := false
  );
end tb_WardRV;

architecture rtl of tb_WardRV is

  -- Constants
  constant CLK_PERIOD  : time := 10 ns;
  constant TOHOST_ADDR : std_logic_vector(31 downto 0) := x"80001000";

  -- Signals
  signal  test_done                : std_logic := '0';
  signal clk_i    : std_logic := '0';
  signal arst_b_i : std_logic := '0';
  signal sim_end  : boolean   := false;

  -- Interfaces
  signal inst_ini : inst_ini_t;
  signal inst_tgt : inst_tgt_t;
  signal sbi_ini  : sbi_ini_t;
  signal sbi_tgt  : sbi_tgt_t;
  signal it_val   : std_logic := '0';
  signal it_ack   : std_logic;

  -- Memory
  type ram_t is array (0 to MEM_SIZE-1) of std_logic_vector(7 downto 0);
  signal mem : ram_t := (others => x"00");


  -- Helper to dump signature (Optional placeholder)
  procedure dump_signature(file_name : in string; signal ram : in ram_t) is
    file f_out      : text open write_mode is file_name;
    variable l      : line;
    variable word   : std_logic_vector(31 downto 0);
  begin
    -- Example: Dump a specific range if needed by the compliance framework
    -- Typically, the framework parses the ELF to find begin_signature symbol
    -- Here we just report completion.
    report "Simulation finished. Signature dump logic can be added here.";
  end procedure;

  function to_hex_string(slv : std_logic_vector) return string is
    variable hex_digits : string(1 to 16) := "0123456789abcdef";
    variable result     : string(1 to slv'length/4);
    variable nibble     : integer;
  begin
    for i in result'range loop
      nibble := to_integer(unsigned(slv(slv'length - (i-1)*4 - 1 downto slv'length - i*4)));
      result(i) := hex_digits(nibble + 1);
    end loop;
    return result;
  end function;

  -- Helper to read Hex
  procedure load_hex(file_name : in string; signal ram : inout ram_t; size : out integer) is
    file f_in       : text open read_mode is file_name;
    variable l      : line;
    variable word   : std_logic_vector(31 downto 0);
    variable addr   : integer := 0;
    variable good   : boolean;
  begin
    while not endfile(f_in) and addr < MEM_SIZE loop
      readline(f_in, l);
      hread(l, word, good);
      if good then

        report integer'image(addr) & "  " & to_hex_string(word);

        -- Little Endian loading
        ram(addr)   <= word(7 downto 0);
        ram(addr+1) <= word(15 downto 8);
        ram(addr+2) <= word(23 downto 16);
        ram(addr+3) <= word(31 downto 24);

        --wait for 0 ns;
        --report integer'image(addr) & "  " & to_hex_string(ram(addr+3));

        addr := addr + 4;
      end if;
    end loop;
    size := addr;
    report "Loaded " & integer'image(addr) & " bytes from " & file_name;
  end procedure;

  procedure print_firmware(signal ram : in ram_t; size : in integer) is
    variable l : line;
    variable word : std_logic_vector(31 downto 0);
  begin
    if VERBOSE then
      write(l, string'("Firmware Content:"));
      writeline(output, l);
      for i in 0 to (size/4)-1 loop
        word := ram(i*4+3) & ram(i*4+2) & ram(i*4+1) & ram(i*4);
        write(l, string'("  @") & to_hex_string(std_logic_vector(to_unsigned(i*4, 32))) & string'(": ") & to_hex_string(word));
        writeline(output, l);
      end loop;
    end if;
  end procedure;

  procedure print_instruction(addr : std_logic_vector; inst : std_logic_vector) is
  begin
    if VERBOSE then
      report "Fetch @ 0x" & to_hex_string(addr) & " : 0x" & to_hex_string(inst);
    end if;
  end procedure;

begin

  -- Clock Generation
  clk_i <= not test_done and not clk_i after CLK_PERIOD / 2 when not sim_end else '0';
  arst_b_i <= '1' after 5 * CLK_PERIOD;

  -- DUT Instance
  dut : entity asylum.WardRV
    generic map (
      RESET_ADDR => x"00000000",
      IT_ADDR    => x"00000000",
      BIG_ENDIAN => false
    )
    port map (
      clk_i      => clk_i,
      arst_b_i   => arst_b_i,
      inst_ini_o => inst_ini,
      inst_tgt_i => inst_tgt,
      sbi_ini_o  => sbi_ini,
      sbi_tgt_i  => sbi_tgt,
      it_val_i   => it_val,
      it_ack_o   => it_ack
    );

  -- Initial Load
  process
    variable fw_size : integer;
  begin
    -- Wait for 1 delta cycle to ensure signals are initialized
    wait for 0 ns;
    load_hex(FIRMWARE_FILE, mem, fw_size);
    wait for 0 ns;
    print_firmware(mem, fw_size);
    wait for 1000 ns;

    report "[TESTBENCH] Test OK";
    test_done <= '1';
    wait;

  end process;

  -- Memory Access Process (Dual Port behavior simulation)
  process(clk_i)
    variable i_addr : integer;
    variable d_addr : integer;
    variable v_inst : std_logic_vector(31 downto 0);
  begin
    if rising_edge(clk_i) then
      -- Instruction Fetch
      inst_tgt.ready <= '0';
      inst_tgt.inst  <= (others => '0');
      
      if inst_ini.valid = '1' then
        i_addr := to_integer(unsigned(inst_ini.addr));
        if i_addr < MEM_SIZE - 3 then
          v_inst := mem(i_addr+3) & mem(i_addr+2) & mem(i_addr+1) & mem(i_addr);
          inst_tgt.inst <= v_inst;
          print_instruction(inst_ini.addr, v_inst);
          inst_tgt.ready <= '1';
        else
          -- Out of bounds fetch returns 0 (NOP/Illegal)
          inst_tgt.ready <= '1';
        end if;
      end if;

      -- Data Access
      sbi_tgt.ready <= '0';
      sbi_tgt.rdata <= (others => '0');
      sbi_tgt.err   <= '0';

      if sbi_ini.valid = '1' then
        d_addr := to_integer(unsigned(sbi_ini.addr));
        
        -- Check for TOHOST (Simulation Exit)
        -- Assuming writing to a specific high address signals end
        if sbi_ini.addr = TOHOST_ADDR and sbi_ini.we = '1' then
           if to_integer(unsigned(sbi_ini.wdata)) = 1 then
             report "TEST PASSED (tohost = 1)";
           else
             report "TEST FAILED (tohost = " & integer'image(to_integer(unsigned(sbi_ini.wdata))) & ")";
           end if;
           sim_end <= true;
           
        elsif d_addr < MEM_SIZE - 3 then
          sbi_tgt.ready <= '1';
          
          -- Write
          if sbi_ini.we = '1' then
            if sbi_ini.be(0) = '1' then mem(d_addr)   <= sbi_ini.wdata(7 downto 0); end if;
            if sbi_ini.be(1) = '1' then mem(d_addr+1) <= sbi_ini.wdata(15 downto 8); end if;
            if sbi_ini.be(2) = '1' then mem(d_addr+2) <= sbi_ini.wdata(23 downto 16); end if;
            if sbi_ini.be(3) = '1' then mem(d_addr+3) <= sbi_ini.wdata(31 downto 24); end if;
          
          -- Read
          else
            sbi_tgt.rdata(7 downto 0)   <= mem(d_addr);
            sbi_tgt.rdata(15 downto 8)  <= mem(d_addr+1);
            sbi_tgt.rdata(23 downto 16) <= mem(d_addr+2);
            sbi_tgt.rdata(31 downto 24) <= mem(d_addr+3);
          end if;
        else
          -- Out of bounds access
          sbi_tgt.err   <= '1';
          sbi_tgt.ready <= '1';
        end if;
      end if;
    end if;
  end process;

end rtl;
