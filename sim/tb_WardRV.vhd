-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : tb_WardRV.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
use     std.textio.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_iss_pkg.all;

library uvvm_util;
context uvvm_util.uvvm_util_context;

use work.WardRV_vips.all;

entity tb_WardRV is
  generic (
    FIRMWARE_FILE  : string  := "firmware.hex";
    SIGNATURE_FILE : string  := "signature.output";
    MEM_SIZE       : integer := 65536; -- 64KB
    VERBOSE        : boolean := false;
    USE_ISS        : boolean := true
  );
end tb_WardRV;

architecture rtl of tb_WardRV is

  -- Constants
  constant CLK_PERIOD  : time := 10 ns;
  constant TOHOST_ADDR : std_logic_vector(31 downto 0) := x"80001000";

  -- Signals
  signal clk_i    : std_logic := '0';
  signal arst_b_i : std_logic := '0';
  signal sim_end_iss : boolean := false;
  signal sim_end_rtl : boolean := false;

  -- Interfaces
  signal inst_ini : inst_ini_t;
  signal inst_tgt : inst_tgt_t;
  signal sbi_ini  : sbi_ini_t;
  signal sbi_tgt  : sbi_tgt_t;
  signal it_val   : std_logic := '0';
  signal it_ack   : std_logic;

  -- JTAG
  signal jtag_ini : jtag_ini_t := (tck => '0', trst_n => '0', tms => '0', tdi => '0');
  signal jtag_tgt : jtag_tgt_t;

  -- Memory
  type ram_t is array (0 to MEM_SIZE-1) of std_logic_vector(7 downto 0);

  -- Helper to dump signature (Optional placeholder)
  procedure dump_signature(file_name : in string; signal ram : in ram_t) is
    file f_out      : text open write_mode is file_name;
    variable l      : line;
    variable word   : std_logic_vector(31 downto 0);
  begin
    -- Example: Dump a specific range if needed by the compliance framework
    -- Typically, the framework parses the ELF to find begin_signature symbol
    -- Here we just report completion.
    log(ID_SEQUENCER, "Simulation finished. Signature dump logic can be added here.");
  end procedure;

  -- Helper to read Hex
  impure function init_ram(file_name : string) return ram_t is
    file f_in       : text open read_mode is file_name;
    variable l      : line;
    variable word   : std_logic_vector(31 downto 0);
    variable addr   : integer := 0;
    variable good   : boolean;
    variable ram    : ram_t := (others => x"00");
  begin
    while not endfile(f_in) and addr < MEM_SIZE loop
      readline(f_in, l);
      hread(l, word, good);
      if good then

        report integer'image(addr) & "  " & to_hstring(word);

        -- Little Endian loading
        ram(addr)   := word(7 downto 0);
        ram(addr+1) := word(15 downto 8);
        ram(addr+2) := word(23 downto 16);
        ram(addr+3) := word(31 downto 24);


        addr := addr + 4;
      end if;
    end loop;
    report "Loaded " & integer'image(addr) & " bytes from " & file_name;
    return ram;
  end function;

  signal mem : ram_t := init_ram(FIRMWARE_FILE);

  procedure print_firmware(signal ram : in ram_t; size : in integer) is
    variable l : line;
    variable word : std_logic_vector(31 downto 0);
  begin
    if VERBOSE then
      log(ID_SEQUENCER, "Firmware Content:");
      for i in 0 to (size/4)-1 loop
        word := ram(i*4+3) & ram(i*4+2) & ram(i*4+1) & ram(i*4);
        log(ID_SEQUENCER, "  @" & to_hstring(std_logic_vector(to_unsigned(i*4, 32))) & ": " & to_hstring(word));
      end loop;
    end if;
  end procedure;

  procedure print_instruction(addr : std_logic_vector; inst : std_logic_vector) is
  begin
    if VERBOSE then
      log(ID_BFM, "Fetch @ 0x" & to_hstring(addr) & " : 0x" & to_hstring(inst));
    end if;
  end procedure;

begin

  -- Clock Generation
  clock_generator(clk_i, CLK_PERIOD);

  -- Reset Generation
  p_reset : process
  begin
    vip_reset_pulse(arst_b_i, 5 * CLK_PERIOD, "System Reset");
    wait until arst_b_i = '1';
    wait;
  end process;

  -- DUT Instance
  dut : entity asylum.WardRV_fsm
    generic map (
      RESET_ADDR => x"00000000",
      VERBOSE    => VERBOSE
    )
    port map (
      clk_i      => clk_i,
      arst_b_i   => arst_b_i,
      inst_ini_o => inst_ini,
      inst_tgt_i => inst_tgt,
      sbi_ini_o  => sbi_ini,
      sbi_tgt_i  => sbi_tgt
      --it_val_i   => it_val,
      --it_ack_o   => it_ack,
      --jtag_ini_i => jtag_ini,
      --jtag_tgt_o => jtag_tgt
    );

  -- ISS Execution Loop
  p_iss : process
    variable iss       : iss_t;
    variable v_inst    : std_logic_vector(31 downto 0);
    variable v_addr    : integer;
    variable v_mem_req : boolean;
    variable v_we      : std_logic;
    variable v_maddr   : std_logic_vector(31 downto 0);
    variable v_wdata   : std_logic_vector(31 downto 0);
    variable v_be      : std_logic_vector(3 downto 0);
    variable v_rdata   : std_logic_vector(31 downto 0);
  begin
    if not USE_ISS then
      wait;
    end if;

    iss.reset(x"00000000");
    wait until arst_b_i = '1';

    while not sim_end_iss loop
      wait until rising_edge(clk_i);
      
      -- Fetch
      v_addr := to_integer(unsigned(iss.get_pc));
      v_inst := mem(v_addr+3) & mem(v_addr+2) & mem(v_addr+1) & mem(v_addr);
      
      print_instruction(iss.get_pc, v_inst);

      -- Execute
      iss.execute_instruction(v_inst, v_mem_req, v_we, v_maddr, v_wdata, v_be);

      -- Handle Memory
      if v_mem_req then
        if v_maddr = TOHOST_ADDR and v_we = '1' then
          if to_integer(unsigned(v_wdata)) = 1 then
            log(ID_LOG_HDR, "ISS: TEST PASSED");
          else
            alert(TB_ERROR, "ISS: TEST FAILED");
          end if;
          sim_end_iss <= true;
        else
          v_addr := to_integer(unsigned(v_maddr));
          if v_we = '1' then
            if v_be(0) = '1' then mem(v_addr)   <= v_wdata(7 downto 0); end if;
            if v_be(1) = '1' then mem(v_addr+1) <= v_wdata(15 downto 8); end if;
            if v_be(2) = '1' then mem(v_addr+2) <= v_wdata(23 downto 16); end if;
            if v_be(3) = '1' then mem(v_addr+3) <= v_wdata(31 downto 24); end if;
          else
            v_rdata := mem(v_addr+3) & mem(v_addr+2) & mem(v_addr+1) & mem(v_addr);
            iss.complete_load(v_rdata);
          end if;
        end if;
      end if;
      
      -- Small delay to avoid delta cycles issues with sim_end
      wait for 1 ns;
    end loop;
    wait;
  end process;

  -- Main Sequencer
  process
  begin
    -- UVVM Setup
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    
    if USE_ISS then
      log(ID_LOG_HDR, "Starting Simulation of WardRV (ISS MODE)");
    else
      log(ID_LOG_HDR, "Starting Simulation of WardRV (RTL MODE)");
    end if;

    -- Wait for reset deassertion
    wait until arst_b_i = '1';

    if USE_ISS then
      wait until sim_end_iss for 1 ms;
    else
      wait until sim_end_rtl for 1 ms;
    end if;

    if not (sim_end_iss or sim_end_rtl) then
      alert(TB_ERROR, "Simulation Timeout");
    end if;

    report_alert_counters(FINAL);
    std.env.stop;

  end process;

  -- Memory Access Process (Dual Port behavior simulation)
  process(clk_i)
    variable i_addr : integer;
    variable d_addr : integer;
    variable v_inst : std_logic_vector(31 downto 0);
  begin
    if not USE_ISS then
      if rising_edge(clk_i) then
        -- Instruction Fetch
        inst_tgt.ready <= '0';
        inst_tgt.inst  <= (others => '0');
        
        if inst_ini.valid = '1' then
          if unsigned(inst_ini.addr) < MEM_SIZE - 3 then
            i_addr := to_integer(unsigned(inst_ini.addr));
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
          
          -- Check for TOHOST (Simulation Exit)
          -- Assuming writing to a specific high address signals end
          if sbi_ini.addr = TOHOST_ADDR and sbi_ini.we = '1' then
             if to_integer(unsigned(sbi_ini.wdata)) = 1 then
               log(ID_LOG_HDR, "TEST PASSED (tohost = 1)");
             else
               alert(TB_ERROR, "TEST FAILED (tohost = " & integer'image(to_integer(unsigned(sbi_ini.wdata))) & ")");
             end if;
             sim_end_rtl <= true;
             
          elsif unsigned(sbi_ini.addr) < MEM_SIZE - 3 then
            d_addr := to_integer(unsigned(sbi_ini.addr));
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
    end if;
  end process;

end rtl;
