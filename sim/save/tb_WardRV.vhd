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

library uvvm_util;
context uvvm_util.uvvm_util_context;

use work.WardRV_vips.all;
use work.tb_WardRV_pkg.all;

entity tb_WardRV is
  generic (
    FIRMWARE_FILE  : string  := "firmware.hex";
    SIGNATURE_FILE : string  := "signature.output";
    GOLDEN_FILE    : string  := "";
    VERBOSE        : boolean := false
  );
end tb_WardRV;

architecture rtl of tb_WardRV is

  -- Signals
  signal clk_i       : std_logic := '0';
  signal arst_b_i    : std_logic := '0';
  signal sim_end_rtl : boolean   := false;

  -- Interfaces
  signal imem_ini    : imem_ini_t;
  signal imem_tgt    : imem_tgt_t;
  signal dmem_ini     : dmem_ini_t;
  signal dmem_tgt     : dmem_tgt_t;
  signal it_val      : std_logic := '0';
  signal it_ack      : std_logic;

  -- JTAG
  signal jtag_ini    : jtag_ini_t := (tck => '0', trst_n => '0', tms => '0', tdi => '0');
  signal jtag_tgt    : jtag_tgt_t;

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

begin

  -- Clock Generation
  clock_generator(clk_i, C_CLK_PERIOD);

  -- Reset Generation
  p_reset : process
  begin
    vip_reset_pulse(arst_b_i, 5 * C_CLK_PERIOD, "System Reset");
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
      imem_ini_o => imem_ini,
      imem_tgt_i => imem_tgt,
      dmem_ini_o  => dmem_ini,
      dmem_tgt_i  => dmem_tgt
    --it_val_i   => it_val,
    --it_ack_o   => it_ack,
    --jtag_ini_i => jtag_ini,
    --jtag_tgt_o => jtag_tgt
    );

  -- Main Sequencer
  process
  begin
    -- UVVM Setup
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    
    log(ID_LOG_HDR, "Starting Simulation of WardRV (RTL MODE)");

    -- Wait for reset deassertion
    wait until arst_b_i = '1';

    wait until sim_end_rtl for 1 ms;

    if not sim_end_rtl then
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
    if rising_edge(clk_i) then
      -- Instruction Fetch
      imem_tgt.ready <= '0';
      imem_tgt.inst  <= (others => '0');
      
      if imem_ini.valid = '1' then
        if unsigned(imem_ini.addr) < C_MEM_SIZE - 3 then
          i_addr := to_integer(unsigned(imem_ini.addr));
          v_inst := mem(i_addr+3) & mem(i_addr+2) & mem(i_addr+1) & mem(i_addr);
          imem_tgt.inst <= v_inst;
          print_instruction(imem_ini.addr, v_inst, VERBOSE);
          imem_tgt.ready <= '1';
        else
          -- Out of bounds fetch returns 0 (NOP/Illegal)
          imem_tgt.ready <= '1';
        end if;
      end if;

      -- Data Access
      dmem_tgt.ready <= '0';
      dmem_tgt.rdata <= (others => '0');
      dmem_tgt.err   <= '0';

      if dmem_ini.valid = '1' then
        
        -- Check for TOHOST (Simulation Exit)
        -- Assuming writing to a specific high address signals end
        if dmem_ini.addr = C_TOHOST_ADDR and dmem_ini.we = '1' then
           if dmem_ini.wdata = C_TOHOST_DATA_OK
           then
             log(ID_LOG_HDR, "RTL: TEST PASSED");
           else
             alert(TB_ERROR, "RTL: TEST FAILED");
           end if;

           if SIGNATURE_FILE /= "" 
           then
             dump_signature(SIGNATURE_FILE, std_logic_vector(unsigned(C_SIGNATURE_ADDR) - unsigned(C_FIRMWARE_ADDR)), C_MEM_SIZE, mem);
             if GOLDEN_FILE /= "" 
             then
               compare_signature(SIGNATURE_FILE, GOLDEN_FILE);
             end if;
           end if;
           sim_end_rtl <= true;
           
        elsif unsigned(dmem_ini.addr) < C_MEM_SIZE - 3 then
          d_addr := to_integer(unsigned(dmem_ini.addr));
          dmem_tgt.ready <= '1';
          
          -- Write
          if dmem_ini.we = '1' then
            if dmem_ini.be(0) = '1' then mem(d_addr)   <= dmem_ini.wdata(7 downto 0); end if;
            if dmem_ini.be(1) = '1' then mem(d_addr+1) <= dmem_ini.wdata(15 downto 8); end if;
            if dmem_ini.be(2) = '1' then mem(d_addr+2) <= dmem_ini.wdata(23 downto 16); end if;
            if dmem_ini.be(3) = '1' then mem(d_addr+3) <= dmem_ini.wdata(31 downto 24); end if;
          
          -- Read
          else
            dmem_tgt.rdata(7 downto 0)   <= mem(d_addr);
            dmem_tgt.rdata(15 downto 8)  <= mem(d_addr+1);
            dmem_tgt.rdata(23 downto 16) <= mem(d_addr+2);
            dmem_tgt.rdata(31 downto 24) <= mem(d_addr+3);
          end if;
        else
          -- Out of bounds access
          dmem_tgt.err   <= '1';
          dmem_tgt.ready <= '1';
        end if;
      end if;
    end if;
  end process;

end rtl;
