-------------------------------------------------------------------------------
-- Title      : WardRV ISS Testbench
-- Project    : 
-------------------------------------------------------------------------------
-- File       : tb_WardRV_iss.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Testbench for WardRV ISS (Instruction Set Simulator)
-------------------------------------------------------------------------------
-- Copyright (c) 2026
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
use work.tb_WardRV_pkg.all;

entity tb_WardRV_iss is
  generic (
    FIRMWARE_FILE  : string  := "firmware.hex";
    SIGNATURE_FILE : string  := "signature.output";
    VERBOSE        : boolean := false
  );
end tb_WardRV_iss;

architecture rtl of tb_WardRV_iss is

  -- Signals
  signal clk_i       : std_logic := '0';
  signal arst_b_i    : std_logic := '0';
  signal sim_end     : boolean   := false;

  signal mem : ram_t := init_ram(FIRMWARE_FILE);

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

  -- ISS Execution Loop
  p_iss : process
    variable iss       : iss_t;
    variable v_inst    : std_logic_vector(31 downto 0);
    variable v_addr    : integer;
    variable v_mem_req : boolean;
    variable v_we      : std_logic;
    variable v_maddr   : std_logic_vector(31 downto 0);
    variable v_maddr_tmp   : std_logic_vector(31 downto 0);
    variable v_wdata   : std_logic_vector(31 downto 0);
    variable v_be      : std_logic_vector(3 downto 0);
    variable v_rdata   : std_logic_vector(31 downto 0);

  begin
    iss.reset(x"00000000");
    wait until arst_b_i = '1';

    while not sim_end loop
      wait until rising_edge(clk_i);
      
      -- Fetch
      v_addr := to_integer(unsigned(iss.get_pc));
      if v_addr < C_MEM_SIZE - 3 then
        v_inst := mem(v_addr+3) & mem(v_addr+2) & mem(v_addr+1) & mem(v_addr);
      else
        v_inst := (others => '0');
      end if;
      
      print_instruction(iss.get_pc, v_inst, VERBOSE);

      -- Execute
      iss.execute_instruction(v_inst, v_mem_req, v_we, v_maddr, v_wdata, v_be);

      -- Handle Memory
      if v_mem_req then
        if v_maddr = C_TOHOST_ADDR and v_we = '1' then
          if to_integer(unsigned(v_wdata)) = 1 then
            log(ID_LOG_HDR, "ISS: TEST PASSED");
          else
            alert(TB_ERROR, "ISS: TEST FAILED");
          end if;

          iss.print_stats;

          dump_signature(SIGNATURE_FILE, C_SIGNATURE_ADDR, C_MEM_SIZE, mem);
          sim_end <= true;

        else
          v_maddr_tmp := v_maddr(31 downto 2) & std_logic_vector'("00");
          v_addr      := to_integer(unsigned(v_maddr_tmp));
          if v_addr < C_MEM_SIZE - 3 then
            if v_we = '1' then
              if VERBOSE then log(ID_BFM, "ISS Store @ 0x" & to_hstring(v_maddr) & " : 0x" & to_hstring(v_wdata) & " (be:" & to_string(v_be) & ")"); end if;
              if v_be(0) = '1' then mem(v_addr)   <= v_wdata(7 downto 0); end if;
              if v_be(1) = '1' then mem(v_addr+1) <= v_wdata(15 downto 8); end if;
              if v_be(2) = '1' then mem(v_addr+2) <= v_wdata(23 downto 16); end if;
              if v_be(3) = '1' then mem(v_addr+3) <= v_wdata(31 downto 24); end if;
            else
              v_rdata := mem(v_addr+3) & mem(v_addr+2) & mem(v_addr+1) & mem(v_addr);
              if VERBOSE then log(ID_BFM, "ISS Load  @ 0x" & to_hstring(v_maddr) & " : 0x" & to_hstring(v_rdata)); end if;
              iss.complete_load(v_rdata);
            end if;
          else
            if VERBOSE then log(ID_BFM, "ISS Access Out of Bounds @ 0x" & to_hstring(v_maddr)); end if;
            if v_we = '0' then
              iss.complete_load((others => '0'));
            end if;
          end if;
        end if;
      end if;
      
      -- Small delay to avoid delta cycles issues with sim_end
      wait until falling_edge(clk_i);
    end loop;



    wait;
  end process;

  -- Main Sequencer
  process
  begin
    -- UVVM Setup
    report_global_ctrl(VOID);
    enable_log_msg(ALL_MESSAGES);
    
    log(ID_LOG_HDR, "Starting Simulation of WardRV (ISS Only)");

    -- Wait for reset deassertion
    wait until arst_b_i = '1';

    wait until sim_end for 1 ms;

    if not sim_end then
      alert(TB_ERROR, "Simulation Timeout");
    end if;

    report_alert_counters(FINAL);
    std.env.stop;

  end process;


end rtl;