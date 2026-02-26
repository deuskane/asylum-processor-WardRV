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
    SIGNATURE_FILE : string  := "";
    GOLDEN_FILE    : string  := "";
    VERBOSE        : boolean := false
  );
end tb_WardRV_iss;

architecture rtl of tb_WardRV_iss is

  -- Signals
  signal clk_i       : std_logic := '0';
  signal arst_b_i    : std_logic := '0';
  signal sim_end     : boolean   := false;

  signal mem : ram_t ;

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
    variable v_iaccess : boolean;
    variable v_irdata  : std_logic_vector(31 downto 0);
    variable v_iaddr   : std_logic_vector(31 downto 0);
    variable v_daccess : boolean;
    variable v_dwe     : std_logic;
    variable v_daddr   : std_logic_vector(31 downto 0);
    variable v_dwdata  : std_logic_vector(31 downto 0);
    variable v_dbe     : std_logic_vector(3 downto 0);
    variable v_drdata  : std_logic_vector(31 downto 0);

  begin
    init_ram       (FIRMWARE_FILE, mem);
    iss.reset      (C_FIRMWARE_ADDR);
    iss.set_verbose(true);

    wait until arst_b_i = '1';

    while not sim_end 
    loop
      wait until rising_edge(clk_i);
      
      -- Fetch
      v_iaccess := true;
      v_iaddr   := iss.get_pc;
      
      if v_iaccess 
      then
        if unsigned(v_iaddr) >= unsigned(C_FIRMWARE_ADDR) and unsigned(v_iaddr) < unsigned(C_FIRMWARE_ADDR) + C_MEM_SIZE - 3 
        then
          read_mem(mem, v_iaddr, v_irdata, VERBOSE);

        else
          v_irdata := (others => '0');
        end if;

        print_instruction(iss.get_pc, v_irdata, VERBOSE);
      end if;
      
      -- Execute
      iss.execute_instruction(v_irdata, v_daccess, v_dwe, v_daddr, v_dwdata, v_dbe);

      -- Handle Memory
      if v_daccess 
      then
        if v_daddr = C_TOHOST_ADDR and v_dwe = '1' 
        then
          if v_dwdata = C_TOHOST_DATA_OK
          then
            log(ID_LOG_HDR, "ISS: TEST PASSED");
          else
            alert(TB_ERROR, "ISS: TEST FAILED");
          end if;

          iss.stats("stats.txt");

          if SIGNATURE_FILE /= "" 
          then
            dump_signature(SIGNATURE_FILE, std_logic_vector(unsigned(C_SIGNATURE_ADDR) - unsigned(C_FIRMWARE_ADDR)), C_MEM_SIZE, mem);
            if GOLDEN_FILE /= "" 
            then
              compare_signature(SIGNATURE_FILE, GOLDEN_FILE);
            end if;
          end if;
          sim_end <= true;

        else
          if v_dwe = '1' 
          then
            write_mem(mem, v_daddr, v_dwdata, v_dbe, VERBOSE);
          else
            read_mem(mem, v_daddr, v_drdata, VERBOSE);
            iss.complete_load(v_drdata);
          end if;
        end if;
      end if;
      iss.complete;
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

    wait until sim_end for C_SIM_TIMEOUT;

    if not sim_end 
    then
      alert(TB_ERROR, "Simulation Timeout");
    end if;

    report_alert_counters(FINAL);
    std.env.stop;

  end process;


end rtl;