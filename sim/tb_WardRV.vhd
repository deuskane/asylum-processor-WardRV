-------------------------------------------------------------------------------
-- Title      : WardRV ISS Testbench
-- Project    : 
-------------------------------------------------------------------------------
-- File       : tb_WardRV.vhd
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

entity tb_WardRV is
  generic (
    FIRMWARE_FILE  : string  := "firmware.hex";
    SIGNATURE_FILE : string  := "";
    GOLDEN_FILE    : string  := "";
    VERBOSE        : boolean := false
  );
end tb_WardRV;

architecture rtl of tb_WardRV is

  -- Signals
  signal clk_i       : std_logic := '0';
  signal arst_b_i    : std_logic := '0';
  signal sim_end     : boolean   := false;

  signal mem : ram_t ;

  -- ISS Interface
  signal inst_ini : inst_ini_t;
  signal inst_tgt : inst_tgt_t;
  signal sbi_ini  : sbi_ini_t;
  signal sbi_tgt  : sbi_tgt_t;

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

  -- ISS Instance
  dut : entity asylum.WardRV_iss
    generic map (
      RESET_ADDR => C_FIRMWARE_ADDR,
      VERBOSE    => VERBOSE
    )
    port map (
      clk_i      => clk_i,
      arst_b_i   => arst_b_i,
      inst_ini_o => inst_ini,
      inst_tgt_i => inst_tgt,
      sbi_ini_o  => sbi_ini,
      sbi_tgt_i  => sbi_tgt
    );

  -- Memory Responder
  p_mem : process
    variable v_rdata : std_logic_vector(31 downto 0);
  begin
    log(ID_LOG_HDR, "Starting ISS Execution Loop");

    init_ram(FIRMWARE_FILE, mem);
    
    -- Initialize outputs
    inst_tgt.ready <= '0';
    inst_tgt.inst  <= (others => '0');
    sbi_tgt.ready  <= '0';
    sbi_tgt.rdata  <= (others => '0');

    wait until arst_b_i = '1';

    while not sim_end loop
      wait until rising_edge(clk_i);
      
      -- Default Ready
      inst_tgt.ready <= '0';
      sbi_tgt.ready  <= '0';
      
      -- Handle Instruction Fetch
      if inst_ini.valid = '1' then
        if unsigned(inst_ini.addr) >= unsigned(C_FIRMWARE_ADDR) and unsigned(inst_ini.addr) < unsigned(C_FIRMWARE_ADDR) + C_MEM_SIZE - 3 then
          read_mem(mem, inst_ini.addr, v_rdata, VERBOSE);
        else
          v_rdata := (others => '0');
        end if;
        inst_tgt.inst  <= v_rdata;
        inst_tgt.ready <= '1';
        print_instruction(inst_ini.addr, v_rdata, VERBOSE);
      end if;
      
      -- Handle Data Access
      if sbi_ini.valid = '1' then
        if sbi_ini.we = '1' then
          -- Write
          if sbi_ini.addr = C_TOHOST_ADDR then
            if sbi_ini.wdata = C_TOHOST_DATA_OK then
              log(ID_LOG_HDR, "ISS: TEST PASSED");
            else
              alert(TB_ERROR, "ISS: TEST FAILED");
            end if;
            
            if SIGNATURE_FILE /= "" then
              dump_signature(SIGNATURE_FILE, std_logic_vector(unsigned(C_SIGNATURE_ADDR) - unsigned(C_FIRMWARE_ADDR)), C_MEM_SIZE, mem);
              if GOLDEN_FILE /= "" then
                compare_signature(SIGNATURE_FILE, GOLDEN_FILE);
              end if;
            end if;
            sim_end <= true;
          else
            write_mem(mem, sbi_ini.addr, sbi_ini.wdata, sbi_ini.be, VERBOSE);
          end if;
        else
          -- Read
          read_mem(mem, sbi_ini.addr, v_rdata, VERBOSE);
          sbi_tgt.rdata <= v_rdata;
        end if;
        sbi_tgt.ready <= '1';
      end if;
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