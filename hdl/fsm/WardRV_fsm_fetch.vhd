-------------------------------------------------------------------------------
-- Title      : WardRV Fetch Unit
-- Project    : WardRV
-------------------------------------------------------------------------------
-- Description: 
-- This module handles the Instruction Fetch (IF) stage.
-- It manages the handshake with the instruction memory and latches the 
-- incoming instruction into an Instruction Register (IR).
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-04-06  1.0      mrosiere Created
-------------------------------------------------------------------------------
library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;

entity WardRV_fsm_fetch is
  port (
    clk_i           : in  std_logic;
    arst_b_i        : in  std_logic;

    -- Control from FSM
    imem_valid_i    : in  std_logic;                     -- FSM requests a new fetch
    pc_i            : in  std_logic_vector(31 downto 0); -- Address to fetch from
    
    -- Status to FSM
    imem_ready_o    : out std_logic;                     -- Memory acknowledged the request
    inst_r_o        : out std_logic_vector(31 downto 0); -- The fetched instruction

    -- Instruction Memory Interface (Bus)
    inst_ini_o      : out inst_ini_t;
    inst_tgt_i      : in  inst_tgt_t
  );
end entity WardRV_fsm_fetch;

architecture behavioural of WardRV_fsm_fetch is
  -- Internal register to track the active request on the bus.
  -- In many bus protocols, the 'valid' signal must remain stable until 'ready' is received.
  signal imem_valid_r : std_logic;
begin

  --------------------------------------------------------------------
  -- Bus Interface Mapping
  --------------------------------------------------------------------
  -- We directly drive the address from the PC.
  inst_ini_o.addr  <= pc_i;
  inst_ini_o.valid <= imem_valid_r;
  
  -- Expose the memory ready signal to the FSM so it knows when to transition states.
  imem_ready_o     <= inst_tgt_i.ready;

  --------------------------------------------------------------------
  -- Request Logic & Instruction Latch
  --------------------------------------------------------------------
  -- This process manages the lifecycle of a fetch request.
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      imem_valid_r <= '0';
      inst_r_o     <= (others => '0');
      
    elsif rising_edge(clk_i) then
      -- Sync the internal valid register with the FSM's request.
      imem_valid_r <= imem_valid_i;

      -- Data Capture (The "Latch"):
      -- We capture the data only when both the master (this module) and 
      -- the slave (memory) agree that the bus transaction is valid and ready.
      -- This is the standard "valid/ready" handshake pattern.
      if (imem_valid_r = '1') and (inst_tgt_i.ready = '1') then
        inst_r_o <= inst_tgt_i.inst;
      end if;
      
    end if;
  end process;

end architecture behavioural;