-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_fsm_debug_bus.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Access path to the debug memory of the Debug Module (Debug Spec 1.0,
-- execution-based debug) : in debug mode, the instruction fetches and the
-- loads / stores of 0xFFFF_F800 - 0xFFFF_FFFF go to dbg_mem (the debug ROM,
-- the abstract commands, the program buffer, the data registers of
-- WardRV_dbg_dm) ; every other access keeps the imem / dmem interfaces. The
-- FSM never fetches and accesses data at the same time.
-- Without DEBUG_ENABLE, imem / dmem are wires and dbg_mem is idle.
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-11-17  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV_fsm_debug_bus is
  generic (
    DEBUG_ENABLE   : boolean := false
  );
  port (
    debug_mode_i   : in  std_logic
    -- Fetch and memory units of the core
   ;core_imem_ini_i: in  imem_ini_t
   ;core_imem_tgt_o: out imem_tgt_t
   ;core_dmem_ini_i: in  dmem_ini_t
   ;core_dmem_tgt_o: out dmem_tgt_t
    -- Interfaces of the core
   ;imem_ini_o     : out imem_ini_t
   ;imem_tgt_i     : in  imem_tgt_t
   ;dmem_ini_o     : out dmem_ini_t
   ;dmem_tgt_i     : in  dmem_tgt_t
    -- Debug memory (Debug Module)
   ;dbg_mem_ini_o  : out dmem_ini_t
   ;dbg_mem_tgt_i  : in  dmem_tgt_t
  );
end entity WardRV_fsm_debug_bus;

architecture rtl of WardRV_fsm_debug_bus is

  signal   imem_dbg  : std_logic;                                 -- Fetch from the debug memory
  signal   dmem_dbg  : std_logic;                                 -- Load / store in the debug memory

begin  -- architecture rtl

  gen_debug : if DEBUG_ENABLE
  generate
    imem_dbg <= debug_mode_i when core_imem_ini_i.addr(32-1 downto DBG_MEM_AWIDTH) = DBG_MEM_BASE(32-1 downto DBG_MEM_AWIDTH) else '0';
    dmem_dbg <= debug_mode_i when core_dmem_ini_i.addr(32-1 downto DBG_MEM_AWIDTH) = DBG_MEM_BASE(32-1 downto DBG_MEM_AWIDTH) else '0';
  end generate gen_debug;

  gen_debug_b : if not DEBUG_ENABLE
  generate
    imem_dbg <= '0';
    dmem_dbg <= '0';
  end generate gen_debug_b;

  -- Instruction fetch
  imem_ini_o.valid      <= core_imem_ini_i.valid and not imem_dbg;
  imem_ini_o.addr       <= core_imem_ini_i.addr;
  core_imem_tgt_o.ready <= dbg_mem_tgt_i.ready when imem_dbg = '1' else imem_tgt_i.ready;
  core_imem_tgt_o.inst  <= dbg_mem_tgt_i.rdata when imem_dbg = '1' else imem_tgt_i.inst;

  -- Load / store
  dmem_ini_o.valid      <= core_dmem_ini_i.valid and not dmem_dbg;
  dmem_ini_o.addr       <= core_dmem_ini_i.addr;
  dmem_ini_o.wdata      <= core_dmem_ini_i.wdata;
  dmem_ini_o.we         <= core_dmem_ini_i.we;
  dmem_ini_o.be         <= core_dmem_ini_i.be;
  core_dmem_tgt_o       <= dbg_mem_tgt_i       when dmem_dbg = '1' else dmem_tgt_i;

  -- Debug memory
  dbg_mem_ini_o.valid   <= (core_imem_ini_i.valid and imem_dbg) or (core_dmem_ini_i.valid and dmem_dbg);
  dbg_mem_ini_o.addr    <= core_dmem_ini_i.addr when core_dmem_ini_i.valid = '1' else core_imem_ini_i.addr;
  dbg_mem_ini_o.wdata   <= core_dmem_ini_i.wdata;
  dbg_mem_ini_o.we      <= core_dmem_ini_i.we and core_dmem_ini_i.valid;  -- A fetch is a read
  dbg_mem_ini_o.be      <= core_dmem_ini_i.be;

end architecture rtl;
