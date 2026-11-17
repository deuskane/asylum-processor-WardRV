-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_dbg_dm_mem.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Debug memory of the Debug Module (execution-based debug, Debug Spec 1.0
-- chapter 3.7.1 and appendix A.2) : what the hart executes and accesses in
-- debug mode at 0xFFFF_F800 (map in WardRV_debug_pkg). Combinational, no
-- wait state (ready = valid).
--  * debug ROM (WardRV_dbg_rom_pkg, from esw/debug_rom/debug_rom.S)
--  * GO / RESUME flags read by the ROM, HALTED / RESUMING strobes written
--    by the ROM
--  * abstract command program, built from the command register (Access
--    Register only), followed by the program buffer and an ebreak
--  * data registers (read and written by the abstract command program)
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
use     asylum.RV_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;
use     asylum.WardRV_dbg_rom_pkg.all;

entity WardRV_dbg_dm_mem is
  generic (
    PROGBUF_SIZE   : natural  := 2                                -- Program buffer (words)
   ;DATA_COUNT     : positive := 2                                -- Data registers
  );
  port (
    -- Hart (debug mode)
    dbg_mem_ini_i  : in  dmem_ini_t
   ;dbg_mem_tgt_o  : out dmem_tgt_t
    -- From the Debug Module
   ;cmd_i          : in  std_logic_vector(32-1 downto 0)          -- command register
   ;progbuf_i      : in  dbg_words_t                              -- At least PROGBUF_SIZE words
   ;data_i         : in  dbg_words_t(0 to DATA_COUNT-1)
   ;go_i           : in  std_logic
   ;resume_i       : in  std_logic
    -- Writes of the hart (one cycle)
   ;halted_o       : out std_logic
   ;resuming_o     : out std_logic
   ;data_we_o      : out std_logic
   ;data_idx_o     : out natural range 0 to DATA_COUNT-1
   ;data_wdata_o   : out std_logic_vector(32-1 downto 0)
  );
end entity WardRV_dbg_dm_mem;

architecture rtl of WardRV_dbg_dm_mem is

  -- Word index of each region
  constant WI_HALTED   : natural := DBG_HALTED   / 4;
  constant WI_RESUMING : natural := DBG_RESUMING / 4;
  constant WI_GO       : natural := DBG_GO       / 4;
  constant WI_RESUME   : natural := DBG_RESUME   / 4;
  constant WI_ABSTRACT : natural := DBG_ABSTRACT / 4;
  constant WI_PROGBUF  : natural := DBG_PROGBUF  / 4;
  constant WI_DATA     : natural := DBG_DATA     / 4;

  -- x0-relative address of data0
  constant DATA0       : integer := DBG_DATA - 16#800#;
  constant S0          : natural := 8;

  constant NOP         : std_logic_vector(32-1 downto 0) := rv_i_type(0, 0, F3_ADD , 0, OPC_OP_IMM);
  constant EBREAK      : std_logic_vector(32-1 downto 0) := rv_i_type(1, 0, F3_PRIV, 0, OPC_SYSTEM);

  -----------------------------------------------------------------------------
  -- Abstract command program of an Access Register command (5 words) :
  --   GPR read  : sw   xN, data0(zero)
  --   GPR write : lw   xN, data0(zero)
  --   CSR read  : csrw dscratch0, s0 ; csrr s0, csr ; sw s0, data0(zero)
  --               csrr s0, dscratch0
  --   CSR write : csrw dscratch0, s0 ; lw s0, data0(zero) ; csrw csr, s0
  --               csrr s0, dscratch0
  -- then ebreak, or a nop that falls into the program buffer (postexec).
  -- The Debug Module only starts supported commands : regno is a GPR
  -- (0x1000-0x101F, bit 12 set) or a CSR (0x0000-0x0FFF).
  -----------------------------------------------------------------------------
  function abstract_program(cmd : std_logic_vector(32-1 downto 0)) return dbg_words_t is
    alias    postexec  : std_logic is cmd(18);
    alias    transfer  : std_logic is cmd(17);
    alias    cmd_write : std_logic is cmd(16);
    alias    is_gpr    : std_logic is cmd(12);
    constant GPR       : natural := to_integer(unsigned(cmd(4 downto 0)));
    constant CSR       : natural := to_integer(unsigned(cmd(11 downto 0)));
    constant DSCRATCH0 : natural := to_integer(unsigned(CSR_DSCRATCH0));
    variable p         : dbg_words_t(0 to DBG_ABSTRACT_SIZE-1) := (others => NOP);
  begin
    if transfer = '1' and is_gpr = '1'
    then
      if cmd_write = '0'
      then
        p(0) := rv_s_type(DATA0, GPR, 0, F3_SW, OPC_STORE);
      else
        p(0) := rv_i_type(DATA0, 0, F3_LW, GPR, OPC_LOAD);
      end if;
    elsif transfer = '1'
    then
      p(0) := rv_i_type(DSCRATCH0, S0, F3_CSRRW, 0, OPC_SYSTEM);
      if cmd_write = '0'
      then
        p(1) := rv_i_type(CSR, 0, F3_CSRRS, S0, OPC_SYSTEM);
        p(2) := rv_s_type(DATA0, S0, 0, F3_SW, OPC_STORE);
      else
        p(1) := rv_i_type(DATA0, 0, F3_LW, S0, OPC_LOAD);
        p(2) := rv_i_type(CSR, S0, F3_CSRRW, 0, OPC_SYSTEM);
      end if;
      p(3) := rv_i_type(DSCRATCH0, 0, F3_CSRRS, S0, OPC_SYSTEM);
    end if;
    if postexec = '0'
    then
      p(4) := EBREAK;
    end if;
    return p;
  end function abstract_program;

  signal   wi          : natural range 0 to 2**(DBG_MEM_AWIDTH-2)-1;
  signal   write       : std_logic;
  signal   abstract    : dbg_words_t(0 to DBG_ABSTRACT_SIZE-1);

begin  -- architecture rtl

  wi       <= to_integer(unsigned(dbg_mem_ini_i.addr(DBG_MEM_AWIDTH-1 downto 2)));
  write    <= dbg_mem_ini_i.valid and dbg_mem_ini_i.we;
  abstract <= abstract_program(cmd_i);

  -----------------------------------------------------------------------------
  -- Read
  -----------------------------------------------------------------------------
  process(all)
  begin
    dbg_mem_tgt_o.rdata <= (others => '0');
    if    wi < DBG_ROM_CONTENT'length
    then
      dbg_mem_tgt_o.rdata    <= DBG_ROM_CONTENT(wi);
    elsif wi = WI_GO
    then
      dbg_mem_tgt_o.rdata(0) <= go_i;
    elsif wi = WI_RESUME
    then
      dbg_mem_tgt_o.rdata(0) <= resume_i;
    elsif wi >= WI_ABSTRACT and wi < WI_ABSTRACT + DBG_ABSTRACT_SIZE
    then
      dbg_mem_tgt_o.rdata    <= abstract(wi - WI_ABSTRACT);
    elsif wi >= WI_PROGBUF  and wi < WI_PROGBUF  + PROGBUF_SIZE
    then
      dbg_mem_tgt_o.rdata    <= progbuf_i(wi - WI_PROGBUF);
    elsif wi  = WI_PROGBUF  + PROGBUF_SIZE
    then
      -- Implicit ebreak after the program buffer
      dbg_mem_tgt_o.rdata    <= EBREAK;
    elsif wi >= WI_DATA     and wi < WI_DATA     + DATA_COUNT
    then
      dbg_mem_tgt_o.rdata    <= data_i(wi - WI_DATA);
    end if;
  end process;

  dbg_mem_tgt_o.ready <= dbg_mem_ini_i.valid;
  dbg_mem_tgt_o.err   <= '0';

  -----------------------------------------------------------------------------
  -- Writes (full words only, as the ROM and the abstract programs do)
  -----------------------------------------------------------------------------
  halted_o     <= write when wi = WI_HALTED   else '0';
  resuming_o   <= write when wi = WI_RESUMING else '0';
  data_we_o    <= write when wi >= WI_DATA and wi < WI_DATA + DATA_COUNT else '0';
  data_idx_o   <= wi - WI_DATA when wi >= WI_DATA and wi < WI_DATA + DATA_COUNT else 0;
  data_wdata_o <= dbg_mem_ini_i.wdata;

end architecture rtl;
