-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_debug_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Constants and types of the WardRV debug unit (RISC-V Debug Spec 1.0) :
--  * JTAG instructions of the TAP / DTM (spec 6.1)
--  * DMI request and DM register addresses (spec 3.14)
--  * Hart requests (DM -> core)
--  * Memory map of the debug memory, seen by the core in debug mode only
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-11-17  2.0      mrosiere Rewrite : Debug Spec 1.0
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;

library asylum;
use     asylum.WardRV_pkg.all;

package WardRV_debug_pkg is

  -- ===========================================================================
  -- JTAG instructions (5-bit IR)
  -- ===========================================================================
  constant JTAG_IR_WIDTH     : natural := 5;
  constant JTAG_IR_IDCODE    : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "00001";
  constant JTAG_IR_DTMCS     : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "10000";
  constant JTAG_IR_DMI       : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "10001";
  constant JTAG_IR_BYPASS    : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "11111";

  -- ===========================================================================
  -- DMI : one request at a time, the Debug Module answers in the cycle of the
  -- request (read data on dmi_rdata, no error)
  -- ===========================================================================
  constant DMI_ABITS         : natural := 7;

  type dmi_req_t is record
    valid        : std_logic;                              -- One cycle
    we           : std_logic;                              -- 1 : write, 0 : read
    addr         : std_logic_vector(DMI_ABITS-1 downto 0);
    wdata        : std_logic_vector(32-1 downto 0);
  end record dmi_req_t;

  -- DM registers
  constant DM_DATA0          : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"04";
  constant DM_DMCONTROL      : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"10";
  constant DM_DMSTATUS       : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"11";
  constant DM_HARTINFO       : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"12";
  constant DM_ABSTRACTCS     : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"16";
  constant DM_COMMAND        : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"17";
  constant DM_ABSTRACTAUTO   : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"18";
  constant DM_PROGBUF0       : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"20";
  constant DM_SBCS           : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"38";
  constant DM_SBADDRESS0     : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"39";
  constant DM_SBDATA0        : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"3C";
  constant DM_HALTSUM0       : std_logic_vector(DMI_ABITS-1 downto 0) := 7x"40";

  -- abstractcs.cmderr
  constant CMDERR_NONE       : std_logic_vector(2 downto 0) := "000";
  constant CMDERR_BUSY       : std_logic_vector(2 downto 0) := "001";
  constant CMDERR_NOTSUP     : std_logic_vector(2 downto 0) := "010";
  constant CMDERR_EXCEPTION  : std_logic_vector(2 downto 0) := "011";
  constant CMDERR_HALTRESUME : std_logic_vector(2 downto 0) := "100";

  -- dcsr.cause
  constant DCSR_CAUSE_EBREAK       : std_logic_vector(2 downto 0) := "001";
  constant DCSR_CAUSE_TRIGGER      : std_logic_vector(2 downto 0) := "010";
  constant DCSR_CAUSE_HALTREQ      : std_logic_vector(2 downto 0) := "011";
  constant DCSR_CAUSE_STEP         : std_logic_vector(2 downto 0) := "100";
  constant DCSR_CAUSE_RESETHALTREQ : std_logic_vector(2 downto 0) := "101";

  -- ===========================================================================
  -- Hart requests (DM -> core, clk domain)
  -- ===========================================================================
  type dbg_hart_req_t is record
    haltreq      : std_logic;  -- Halt (level)
    resethaltreq : std_logic;  -- Halt before the first instruction after the hart reset
  end record dbg_hart_req_t;

  constant DBG_HART_REQ_NULL : dbg_hart_req_t := (haltreq => '0', resethaltreq => '0');

  -- ===========================================================================
  -- Debug memory : 2 KB at 0xFFFF_F800, seen by the core in debug mode only
  -- (dmem_ini_t / dmem_tgt_t, 32-bit words). Every word is reachable with a
  -- x0-relative load / store / jalr : immediate = offset - 0x800.
  -- esw/debug_rom/debug_rom.S uses the same offsets.
  -- ===========================================================================
  constant DBG_MEM_BASE      : std_logic_vector(32-1 downto 0) := x"FFFFF800";
  constant DBG_MEM_AWIDTH    : natural := 11;

  -- Byte offsets (debug ROM, halt entry : 0x000)            -- Core access
  constant DBG_HALTED        : natural := 16#100#;  -- W   : the hart is parked in the ROM
  constant DBG_RESUMING      : natural := 16#104#;  -- W   : the hart resumes
  constant DBG_GO            : natural := 16#108#;  -- R   : 1 = execute the abstract command
  constant DBG_RESUME        : natural := 16#10C#;  -- R   : 1 = resume
  constant DBG_ABSTRACT      : natural := 16#200#;  -- R   : abstract command program
  constant DBG_PROGBUF       : natural := 16#214#;  -- R   : program buffer, then ebreak
  constant DBG_DATA          : natural := 16#300#;  -- R/W : data registers

  constant DBG_ABSTRACT_SIZE : natural := 5;        -- Words
  constant DBG_PROGBUF_MAX   : natural := 16;       -- Words
  constant DBG_DATA_MAX      : natural := 12;       -- Words

  type dbg_words_t is array (natural range <>) of std_logic_vector(32-1 downto 0);

  -- Idle dmem target (default of the unconnected debug ports)
  constant DMEM_TGT_NULL     : dmem_tgt_t := (ready => '0', rdata => (others => '0'), err => '0');

end package WardRV_debug_pkg;
