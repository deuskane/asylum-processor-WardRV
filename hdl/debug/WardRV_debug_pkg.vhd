-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    :
-------------------------------------------------------------------------------
-- File       : WardRV_debug_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description:
-- Package of the WardRV debug unit (RISC-V Debug Specification 1.0) :
--  * JTAG instruction codes (IEEE 1149.1 TAP + RISC-V DTM)
--  * DMI records (DTM -> DM request, DM -> DTM response) and constants
--  * Hart request record (DM -> core)
--  * Debug memory map seen by the core in debug mode (SBI target, 32 bits)
--  * RV32I instruction encoders used to build the abstract commands
--  * Components of the debug unit
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-10-09  2.0      mrosiere Rewrite : Debug Spec 1.0, DMI records,
--                               debug memory map, components
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.sbi_pkg.all;

package WardRV_debug_pkg is

  -- ===========================================================================
  -- JTAG (IEEE 1149.1 TAP + RISC-V DTM)
  -- ===========================================================================
  constant JTAG_IR_WIDTH     : natural := 5;
  constant JTAG_IR_IDCODE    : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "00001";
  constant JTAG_IR_DTMCS     : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "10000";
  constant JTAG_IR_DMI       : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "10001";
  constant JTAG_IR_BYPASS    : std_logic_vector(JTAG_IR_WIDTH-1 downto 0) := "11111";

  -- ===========================================================================
  -- DMI
  -- ===========================================================================
  constant DMI_ABITS         : natural := 7;
  constant DMI_DR_WIDTH      : natural := DMI_ABITS + 32 + 2; -- addr & data & op

  -- Request op (DTM -> DM)
  constant DMI_OP_NOP        : std_logic_vector(1 downto 0) := "00";
  constant DMI_OP_READ       : std_logic_vector(1 downto 0) := "01";
  constant DMI_OP_WRITE      : std_logic_vector(1 downto 0) := "10";

  -- Response op (DM -> DTM) and DTM sticky status (dtmcs.dmistat, dmi.op)
  constant DMI_RSP_SUCCESS   : std_logic_vector(1 downto 0) := "00";
  constant DMI_RSP_FAILED    : std_logic_vector(1 downto 0) := "10";
  constant DMI_RSP_BUSY      : std_logic_vector(1 downto 0) := "11";

  -- One transaction in flight. ready : request accepted (same cycle as valid)
  -- valid : response available (same cycle as ready or later, no backpressure)
  type dmi_req_t is record           -- DTM -> DM (clk domain)
    valid        : std_logic;
    op           : std_logic_vector(1 downto 0);
    addr         : std_logic_vector(DMI_ABITS-1 downto 0);
    data         : std_logic_vector(31 downto 0);
  end record dmi_req_t;

  type dmi_rsp_t is record           -- DM -> DTM
    ready        : std_logic;
    valid        : std_logic;
    op           : std_logic_vector(1 downto 0);
    data         : std_logic_vector(31 downto 0);
  end record dmi_rsp_t;

  constant DMI_REQ_NULL : dmi_req_t := (valid => '0', op => DMI_OP_NOP     , addr => (others => '0'), data => (others => '0'));
  constant DMI_RSP_NULL : dmi_rsp_t := (ready => '0', valid => '0', op => DMI_RSP_SUCCESS, data => (others => '0'));

  -- DM registers (DMI addresses)
  constant DM_DATA0          : std_logic_vector(DMI_ABITS-1 downto 0) := "0000100"; -- 0x04
  constant DM_DATA11         : std_logic_vector(DMI_ABITS-1 downto 0) := "0001111"; -- 0x0F
  constant DM_DMCONTROL      : std_logic_vector(DMI_ABITS-1 downto 0) := "0010000"; -- 0x10
  constant DM_DMSTATUS       : std_logic_vector(DMI_ABITS-1 downto 0) := "0010001"; -- 0x11
  constant DM_HARTINFO       : std_logic_vector(DMI_ABITS-1 downto 0) := "0010010"; -- 0x12
  constant DM_HALTSUM1       : std_logic_vector(DMI_ABITS-1 downto 0) := "0010011"; -- 0x13
  constant DM_ABSTRACTCS     : std_logic_vector(DMI_ABITS-1 downto 0) := "0010110"; -- 0x16
  constant DM_COMMAND        : std_logic_vector(DMI_ABITS-1 downto 0) := "0010111"; -- 0x17
  constant DM_ABSTRACTAUTO   : std_logic_vector(DMI_ABITS-1 downto 0) := "0011000"; -- 0x18
  constant DM_NEXTDM         : std_logic_vector(DMI_ABITS-1 downto 0) := "0011101"; -- 0x1D
  constant DM_PROGBUF0       : std_logic_vector(DMI_ABITS-1 downto 0) := "0100000"; -- 0x20
  constant DM_PROGBUF15      : std_logic_vector(DMI_ABITS-1 downto 0) := "0101111"; -- 0x2F
  constant DM_SBCS           : std_logic_vector(DMI_ABITS-1 downto 0) := "0111000"; -- 0x38
  constant DM_SBADDRESS0     : std_logic_vector(DMI_ABITS-1 downto 0) := "0111001"; -- 0x39
  constant DM_SBDATA0        : std_logic_vector(DMI_ABITS-1 downto 0) := "0111100"; -- 0x3C
  constant DM_HALTSUM0       : std_logic_vector(DMI_ABITS-1 downto 0) := "1000000"; -- 0x40

  -- Abstract command errors (abstractcs.cmderr)
  constant CMDERR_NONE       : std_logic_vector(2 downto 0) := "000";
  constant CMDERR_BUSY       : std_logic_vector(2 downto 0) := "001";
  constant CMDERR_NOTSUP     : std_logic_vector(2 downto 0) := "010";
  constant CMDERR_EXCEPTION  : std_logic_vector(2 downto 0) := "011";
  constant CMDERR_HALTRESUME : std_logic_vector(2 downto 0) := "100";
  constant CMDERR_BUS        : std_logic_vector(2 downto 0) := "101";

  -- dcsr.cause
  constant DCSR_CAUSE_EBREAK       : std_logic_vector(2 downto 0) := "001";
  constant DCSR_CAUSE_TRIGGER      : std_logic_vector(2 downto 0) := "010";
  constant DCSR_CAUSE_HALTREQ      : std_logic_vector(2 downto 0) := "011";
  constant DCSR_CAUSE_STEP         : std_logic_vector(2 downto 0) := "100";
  constant DCSR_CAUSE_RESETHALTREQ : std_logic_vector(2 downto 0) := "101";

  -- ===========================================================================
  -- Hart request (DM -> core, clk domain)
  -- ===========================================================================
  type dbg_hart_req_t is record
    haltreq      : std_logic; -- Halt request (level, dmcontrol.haltreq)
    resethaltreq : std_logic; -- Halt at the first instruction after the hart reset
  end record dbg_hart_req_t;

  constant DBG_HART_REQ_NULL : dbg_hart_req_t := (haltreq => '0', resethaltreq => '0');

  -- ===========================================================================
  -- Debug memory (SBI target seen by the core in debug mode only)
  -- 2 KB window at 0xFFFF_F800 : every word is reachable with a x0-relative
  -- load / store / jalr (12-bit immediate between -2048 and -1).
  -- The core writes only full words (sw).
  -- ===========================================================================
  constant DBG_MEM_AWIDTH      : natural := 11;
  constant DBG_MEM_BASE        : std_logic_vector(31 downto 0) := x"FFFFF800";

  constant DBG_OFFSET_HALT     : natural := 16#000#; -- ROM : halt entry
  constant DBG_OFFSET_EXC      : natural := 16#008#; -- ROM : exception entry
  constant DBG_OFFSET_HALTED   : natural := 16#100#; -- W   : hart is halted
  constant DBG_OFFSET_GOING    : natural := 16#104#; -- W   : hart executes the command
  constant DBG_OFFSET_RESUMING : natural := 16#108#; -- W   : hart resumes
  constant DBG_OFFSET_EXCEPTION: natural := 16#10C#; -- W   : exception in debug mode
  constant DBG_OFFSET_WHERETO  : natural := 16#200#; -- R   : jal to ABSTRACT
  constant DBG_OFFSET_ABSTRACT : natural := 16#300#; -- R   : abstract command program
  constant DBG_OFFSET_PROGBUF  : natural := 16#340#; -- R   : program buffer (+ impebreak)
  constant DBG_OFFSET_DATA     : natural := 16#380#; -- R/W : data registers
  constant DBG_OFFSET_FLAGS    : natural := 16#400#; -- R   : bit 0 go, bit 1 resume

  constant DBG_ROM_SIZE        : natural := 64;      -- words (0x000 - 0x0FF)
  constant DBG_ABSTRACT_SIZE   : natural := 8;       -- words
  constant DBG_PROGBUF_MAX     : natural := 16;      -- words
  constant DBG_DATA_MAX        : natural := 12;      -- words

  constant DBG_ADDR_HALT       : std_logic_vector(31 downto 0) := x"FFFFF800";
  constant DBG_ADDR_EXCEPTION  : std_logic_vector(31 downto 0) := x"FFFFF808";

  -- Debug memory bus : 11-bit byte address, 32-bit data
  subtype dbg_sbi_ini_t is sbi_ini_t(addr (DBG_MEM_AWIDTH-1 downto 0),
                                     wdata(31 downto 0));
  subtype dbg_sbi_tgt_t is sbi_tgt_t(rdata(31 downto 0));

  constant DBG_SBI_INI_NULL : dbg_sbi_ini_t := (cs => '0', re => '0', we => '0', addr => (others => '0'), wdata => (others => '0'));
  constant DBG_SBI_TGT_NULL : dbg_sbi_tgt_t := (ready => '0', rdata => (others => '0'), info => (name => (others => ' ')));

  type dbg_words_t is array (natural range <>) of std_logic_vector(31 downto 0);

  -- x0-relative immediate of a debug memory offset
  function dbg_imm(offset : natural) return integer;

  -- ===========================================================================
  -- RV32I encoders (abstract command generation)
  -- ===========================================================================
  constant RV_NOP    : std_logic_vector(31 downto 0) := x"00000013"; -- addi x0, x0, 0
  constant RV_EBREAK : std_logic_vector(31 downto 0) := x"00100073";
  constant RV_DRET   : std_logic_vector(31 downto 0) := x"7B200073";
  constant RV_X0     : std_logic_vector(4 downto 0)  := "00000";
  constant RV_S0     : std_logic_vector(4 downto 0)  := "01000";  -- x8

  function rv_lw    (rd  : std_logic_vector(4 downto 0); rs1 : std_logic_vector(4 downto 0); imm : integer) return std_logic_vector;
  function rv_sw    (rs2 : std_logic_vector(4 downto 0); rs1 : std_logic_vector(4 downto 0); imm : integer) return std_logic_vector;
  function rv_csrrw (rd  : std_logic_vector(4 downto 0); csr : std_logic_vector(11 downto 0); rs1 : std_logic_vector(4 downto 0)) return std_logic_vector;
  function rv_csrrs (rd  : std_logic_vector(4 downto 0); csr : std_logic_vector(11 downto 0); rs1 : std_logic_vector(4 downto 0)) return std_logic_vector;
  function rv_jal   (rd  : std_logic_vector(4 downto 0); imm : integer) return std_logic_vector;

-- [COMPONENT_INSERT][BEGIN]
component WardRV_dbg_tap is
  generic (
    IDCODE_VALUE : std_logic_vector(31 downto 0) := x"10000001";
    TRST_ENABLE  : boolean                       := true
  );
  port (
    arst_b_i     : in  std_logic;
    jtag_ini_i   : in  jtag_ini_t;
    jtag_tgt_o   : out jtag_tgt_t;
    tck_o        : out std_logic;
    trst_b_o     : out std_logic;
    tlr_o        : out std_logic;
    ir_o         : out std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
    capture_dr_o : out std_logic;
    shift_dr_o   : out std_logic;
    update_dr_o  : out std_logic;
    tdi_o        : out std_logic;
    user_tdo_i   : in  std_logic
  );
end component WardRV_dbg_tap;

component WardRV_dbg_dtm is
  generic (
    IDLE_CYCLES  : natural := 5
  );
  port (
    tck_i        : in  std_logic;
    trst_b_i     : in  std_logic;
    tlr_i        : in  std_logic;
    ir_i         : in  std_logic_vector(JTAG_IR_WIDTH-1 downto 0);
    capture_dr_i : in  std_logic;
    shift_dr_i   : in  std_logic;
    update_dr_i  : in  std_logic;
    tdi_i        : in  std_logic;
    tdo_o        : out std_logic;
    dmi_req_o    : out dmi_req_t;
    dmi_rsp_i    : in  dmi_rsp_t;
    dmi_busy_i   : in  std_logic
  );
end component WardRV_dbg_dtm;

component WardRV_dbg_dmi_cdc is
  port (
    tck_i        : in  std_logic;
    trst_b_i     : in  std_logic;
    tck_req_i    : in  dmi_req_t;
    tck_rsp_o    : out dmi_rsp_t;
    tck_busy_o   : out std_logic;
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    dmi_req_o    : out dmi_req_t;
    dmi_rsp_i    : in  dmi_rsp_t
  );
end component WardRV_dbg_dmi_cdc;

component WardRV_dbg_jtag is
  generic (
    IDCODE_VALUE    : std_logic_vector(31 downto 0) := x"10000001";
    TRST_ENABLE     : boolean                       := true;
    DTM_IDLE_CYCLES : natural                       := 5
  );
  port (
    jtag_ini_i   : in  jtag_ini_t;
    jtag_tgt_o   : out jtag_tgt_t;
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    dmi_req_o    : out dmi_req_t;
    dmi_rsp_i    : in  dmi_rsp_t
  );
end component WardRV_dbg_jtag;

component WardRV_dbg_dm_mem is
  generic (
    PROGBUF_SIZE : natural := 2;
    DATA_COUNT   : natural := 2;
    IMPEBREAK    : boolean := true
  );
  port (
    dbg_sbi_ini_i : in  dbg_sbi_ini_t;
    dbg_sbi_tgt_o : out dbg_sbi_tgt_t;
    abstract_i    : in  dbg_words_t(0 to DBG_ABSTRACT_SIZE-1);
    progbuf_i     : in  dbg_words_t(0 to DBG_PROGBUF_MAX-1);
    data_i        : in  dbg_words_t(0 to DBG_DATA_MAX-1);
    flag_go_i     : in  std_logic;
    flag_resume_i : in  std_logic;
    halted_o      : out std_logic;
    going_o       : out std_logic;
    resuming_o    : out std_logic;
    exception_o   : out std_logic;
    data_we_o     : out std_logic;
    data_idx_o    : out natural range 0 to DBG_DATA_MAX-1;
    data_wdata_o  : out std_logic_vector(31 downto 0)
  );
end component WardRV_dbg_dm_mem;

component WardRV_dbg_sba is
  generic (
    SBA_ADDR_WIDTH : positive := 32
  );
  port (
    clk_i         : in  std_logic;
    arst_b_i      : in  std_logic;
    dmactive_i    : in  std_logic;
    dmi_we_i      : in  std_logic;
    dmi_re_i      : in  std_logic;
    dmi_addr_i    : in  std_logic_vector(DMI_ABITS-1 downto 0);
    dmi_wdata_i   : in  std_logic_vector(31 downto 0);
    dmi_rdata_o   : out std_logic_vector(31 downto 0);
    sba_sbi_ini_o : out sbi_ini_t(addr (SBA_ADDR_WIDTH-1 downto 0),
                                  wdata(7 downto 0));
    sba_sbi_tgt_i : in  sbi_tgt_t(rdata(7 downto 0))
  );
end component WardRV_dbg_sba;

component WardRV_dbg_dm is
  generic (
    PROGBUF_SIZE        : natural                       := 2;
    DATA_COUNT          : natural                       := 2;
    IMPEBREAK           : boolean                       := true;
    SBA_ENABLE          : boolean                       := false;
    SBA_ADDR_WIDTH      : positive                      := 8;
    RESETHALTREQ_ENABLE : boolean                       := false;
    HARTID              : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    clk_i          : in  std_logic;
    arst_b_i       : in  std_logic;
    dmi_req_i      : in  dmi_req_t;
    dmi_rsp_o      : out dmi_rsp_t;
    hart_rst_b_i   : in  std_logic;
    dbg_hart_req_o : out dbg_hart_req_t;
    dbg_sbi_ini_i  : in  dbg_sbi_ini_t;
    dbg_sbi_tgt_o  : out dbg_sbi_tgt_t;
    ndmreset_o     : out std_logic;
    sba_sbi_ini_o  : out sbi_ini_t(addr (SBA_ADDR_WIDTH-1 downto 0),
                                   wdata(7 downto 0));
    sba_sbi_tgt_i  : in  sbi_tgt_t(rdata(7 downto 0)) := (ready => '0', rdata => (others => '0'), info => (name => (others => ' ')))
  );
end component WardRV_dbg_dm;

component WardRV_debug is
  generic (
    IDCODE_VALUE        : std_logic_vector(31 downto 0) := x"10000001";
    TRST_ENABLE         : boolean                       := true;
    DTM_IDLE_CYCLES     : natural                       := 5;
    PROGBUF_SIZE        : natural                       := 2;
    DATA_COUNT          : natural                       := 2;
    IMPEBREAK           : boolean                       := true;
    SBA_ENABLE          : boolean                       := false;
    SBA_ADDR_WIDTH      : positive                      := 8;
    RESETHALTREQ_ENABLE : boolean                       := false;
    HARTID              : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    jtag_ini_i     : in  jtag_ini_t;
    jtag_tgt_o     : out jtag_tgt_t;
    clk_i          : in  std_logic;
    arst_b_i       : in  std_logic;
    hart_rst_b_i   : in  std_logic;
    dbg_hart_req_o : out dbg_hart_req_t;
    dbg_sbi_ini_i  : in  dbg_sbi_ini_t;
    dbg_sbi_tgt_o  : out dbg_sbi_tgt_t;
    ndmreset_o     : out std_logic;
    sba_sbi_ini_o  : out sbi_ini_t(addr (SBA_ADDR_WIDTH-1 downto 0),
                                   wdata(7 downto 0));
    sba_sbi_tgt_i  : in  sbi_tgt_t(rdata(7 downto 0)) := (ready => '0', rdata => (others => '0'), info => (name => (others => ' ')))
  );
end component WardRV_debug;

-- [COMPONENT_INSERT][END]

end package WardRV_debug_pkg;

package body WardRV_debug_pkg is

  function dbg_imm(offset : natural) return integer is
  begin
    return offset - 2048;
  end function;

  function imm12(imm : integer) return std_logic_vector is
  begin
    return std_logic_vector(to_signed(imm, 12));
  end function;

  function rv_lw (rd : std_logic_vector(4 downto 0); rs1 : std_logic_vector(4 downto 0); imm : integer) return std_logic_vector is
  begin
    return imm12(imm) & rs1 & "010" & rd & "0000011";
  end function;

  function rv_sw (rs2 : std_logic_vector(4 downto 0); rs1 : std_logic_vector(4 downto 0); imm : integer) return std_logic_vector is
    variable v_imm : std_logic_vector(11 downto 0);
  begin
    v_imm := imm12(imm);
    return v_imm(11 downto 5) & rs2 & rs1 & "010" & v_imm(4 downto 0) & "0100011";
  end function;

  function rv_csrrw (rd : std_logic_vector(4 downto 0); csr : std_logic_vector(11 downto 0); rs1 : std_logic_vector(4 downto 0)) return std_logic_vector is
  begin
    return csr & rs1 & "001" & rd & "1110011";
  end function;

  function rv_csrrs (rd : std_logic_vector(4 downto 0); csr : std_logic_vector(11 downto 0); rs1 : std_logic_vector(4 downto 0)) return std_logic_vector is
  begin
    return csr & rs1 & "010" & rd & "1110011";
  end function;

  function rv_jal (rd : std_logic_vector(4 downto 0); imm : integer) return std_logic_vector is
    variable v_imm : std_logic_vector(20 downto 0);
  begin
    v_imm := std_logic_vector(to_signed(imm, 21));
    return v_imm(20) & v_imm(10 downto 1) & v_imm(11) & v_imm(19 downto 12) & rd & "1101111";
  end function;

end package body WardRV_debug_pkg;
