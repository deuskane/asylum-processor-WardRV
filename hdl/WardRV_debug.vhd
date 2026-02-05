library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library asylum;
use asylum.WardRV_pkg.all;
use work.WardRV_debug_pkg.all;

entity WardRV_debug is
  generic (
    IDCODE_VALUE : std_logic_vector(31 downto 0) := x"10000001"
  );
  port (
    -- JTAG (TCK Domain)
    jtag_ini_i : in  jtag_ini_t;
    jtag_tgt_o : out jtag_tgt_t;

    -- Core (CLK Domain)
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;
    dbg_req_o  : out dbg_req_t;
    dbg_rsp_i  : in  dbg_rsp_t
  );
end WardRV_debug;

architecture rtl of WardRV_debug is

  -- ===========================================================================
  -- JTAG TAP Controller
  -- ===========================================================================
  type tap_state_t is (
    TEST_LOGIC_RESET, RUN_TEST_IDLE,
    SELECT_DR_SCAN, CAPTURE_DR, SHIFT_DR, EXIT1_DR, PAUSE_DR, EXIT2_DR, UPDATE_DR,
    SELECT_IR_SCAN, CAPTURE_IR, SHIFT_IR, EXIT1_IR, PAUSE_IR, EXIT2_IR, UPDATE_IR
  );
  signal tap_state : tap_state_t;

  -- JTAG Registers
  signal ir_reg    : std_logic_vector(4 downto 0); -- Instruction Register
  signal idcode_dr : std_logic_vector(31 downto 0);
  signal dtmcs_dr  : std_logic_vector(31 downto 0);
  signal dmi_dr    : std_logic_vector(40 downto 0) := (others => '0'); -- Address(7) + Data(32) + Op(2)
  signal bypass_dr : std_logic;

  -- JTAG Instructions (RISC-V Standard)
  constant IR_IDCODE : std_logic_vector(4 downto 0) := "00001";
  constant IR_DTMCS  : std_logic_vector(4 downto 0) := "10000";
  constant IR_DMI    : std_logic_vector(4 downto 0) := "10001";
  constant IR_BYPASS : std_logic_vector(4 downto 0) := "11111";

  -- Internal Signals
  signal tdo_mux     : std_logic;
  signal dmi_req     : std_logic;
  signal dmi_wr      : std_logic;
  signal dmi_addr    : std_logic_vector(6 downto 0);
  signal dmi_wdata   : std_logic_vector(31 downto 0);
  signal dmi_rdata   : std_logic_vector(31 downto 0);

  -- CDC Signals (Synchronization)
  signal dmi_req_sync : std_logic_vector(2 downto 0);
  signal dmi_ack_sync : std_logic_vector(2 downto 0);
  signal dmi_ack      : std_logic;

  -- DM Registers (CLK Domain)
  signal dmcontrol : std_logic_vector(31 downto 0);
  
begin

  -- ===========================================================================
  -- 1. JTAG TAP FSM (TCK Domain)
  -- ===========================================================================
  process(jtag_ini_i.tck, jtag_ini_i.trst_n)
  begin
    if jtag_ini_i.trst_n = '0' then
      tap_state <= TEST_LOGIC_RESET;
      ir_reg    <= IR_IDCODE; -- Default to IDCODE
    elsif rising_edge(jtag_ini_i.tck) then
      -- FSM Transitions
      case tap_state is
        when TEST_LOGIC_RESET => if jtag_ini_i.tms = '0' then tap_state <= RUN_TEST_IDLE; end if;
        when RUN_TEST_IDLE    => if jtag_ini_i.tms = '1' then tap_state <= SELECT_DR_SCAN; end if;
        
        when SELECT_DR_SCAN   => if jtag_ini_i.tms = '1' then tap_state <= SELECT_IR_SCAN; else tap_state <= CAPTURE_DR; end if;
        when CAPTURE_DR       => if jtag_ini_i.tms = '1' then tap_state <= EXIT1_DR;       else tap_state <= SHIFT_DR;   end if;
        when SHIFT_DR         => if jtag_ini_i.tms = '1' then tap_state <= EXIT1_DR;       end if;
        when EXIT1_DR         => if jtag_ini_i.tms = '1' then tap_state <= UPDATE_DR;      else tap_state <= PAUSE_DR;   end if;
        when PAUSE_DR         => if jtag_ini_i.tms = '1' then tap_state <= EXIT2_DR;       end if;
        when EXIT2_DR         => if jtag_ini_i.tms = '1' then tap_state <= UPDATE_DR;      else tap_state <= SHIFT_DR;   end if;
        when UPDATE_DR        => if jtag_ini_i.tms = '1' then tap_state <= SELECT_DR_SCAN; else tap_state <= RUN_TEST_IDLE; end if;

        when SELECT_IR_SCAN   => if jtag_ini_i.tms = '1' then tap_state <= TEST_LOGIC_RESET; else tap_state <= CAPTURE_IR; end if;
        when CAPTURE_IR       => if jtag_ini_i.tms = '1' then tap_state <= EXIT1_IR;       else tap_state <= SHIFT_IR;   end if;
        when SHIFT_IR         => if jtag_ini_i.tms = '1' then tap_state <= EXIT1_IR;       end if;
        when EXIT1_IR         => if jtag_ini_i.tms = '1' then tap_state <= UPDATE_IR;      else tap_state <= PAUSE_IR;   end if;
        when PAUSE_IR         => if jtag_ini_i.tms = '1' then tap_state <= EXIT2_IR;       end if;
        when EXIT2_IR         => if jtag_ini_i.tms = '1' then tap_state <= UPDATE_IR;      else tap_state <= SHIFT_IR;   end if;
        when UPDATE_IR        => if jtag_ini_i.tms = '1' then tap_state <= SELECT_DR_SCAN; else tap_state <= RUN_TEST_IDLE; end if;
      end case;

      -- IR Update
      if tap_state = TEST_LOGIC_RESET then
        ir_reg <= IR_IDCODE;
      elsif tap_state = UPDATE_IR then
        ir_reg <= dmi_dr(4 downto 0); -- Reuse DMI shift register for IR shift temporarily or use dedicated
      end if;
    end if;
  end process;

  -- ===========================================================================
  -- 2. Data Registers Shift Logic (TCK Domain)
  -- ===========================================================================
  -- Note: Simplified shifting logic. In a real implementation, we would have
  -- a dedicated shift register for IR and DR. Here we use dmi_dr as the main shifter.
  
  process(jtag_ini_i.tck)
  begin
    if rising_edge(jtag_ini_i.tck) then
      -- Capture
      if tap_state = CAPTURE_IR then
        dmi_dr(4 downto 0) <= "00001"; -- Capture pattern (LSB must be 1)
      elsif tap_state = CAPTURE_DR then
        case ir_reg is
          when IR_IDCODE => dmi_dr(31 downto 0) <= IDCODE_VALUE;
          when IR_DTMCS  => dmi_dr(31 downto 0) <= x"00000071"; -- Version 1, 7 bits address
          when IR_DMI    => 
            -- DMI Read Data Capture
            dmi_dr <= dmi_addr & dmi_rdata & "00"; -- Status 0 (Success)
          when others    => bypass_dr <= '0';
        end case;
      
      -- Shift
      elsif tap_state = SHIFT_IR then
        dmi_dr(4 downto 0) <= jtag_ini_i.tdi & dmi_dr(4 downto 1);
      elsif tap_state = SHIFT_DR then
        case ir_reg is
          when IR_IDCODE | IR_DTMCS =>
            dmi_dr(31 downto 0) <= jtag_ini_i.tdi & dmi_dr(31 downto 1);
          when IR_DMI =>
            dmi_dr <= jtag_ini_i.tdi & dmi_dr(40 downto 1);
          when others =>
            bypass_dr <= jtag_ini_i.tdi;
        end case;
      end if;

      -- DMI Request Generation (Update DR)
      dmi_req <= '0';
      if tap_state = UPDATE_DR and ir_reg = IR_DMI then
        dmi_req   <= '1';
        dmi_addr  <= dmi_dr(40 downto 34);
        dmi_wdata <= dmi_dr(33 downto 2);
        dmi_wr    <= dmi_dr(1); -- Op 10=Write, 01=Read (Simplified)
        -- Note: Standard says Op=1 is Read, Op=2 is Write. 
        if dmi_dr(1 downto 0) = "10" then dmi_wr <= '1'; else dmi_wr <= '0'; end if;
      end if;
    end if;
  end process;

  -- TDO Mux
  process(all)
  begin
    if tap_state = SHIFT_IR then
      tdo_mux <= dmi_dr(0);
    else
      case ir_reg is
        when IR_IDCODE | IR_DTMCS => tdo_mux <= dmi_dr(0);
        when IR_DMI               => tdo_mux <= dmi_dr(0);
        when others               => tdo_mux <= bypass_dr;
      end case;
    end if;
  end process;

  -- TDO Output (Change on falling edge of TCK per standard)
  process(jtag_ini_i.tck)
  begin
    if falling_edge(jtag_ini_i.tck) then
      jtag_tgt_o.tdo <= tdo_mux;
    end if;
  end process;

  -- TDO Enable (Active during SHIFT-DR and SHIFT-IR)
  jtag_tgt_o.tdo_en <= '1' when (tap_state = SHIFT_DR or tap_state = SHIFT_IR) else '0';

  -- ===========================================================================
  -- 3. CDC (Clock Domain Crossing) TCK <-> CLK
  -- ===========================================================================
  -- Simple handshake for DMI Request
  
  -- TCK to CLK
  process(clk_i, arst_b_i)
  begin
    if arst_b_i = '0' then
      dmi_req_sync <= (others => '0');
    elsif rising_edge(clk_i) then
      dmi_req_sync <= dmi_req_sync(1 downto 0) & dmi_req;
    end if;
  end process;

  -- CLK to TCK
  process(jtag_ini_i.tck, jtag_ini_i.trst_n)
  begin
    if jtag_ini_i.trst_n = '0' then
      dmi_ack_sync <= (others => '0');
    elsif rising_edge(jtag_ini_i.tck) then
      dmi_ack_sync <= dmi_ack_sync(1 downto 0) & dmi_ack;
    end if;
  end process;

  -- ===========================================================================
  -- 4. Debug Module Logic (CLK Domain)
  -- ===========================================================================
  process(clk_i, arst_b_i)
    variable v_req_pulse : boolean;
  begin
    if arst_b_i = '0' then
      dmcontrol <= (others => '0');
      dmi_rdata <= (others => '0');
      dmi_ack   <= '0';
    elsif rising_edge(clk_i) then
      v_req_pulse := (dmi_req_sync(2) = '1' and dmi_ack = '0');
      
      if v_req_pulse then
        dmi_ack <= '1';
        
        -- DMI Register Map
        case dmi_addr is
          -- 0x10: DMCONTROL
          when "0010000" => 
            if dmi_wr = '1' then
              dmcontrol <= dmi_wdata;
            else
              dmi_rdata <= dmcontrol;
            end if;

          -- 0x11: DMSTATUS (Read Only)
          when "0010001" =>
            dmi_rdata <= (others => '0');
            dmi_rdata(17 downto 16) <= "11"; -- All Halted/Running supported
            dmi_rdata(9)            <= dbg_rsp_i.halted;
            dmi_rdata(8)            <= dbg_rsp_i.running;
            dmi_rdata(3 downto 0)   <= "0010"; -- Version 0.13

          -- 0x40: HALTSUM0 (Optional)
          when "1000000" =>
            dmi_rdata <= (others => '0');
            dmi_rdata(0) <= dbg_rsp_i.halted;

          when others =>
            dmi_rdata <= (others => '0');
        end case;

      elsif dmi_req_sync(2) = '0' then
        dmi_ack <= '0';
      end if;
    end if;
  end process;

  -- Output Mapping
  dbg_req_o.halt     <= dmcontrol(31); -- haltreq
  dbg_req_o.resume   <= dmcontrol(30); -- resumereq
  dbg_req_o.ndmreset <= dmcontrol(1);  -- ndmreset

end rtl;
