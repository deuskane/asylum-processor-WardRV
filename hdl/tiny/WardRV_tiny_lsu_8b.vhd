library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;
use work.WardRV_tiny_pkg.all;

-- =============================================================================
-- 6. LSU 8-bit (Memory Interface)
-- =============================================================================
entity WardRV_tiny_lsu_8b is
  port (
    -- Core Interface
    clk_i        : in  std_logic;
    arst_b_i     : in  std_logic;
    alu_res_i    : in  std_logic_vector(31 downto 0);
    rs2_data_i   : in  std_logic_vector(31 downto 0);
    opcode_i     : in  std_logic_vector(6 downto 0);
    funct3_i     : in  std_logic_vector(2 downto 0);
    
    -- Bus Interface (8-bit)
    sbi_ini_o : out sbi_ini_t;
    sbi_tgt_i : in  sbi_tgt_t;
    
    -- Output
    mem_rdata_o  : out std_logic_vector(31 downto 0);
    busy_o       : out std_logic
  );
end entity;

architecture rtl of WardRV_tiny_lsu_8b is
  signal state_r : lsu_8b_state_t;

  -- Registered transaction info
  signal addr_reg     : std_logic_vector(31 downto 0);
  signal wdata_reg    : std_logic_vector(31 downto 0);
  signal rdata_reg    : std_logic_vector(31 downto 0);
  signal funct3_reg   : std_logic_vector(2 downto 0);
  signal opcode_reg   : std_logic_vector(6 downto 0);
  signal byte_count_r : integer range 0 to 3;
  
  signal busy_reg : std_logic;
  
  signal req_is_load  : boolean;
  signal req_is_store : boolean;
  
begin

  req_is_load  <= (opcode_i = OPC_LOAD);
  req_is_store <= (opcode_i = OPC_STORE);
  
  busy_o <= busy_reg;

  -- State machine and transaction logic
  process(clk_i, arst_b_i)
    variable access_is_done : boolean;
  begin
    if arst_b_i = '0' then
      state_r      <= IDLE;
      busy_reg     <= '0';
      byte_count_r <= 0;
      addr_reg     <= (others => '0');
      wdata_reg    <= (others => '0');
      rdata_reg    <= (others => '0');
      funct3_reg   <= (others => '0');
      opcode_reg   <= (others => '0');
    elsif rising_edge(clk_i) then
      case state_r is
        when IDLE =>
          busy_reg <= '0';
          if req_is_load or req_is_store then
            state_r      <= MEM_ACCESS;
            busy_reg     <= '1';
            byte_count_r <= 0;
            addr_reg     <= alu_res_i;
            wdata_reg    <= rs2_data_i;
            funct3_reg   <= funct3_i;
            opcode_reg   <= opcode_i;
            rdata_reg    <= (others => '0');
          end if;
          
        when MEM_ACCESS =>
          if sbi_tgt_i.ready = '1' then
            if opcode_reg = OPC_LOAD then
              case byte_count_r is
                when 0 => rdata_reg(7 downto 0)   <= sbi_tgt_i.rdata(7 downto 0);
                when 1 => rdata_reg(15 downto 8)  <= sbi_tgt_i.rdata(7 downto 0);
                when 2 => rdata_reg(23 downto 16) <= sbi_tgt_i.rdata(7 downto 0);
                when 3 => rdata_reg(31 downto 24) <= sbi_tgt_i.rdata(7 downto 0);
                when others => null;
              end case;
            end if;
            
            access_is_done := false;
            case funct3_reg is
              when F3_LB | F3_LBU => access_is_done := true;
              when F3_LH | F3_LHU => if byte_count_r = 1 then access_is_done := true; end if;
              when F3_LW          => if byte_count_r = 3 then access_is_done := true; end if;
              when others => access_is_done := true;
            end case;
            
            if access_is_done then
              state_r  <= UPDATE_PIPE;
              busy_reg <= '0';
            else
              byte_count_r <= byte_count_r + 1;
            end if;
          end if;

        when UPDATE_PIPE =>
          state_r  <= IDLE;
          busy_reg <= '0';

      end case;
    end if;
  end process;

  -- Combinatorial outputs to memory
  sbi_ini_o.valid <= '1' when state_r = MEM_ACCESS else '0';
  sbi_ini_o.addr  <= std_logic_vector(unsigned(addr_reg) + byte_count_r);
  sbi_ini_o.we    <= '1' when opcode_reg = OPC_STORE else '0';
  sbi_ini_o.be    <= "0001";
  
  sbi_ini_o.wdata <= x"000000" & wdata_reg(7 downto 0)   when byte_count_r = 0 else
                     x"000000" & wdata_reg(15 downto 8)  when byte_count_r = 1 else
                     x"000000" & wdata_reg(23 downto 16) when byte_count_r = 2 else
                     x"000000" & wdata_reg(31 downto 24);

  -- Read data alignment and sign extension
  process(state_r, busy_reg, opcode_reg, funct3_reg, rdata_reg)
    variable v_byte : std_logic_vector(7 downto 0);
    variable v_half : std_logic_vector(15 downto 0);
  begin
    v_byte := rdata_reg(7 downto 0);
    v_half := rdata_reg(15 downto 8) & rdata_reg(7 downto 0); -- Little Endian assembly
    mem_rdata_o <= (others => '0');

    if state_r = IDLE and busy_reg = '1' and opcode_reg = OPC_LOAD then
        case funct3_reg is
          when F3_LB  => mem_rdata_o <= std_logic_vector(resize(signed(v_byte), 32));
          when F3_LBU => mem_rdata_o <= std_logic_vector(resize(unsigned(v_byte), 32));
          when F3_LH  => mem_rdata_o <= std_logic_vector(resize(signed(v_half), 32));
          when F3_LHU => mem_rdata_o <= std_logic_vector(resize(unsigned(v_half), 32));
          when F3_LW  => mem_rdata_o <= rdata_reg;
          when others => null;
        end case;
    end if;
  end process;

end architecture;