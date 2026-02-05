library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;

-- =============================================================================
-- 3. Execute (ALU & Branch)
-- =============================================================================
entity WardRV_tiny_execute is
  port (
    -- Inputs
    pc_i         : in  std_logic_vector(31 downto 0);
    rs1_data_i   : in  std_logic_vector(31 downto 0);
    rs2_data_i   : in  std_logic_vector(31 downto 0);
    imm_i        : in  std_logic_vector(31 downto 0);
    opcode_i     : in  std_logic_vector(6 downto 0);
    funct3_i     : in  std_logic_vector(2 downto 0);
    funct7_i     : in  std_logic_vector(6 downto 0);
    
    -- Outputs
    alu_res_o    : out std_logic_vector(31 downto 0);
    branch_req_o : out std_logic;
    branch_tgt_o : out std_logic_vector(31 downto 0)
  );
end entity;

architecture rtl of WardRV_tiny_execute is
  signal op_a, op_b : std_logic_vector(31 downto 0);
  signal alu_res    : std_logic_vector(31 downto 0);
  signal branch_taken : std_logic;
begin

  -- Operand Muxing
  process(all)
  begin
    -- Default A: RS1
    if opcode_i = OPC_AUIPC or opcode_i = OPC_JAL then
      op_a <= pc_i;
    elsif opcode_i = OPC_LUI then
      op_a <= (others => '0');
    else
      op_a <= rs1_data_i;
    end if;

    -- Default B: RS2
    if opcode_i = OPC_OP then
      op_b <= rs2_data_i;
    elsif opcode_i = OPC_BRANCH then
      op_b <= rs2_data_i;
    else
      op_b <= imm_i; -- I-Type, S-Type, U-Type, J-Type
    end if;
  end process;

  -- ALU
  process(all)
    variable v_res : unsigned(31 downto 0);
    variable v_shamt : integer;
  begin
    v_res := (others => '0');
    v_shamt := to_integer(unsigned(op_b(4 downto 0)));
    
    case opcode_i is
      when OPC_LUI | OPC_AUIPC | OPC_JAL | OPC_JALR | OPC_LOAD | OPC_STORE =>
        v_res := unsigned(op_a) + unsigned(op_b);
        
      when OPC_OP_IMM | OPC_OP =>
        case funct3_i is
          when F3_ADD_SUB =>
            if opcode_i = OPC_OP and funct7_i(5) = '1' then
              v_res := unsigned(op_a) - unsigned(op_b);
            else
              v_res := unsigned(op_a) + unsigned(op_b);
            end if;
          when F3_SLL => v_res := shift_left(unsigned(op_a), v_shamt);
          when F3_SLT => if signed(op_a) < signed(op_b) then v_res := to_unsigned(1, 32); end if;
          when F3_SLTU => if unsigned(op_a) < unsigned(op_b) then v_res := to_unsigned(1, 32); end if;
          when F3_XOR => v_res := unsigned(op_a) xor unsigned(op_b);
          when F3_SRL_SRA =>
            if funct7_i(5) = '1' then
              v_res := unsigned(std_logic_vector(shift_right(signed(op_a), v_shamt)));
            else
              v_res := shift_right(unsigned(op_a), v_shamt);
            end if;
          when F3_OR => v_res := unsigned(op_a) or unsigned(op_b);
          when F3_AND => v_res := unsigned(op_a) and unsigned(op_b);
          when others => null;
        end case;
      when others => null;
    end case;
    
    alu_res <= std_logic_vector(v_res);
  end process;
  
  alu_res_o <= alu_res;

  -- Branch Logic
  process(all)
    variable v_eq, v_lt, v_ltu : boolean;
  begin
    v_eq  := (unsigned(rs1_data_i) = unsigned(rs2_data_i));
    v_lt  := (signed(rs1_data_i) < signed(rs2_data_i));
    v_ltu := (unsigned(rs1_data_i) < unsigned(rs2_data_i));
    branch_taken <= '0';

    if opcode_i = OPC_BRANCH then
      case funct3_i is
        when F3_BEQ  => if v_eq then branch_taken <= '1'; end if;
        when "001"   => if not v_eq then branch_taken <= '1'; end if; -- BNE
        when F3_SLT  => if v_lt then branch_taken <= '1'; end if;     -- BLT
        when "101"   => if not v_lt then branch_taken <= '1'; end if; -- BGE
        when F3_SLTU => if v_ltu then branch_taken <= '1'; end if;    -- BLTU
        when "111"   => if not v_ltu then branch_taken <= '1'; end if;-- BGEU
        when others => null;
      end case;
    elsif opcode_i = OPC_JAL or opcode_i = OPC_JALR then
      branch_taken <= '1';
    end if;
  end process;

  branch_req_o <= branch_taken;
  branch_tgt_o <= alu_res; -- JAL/JALR/Branch target computed by ALU

end architecture;