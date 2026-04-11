library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.RV_pkg.all;
use     asylum.WardRV_stats_pkg.all;
use     asylum.WardRV_decode_pkg.all;
use     asylum.WardRV_fsm_alu_pkg.all;

entity WardRV_fsm_decode is
  port (
    inst_i              : in  std_logic_vector(31 downto 0);
    -- Immediates  
    imm_i_o             : out std_logic_vector(31 downto 0);
    imm_s_o             : out std_logic_vector(31 downto 0);
    imm_b_o             : out std_logic_vector(31 downto 0);
    imm_u_o             : out std_logic_vector(31 downto 0);
    imm_j_o             : out std_logic_vector(31 downto 0);
    -- Register File Control
    rd_addr_o           : out std_logic_vector(4 downto 0);
    rs1_addr_o          : out std_logic_vector(4 downto 0);
    rs2_addr_o          : out std_logic_vector(4 downto 0);
    rd_we_o             : out std_logic;
    rs1_re_o            : out std_logic;
    rs2_re_o            : out std_logic;
    -- ALU Control  
    alu_op_o            : out alu_op_t;
    alu_src_a_sel_o     : out std_logic; -- 0: RS1, 1: PC
    alu_src_b_sel_o     : out std_logic_vector(2 downto 0); -- 0:RS2, 1:ImmI, 2:ImmS, 3:ImmU, 4:ImmJ
    -- Memory Control  
    mem_req_o           : out std_logic;
    mem_we_o            : out std_logic;
    mem_be_o            : out std_logic_vector(3 downto 0);
    mem_data_unsigned_o : out std_logic; -- 0 for signed (LB, LH), 1 for unsigned (LBU, LHU)
    -- Control Flow
    is_branch_o              : out std_logic;
    branch_use_flag_zero_o   : out std_logic;
    branch_use_flag_carry_o  : out std_logic;
    branch_use_flag_sign_o   : out std_logic;
    branch_flag_is_set_o     : out std_logic;
    pc_sel_o                 : out std_logic_vector(1 downto 0);
    -- Instruction Metadata (for logging/FSM)
    funct3_o                 : out bit_vector(2 downto 0);
    inst_type_o              : out inst_type_t
  );
end entity WardRV_fsm_decode;

architecture behavioural of WardRV_fsm_decode is
  signal inst_bv : bit_vector(31 downto 0);
  alias opcode   : bit_vector(6 downto 0) is inst_bv(6 downto 0);
  alias funct3   : bit_vector(2 downto 0) is inst_bv(14 downto 12);
  alias funct7   : bit_vector(6 downto 0) is inst_bv(31 downto 25);
begin
  inst_bv <= to_bitvector(inst_i);
  
  -- Immediate Decoding
  imm_i_o <= std_logic_vector(resize(signed(inst_i(31 downto 20)), 32));
  imm_s_o <= std_logic_vector(resize(signed(std_logic_vector'(inst_i(31 downto 25) & inst_i(11 downto 7))), 32));
  imm_b_o <= std_logic_vector(resize(signed(std_logic_vector'(inst_i(31) & inst_i(7) & inst_i(30 downto 25) & inst_i(11 downto 8) & '0')), 32));
  imm_u_o <= inst_i(31 downto 12) & x"000";
  imm_j_o <= std_logic_vector(resize(signed(std_logic_vector'(inst_i(31) & inst_i(19 downto 12) & inst_i(20) & inst_i(30 downto 21) & '0')), 32));

  -- Register Addresses
  rd_addr_o  <= inst_i(11 downto 7);
  rs1_addr_o <= inst_i(19 downto 15);
  rs2_addr_o <= inst_i(24 downto 20);
  funct3_o   <= funct3;

  process(all)
  begin
    -- Default assignments
    rd_we_o             <= '0';
    rs1_re_o            <= '0';
    rs2_re_o            <= '0';
    alu_op_o            <= ALU_ADD;
    alu_src_a_sel_o     <= ALU_SRC_A_RS1; 
    alu_src_b_sel_o     <= ALU_SRC_B_RS2;
    mem_req_o           <= '0'; 
    mem_we_o            <= '0';
    mem_be_o            <= (others => '0');
    mem_data_unsigned_o <= '0';
    is_branch_o         <= '0';
    branch_use_flag_zero_o   <= '0';
    branch_use_flag_carry_o  <= '0';
    branch_use_flag_sign_o   <= '0';
    branch_flag_is_set_o     <= '0';
    pc_sel_o            <= PC_SEL_NEXT;
    inst_type_o         <= I_UNKNOWN;

    case opcode is
      when OPC_LUI =>
        rd_we_o <= '1'; alu_op_o <= ALU_OR; alu_src_b_sel_o <= ALU_SRC_B_IMM_U; inst_type_o <= I_LUI;
      when OPC_AUIPC =>
        rd_we_o <= '1'; alu_src_a_sel_o <= ALU_SRC_A_PC; alu_src_b_sel_o <= ALU_SRC_B_IMM_U; inst_type_o <= I_AUIPC;
      when OPC_JAL =>
        rd_we_o <= '1'; alu_src_a_sel_o <= ALU_SRC_A_PC; alu_src_b_sel_o <= ALU_SRC_B_IMM_J; is_branch_o <= '1'; pc_sel_o <= PC_SEL_JUMP; inst_type_o <= I_JAL;
      when OPC_JALR =>
        rd_we_o <= '1'; rs1_re_o <= '1'; alu_src_b_sel_o <= ALU_SRC_B_IMM_I; is_branch_o <= '1'; pc_sel_o <= PC_SEL_JUMP; inst_type_o <= I_JALR;
      when OPC_BRANCH =>
        rs1_re_o <= '1'; rs2_re_o <= '1'; alu_op_o <= ALU_SUB; is_branch_o <= '1'; pc_sel_o <= PC_SEL_BRANCH;
        case funct3 is
          when F3_BEQ => inst_type_o <= I_BEQ;  branch_use_flag_zero_o  <= '1'; branch_flag_is_set_o <= '1';
          when F3_BNE => inst_type_o <= I_BNE;  branch_use_flag_zero_o  <= '1';
          when F3_BLT => inst_type_o <= I_BLT;  branch_use_flag_sign_o  <= '1'; branch_flag_is_set_o <= '1';
          when F3_BGE => inst_type_o <= I_BGE;  branch_use_flag_sign_o  <= '1';
          when F3_BLTU=> inst_type_o <= I_BLTU; branch_use_flag_carry_o <= '1'; branch_flag_is_set_o <= '1';
          when F3_BGEU=> inst_type_o <= I_BGEU; branch_use_flag_carry_o <= '1';
          when others => null;
        end case;
      when OPC_LOAD =>
        rd_we_o         <= '1'; 
        rs1_re_o        <= '1'; 
        alu_src_b_sel_o <= ALU_SRC_B_IMM_I; 
        mem_req_o       <= '1';

        case funct3 is
          when F3_LB => inst_type_o <= I_LB;  mem_be_o <= "0001";
          when F3_LH => inst_type_o <= I_LH;  mem_be_o <= "0011";
          when F3_LW => inst_type_o <= I_LW;  mem_be_o <= "1111";
          when F3_LBU=> inst_type_o <= I_LBU; mem_be_o <= "0001"; mem_data_unsigned_o <= '1';
          when F3_LHU=> inst_type_o <= I_LHU; mem_be_o <= "0011"; mem_data_unsigned_o <= '1';
          when others => null;
        end case;
      when OPC_STORE =>
        rs1_re_o        <= '1'; 
        rs2_re_o        <= '1'; 
        alu_src_b_sel_o <= ALU_SRC_B_IMM_S; 
        mem_req_o       <= '1'; 
        mem_we_o        <= '1';
        case funct3 is
          when F3_SB => inst_type_o <= I_SB; mem_be_o <= "0001"; 
          when F3_SH => inst_type_o <= I_SH; mem_be_o <= "0011";
          when F3_SW => inst_type_o <= I_SW; mem_be_o <= "1111";
          when others => null;
        end case;
      when OPC_OP_IMM =>
        rd_we_o         <= '1'; 
        rs1_re_o        <= '1'; 
        alu_src_b_sel_o <= ALU_SRC_B_IMM_I;
        case funct3 is
          when F3_ADD  => alu_op_o <= ALU_ADD;  inst_type_o <= I_ADDI;
          when F3_SLT  => alu_op_o <= ALU_SLT;  inst_type_o <= I_SLTI;
          when F3_SLTU => alu_op_o <= ALU_SLTU; inst_type_o <= I_SLTIU;
          when F3_XOR  => alu_op_o <= ALU_XOR;  inst_type_o <= I_XORI;
          when F3_OR   => alu_op_o <= ALU_OR;   inst_type_o <= I_ORI;
          when F3_AND  => alu_op_o <= ALU_AND;  inst_type_o <= I_ANDI;
          when F3_SLL  => alu_op_o <= ALU_SLL;  inst_type_o <= I_SLLI;
          when F3_SRL_SRA => 
            if funct7(5) = '1' then alu_op_o <= ALU_SRA; inst_type_o <= I_SRAI;
            else                    alu_op_o <= ALU_SRL; inst_type_o <= I_SRLI; end if;
          when others => null;
        end case;
      when OPC_OP =>
        rd_we_o <= '1'; rs1_re_o <= '1'; rs2_re_o <= '1';
        case funct3 is
          when F3_ADD => 
            if funct7(5) = '1' then alu_op_o <= ALU_SUB; inst_type_o <= I_SUB;
            else                    alu_op_o <= ALU_ADD; inst_type_o <= I_ADD; end if;
          when F3_SLL => alu_op_o <= ALU_SLL; inst_type_o <= I_SLL;
          when F3_SLT => alu_op_o <= ALU_SLT; inst_type_o <= I_SLT;
          when F3_SLTU=> alu_op_o <= ALU_SLTU;inst_type_o <= I_SLTU;
          when F3_XOR => alu_op_o <= ALU_XOR; inst_type_o <= I_XOR;
          when F3_SRL_SRA => 
            if funct7(5) = '1' then alu_op_o <= ALU_SRA; inst_type_o <= I_SRA;
            else                    alu_op_o <= ALU_SRL; inst_type_o <= I_SRL; end if;
          when F3_OR  => alu_op_o <= ALU_OR;  inst_type_o <= I_OR;
          when F3_AND => alu_op_o <= ALU_AND; inst_type_o <= I_AND;
          when others => null;
        end case;
      when OPC_SYSTEM =>
        rd_we_o <= '1'; alu_op_o <= ALU_OR; inst_type_o <= I_UNKNOWN;
      when others =>
        inst_type_o <= I_UNKNOWN;
    end case;

    -- rd_we must be 0 if rd_addr is x0
    if unsigned(inst_i(11 downto 7)) = 0 then
      rd_we_o <= '0';
    end if;
  end process;
end architecture behavioural;