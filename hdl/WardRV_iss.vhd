library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.RV_pkg.all;

entity WardRV_iss is
  generic (
    RESET_ADDR : std_logic_vector(31 downto 0) := (others => '0')
  );
  port (
    clk_i      : in  std_logic;
    arst_b_i   : in  std_logic;

    -- Instruction Interface
    inst_ini_o : out inst_ini_t;
    inst_tgt_i : in  inst_tgt_t;

    -- Data Interface
    sbi_ini_o  : out sbi_ini_t;
    sbi_tgt_i  : in  sbi_tgt_t
  );
end entity WardRV_iss;

architecture behavioural of WardRV_iss is

  -- State Machine
  type state_t is (S_FETCH_REQ, S_FETCH_WAIT, S_DECODE, S_MEM_REQ, S_MEM_WAIT, S_WRITEBACK);
  signal state : state_t;

  -- CPU State
  signal pc      : std_logic_vector(31 downto 0);
  signal next_pc : std_logic_vector(31 downto 0);
  
  -- Register File (x0 is hardwired to 0 in logic)
  type regfile_t is array (0 to 31) of std_logic_vector(31 downto 0);
  signal regs : regfile_t;

  -- Current Instruction
  signal inst : std_logic_vector(31 downto 0);

  -- Decoded Fields
  alias opcode : std_logic_vector(6 downto 0) is inst(6 downto 0);
  alias rd     : std_logic_vector(4 downto 0) is inst(11 downto 7);
  alias funct3 : std_logic_vector(2 downto 0) is inst(14 downto 12);
  alias rs1    : std_logic_vector(4 downto 0) is inst(19 downto 15);
  alias rs2    : std_logic_vector(4 downto 0) is inst(24 downto 20);
  alias funct7 : std_logic_vector(6 downto 0) is inst(31 downto 25);

  -- Internal
  signal alu_res  : std_logic_vector(31 downto 0);
  signal mem_addr : std_logic_vector(31 downto 0);
  signal mem_wdata: std_logic_vector(31 downto 0);
  signal mem_we   : std_logic;
  signal mem_be   : std_logic_vector(3 downto 0);

begin

  process(clk_i, arst_b_i)
    variable v_imm_i : std_logic_vector(31 downto 0);
    variable v_imm_s : std_logic_vector(31 downto 0);
    variable v_imm_b : std_logic_vector(31 downto 0);
    variable v_imm_u : std_logic_vector(31 downto 0);
    variable v_imm_j : std_logic_vector(31 downto 0);
    variable v_op1   : signed(31 downto 0);
    variable v_op2   : signed(31 downto 0);
    variable v_res   : std_logic_vector(31 downto 0);
    variable v_npc   : std_logic_vector(31 downto 0);
  begin
    if arst_b_i = '0' then
      state <= S_FETCH_REQ;
      pc    <= RESET_ADDR;
      regs  <= (others => (others => '0'));
      inst_ini_o.valid <= '0';
      sbi_ini_o.valid  <= '0';
      inst <= (others => '0');
    elsif rising_edge(clk_i) then
      
      -- Default Bus Outputs
      inst_ini_o.valid <= '0';
      sbi_ini_o.valid  <= '0';
      sbi_ini_o.we     <= '0';
      sbi_ini_o.be     <= "0000";

      case state is
        -- 1. Fetch Request
        when S_FETCH_REQ =>
          inst_ini_o.valid <= '1';
          inst_ini_o.addr  <= pc;
          state <= S_FETCH_WAIT;

        -- 2. Fetch Wait
        when S_FETCH_WAIT =>
          inst_ini_o.valid <= '1';
          inst_ini_o.addr  <= pc;
          if inst_tgt_i.ready = '1' then
            inst <= inst_tgt_i.inst;
            state <= S_DECODE;
            inst_ini_o.valid <= '0';
          end if;

        -- 3. Decode & Execute (Behavioral)
        when S_DECODE =>
          -- Immediates
          v_imm_i := std_logic_vector(resize(signed(inst(31 downto 20)), 32));
          v_imm_s := std_logic_vector(resize(signed(std_logic_vector'(inst(31 downto 25) & inst(11 downto 7))), 32)); -- Store
          v_imm_b := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8) & '0')), 32));
          v_imm_u := inst(31 downto 12) & x"000";
          v_imm_j := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21) & '0')), 32));

          -- Operands
          v_op1 := signed(regs(to_integer(unsigned(rs1))));
          v_op2 := signed(regs(to_integer(unsigned(rs2))));
          
          -- Defaults
          v_npc := std_logic_vector(unsigned(pc) + 4);
          v_res := (others => '0');
          mem_we <= '0';

          case opcode is
            when OPC_LUI => -- LUI
              v_res := v_imm_u;
              state <= S_WRITEBACK;

            when OPC_AUIPC => -- AUIPC
              v_res := std_logic_vector(unsigned(pc) + unsigned(v_imm_u));
              state <= S_WRITEBACK;

            when OPC_JAL => -- JAL
              v_res := std_logic_vector(unsigned(pc) + 4);
              v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_j));
              state <= S_WRITEBACK;

            when OPC_JALR => -- JALR
              v_res := std_logic_vector(unsigned(pc) + 4);
              v_npc := std_logic_vector(unsigned(unsigned(v_op1) + unsigned(v_imm_i)) and x"FFFFFFFE");
              state <= S_WRITEBACK;

            when OPC_BRANCH => -- BRANCH
              state <= S_FETCH_REQ; -- No WB
              case funct3 is
                when F3_BEQ  => if v_op1 = v_op2 then v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_b)); end if;
                when F3_BNE  => if v_op1 /= v_op2 then v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_b)); end if;
                when F3_BLT  => if v_op1 < v_op2 then v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_b)); end if;
                when F3_BGE  => if v_op1 >= v_op2 then v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_b)); end if;
                when F3_BLTU => if unsigned(v_op1) < unsigned(v_op2) then v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_b)); end if;
                when F3_BGEU => if unsigned(v_op1) >= unsigned(v_op2) then v_npc := std_logic_vector(unsigned(pc) + unsigned(v_imm_b)); end if;
                when others => null;
              end case;

            when OPC_LOAD => -- LOAD
              mem_addr <= std_logic_vector(unsigned(v_op1) + unsigned(v_imm_i));
              state <= S_MEM_REQ;

            when OPC_STORE => -- STORE
              mem_addr <= std_logic_vector(unsigned(v_op1) + unsigned(v_imm_s));
              mem_wdata <= std_logic_vector(v_op2);
              mem_we <= '1';
              case funct3 is
                when F3_SB  => mem_be <= "0001"; -- Byte
                when F3_SH  => mem_be <= "0011"; -- Half
                when F3_SW  => mem_be <= "1111"; -- Word
                when others => mem_be <= "0000";
              end case;
              state <= S_MEM_REQ;

            when OPC_OP_IMM => -- OP-IMM
              case funct3 is
                when F3_ADD  => v_res := std_logic_vector(v_op1 + signed(v_imm_i)); -- ADDI
                when F3_SLT  => if v_op1 < signed(v_imm_i) then v_res := x"00000001"; else v_res := (others => '0'); end if; -- SLTI
                when F3_SLTU => if unsigned(v_op1) < unsigned(v_imm_i) then v_res := x"00000001"; else v_res := (others => '0'); end if; -- SLTIU
                when F3_XOR  => v_res := std_logic_vector(v_op1) xor v_imm_i; -- XORI
                when F3_OR   => v_res := std_logic_vector(v_op1) or v_imm_i;  -- ORI
                when F3_AND  => v_res := std_logic_vector(v_op1) and v_imm_i; -- ANDI
                when F3_SLL  => v_res := std_logic_vector(shift_left(unsigned(v_op1), to_integer(unsigned(v_imm_i(4 downto 0))))); -- SLLI
                when F3_SRL  => -- SRLI / SRAI
                  if v_imm_i(30) = '1' then -- SRAI
                    v_res := std_logic_vector(shift_right(v_op1, to_integer(unsigned(v_imm_i(4 downto 0)))));
                  else -- SRLI
                    v_res := std_logic_vector(shift_right(unsigned(v_op1), to_integer(unsigned(v_imm_i(4 downto 0)))));
                  end if;
                when others => null;
              end case;
              state <= S_WRITEBACK;

            when OPC_OP => -- OP
              case funct3 is
                when F3_ADD  => -- ADD / SUB
                  if funct7(5) = '1' then v_res := std_logic_vector(v_op1 - v_op2);
                  else                    v_res := std_logic_vector(v_op1 + v_op2);
                  end if;
                when F3_SLL  => v_res := std_logic_vector(shift_left(unsigned(v_op1), to_integer(unsigned(v_op2(4 downto 0)))));
                when F3_SLT  => if v_op1 < v_op2 then v_res := x"00000001"; else v_res := (others => '0'); end if;
                when F3_SLTU => if unsigned(v_op1) < unsigned(v_op2) then v_res := x"00000001"; else v_res := (others => '0'); end if;
                when F3_XOR  => v_res := std_logic_vector(v_op1 xor v_op2);
                when F3_SRL  => -- SRL / SRA
                  if funct7(5) = '1' then v_res := std_logic_vector(shift_right(v_op1, to_integer(unsigned(v_op2(4 downto 0)))));
                  else                    v_res := std_logic_vector(shift_right(unsigned(v_op1), to_integer(unsigned(v_op2(4 downto 0)))));
                  end if;
                when F3_OR   => v_res := std_logic_vector(v_op1 or v_op2);
                when F3_AND  => v_res := std_logic_vector(v_op1 and v_op2);
                when others  => null;
              end case;
              state <= S_WRITEBACK;

            when OPC_MISC_MEM => -- FENCE
              state <= S_FETCH_REQ;

            when OPC_SYSTEM => -- SYSTEM
              case funct3 is
                when F3_PRIV => -- ECALL / EBREAK
                  state <= S_FETCH_REQ;
                when others => -- CSR Instructions (CSRRW, CSRRS, etc.)
                  -- Simplified: Read 0, Write Ignored
                  v_res := (others => '0');
                  state <= S_WRITEBACK;
              end case;

            when others => -- NOP
              state <= S_FETCH_REQ;
          end case;

          alu_res <= v_res;
          next_pc <= v_npc;

        -- 4. Memory Access
        when S_MEM_REQ =>
          sbi_ini_o.valid <= '1';
          sbi_ini_o.addr  <= mem_addr;
          sbi_ini_o.wdata <= mem_wdata;
          sbi_ini_o.we    <= mem_we;
          sbi_ini_o.be    <= mem_be;
          if sbi_tgt_i.ready = '1' then
             if mem_we = '0' then
               case funct3 is
                 when F3_LB  => alu_res <= std_logic_vector(resize(signed(sbi_tgt_i.rdata(7 downto 0)), 32));
                 when F3_LH  => alu_res <= std_logic_vector(resize(signed(sbi_tgt_i.rdata(15 downto 0)), 32));
                 when F3_LW  => alu_res <= sbi_tgt_i.rdata;
                 when F3_LBU => alu_res <= std_logic_vector(resize(unsigned(sbi_tgt_i.rdata(7 downto 0)), 32));
                 when F3_LHU => alu_res <= std_logic_vector(resize(unsigned(sbi_tgt_i.rdata(15 downto 0)), 32));
                 when others => alu_res <= sbi_tgt_i.rdata;
               end case;
             end if;
             if mem_we = '1' then state <= S_FETCH_REQ; pc <= next_pc; else state <= S_WRITEBACK; end if;
          else
             state <= S_MEM_WAIT;
          end if;

        when S_MEM_WAIT =>
          sbi_ini_o.valid <= '1';
          sbi_ini_o.addr  <= mem_addr;
          sbi_ini_o.wdata <= mem_wdata;
          sbi_ini_o.we    <= mem_we;
          sbi_ini_o.be    <= mem_be;
          if sbi_tgt_i.ready = '1' then
             if mem_we = '0' then
               case funct3 is
                 when F3_LB  => alu_res <= std_logic_vector(resize(signed(sbi_tgt_i.rdata(7 downto 0)), 32));
                 when F3_LH  => alu_res <= std_logic_vector(resize(signed(sbi_tgt_i.rdata(15 downto 0)), 32));
                 when F3_LW  => alu_res <= sbi_tgt_i.rdata;
                 when F3_LBU => alu_res <= std_logic_vector(resize(unsigned(sbi_tgt_i.rdata(7 downto 0)), 32));
                 when F3_LHU => alu_res <= std_logic_vector(resize(unsigned(sbi_tgt_i.rdata(15 downto 0)), 32));
                 when others => alu_res <= sbi_tgt_i.rdata;
               end case;
             end if;
             if mem_we = '1' then state <= S_FETCH_REQ; pc <= next_pc; else state <= S_WRITEBACK; end if;
          end if;

        -- 5. Writeback
        when S_WRITEBACK =>
          if unsigned(rd) /= 0 then regs(to_integer(unsigned(rd))) <= alu_res; end if;
          pc <= next_pc;
          state <= S_FETCH_REQ;

      end case;
    end if;
  end process;

end architecture behavioural;