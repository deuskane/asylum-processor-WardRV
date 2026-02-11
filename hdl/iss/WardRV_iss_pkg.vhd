-------------------------------------------------------------------------------
-- Title      : WardRV ISS Package
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_iss_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: Instruction Set Simulator Class
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library asylum;
use asylum.WardRV_pkg.all;
use asylum.RV_pkg.all;

package WardRV_iss_pkg is

  -- Generic parameters for memory configuration
  constant IMEM_ADDR_WIDTH : positive := 32;
  constant DMEM_ADDR_WIDTH : positive := 32;
  constant DMEM_DATA_WIDTH : positive := 32;

  type iss_t is protected
    
    -- Initialization
    procedure reset(
      start_pc : in std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0)
    );

    -- Accessors
    impure function get_pc return std_logic_vector;
    impure function get_reg(r : integer) return std_logic_vector;
    
    -- Execution Step
    -- Returns info about memory request if any
    procedure execute_instruction(
      inst          : in  std_logic_vector(31 downto 0);
      -- Memory Request Info
      mem_req       : out boolean;
      mem_we        : out std_logic;
      mem_addr      : out std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      mem_wdata     : out std_logic_vector(DMEM_DATA_WIDTH-1 downto 0);
      mem_be        : out std_logic_vector((DMEM_DATA_WIDTH/8)-1 downto 0)
    );

    -- Complete a Load operation
    procedure complete_load(
      mem_rdata     : in std_logic_vector(DMEM_DATA_WIDTH-1 downto 0)
    );

  end protected;

end package;

package body WardRV_iss_pkg is

  type regfile_t is array (0 to 31) of std_logic_vector(31 downto 0);

  type iss_t is protected body
    
    variable pc_r   : std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0);
    variable regs_r : regfile_t;
    
    -- State for pending load
    variable pending_load_rd     : std_logic_vector(4 downto 0);
    variable pending_load_funct3 : std_logic_vector(2 downto 0);
    variable pending_load_byte_off : integer;

    procedure reset(
      start_pc : in std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0)
    ) is
    begin
      pc_r   := start_pc;
      regs_r := (others => (others => '0'));
      pending_load_rd := (others => '0');
    end procedure;

    impure function get_pc return std_logic_vector is
    begin
      return pc_r;
    end function;

    impure function get_reg(r : integer) return std_logic_vector is
    begin
      if r = 0 then return x"00000000"; end if;
      return regs_r(r);
    end function;

    procedure execute_instruction(
      inst          : in  std_logic_vector(31 downto 0);
      mem_req       : out boolean;
      mem_we        : out std_logic;
      mem_addr      : out std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      mem_wdata     : out std_logic_vector(DMEM_DATA_WIDTH-1 downto 0);
      mem_be        : out std_logic_vector((DMEM_DATA_WIDTH/8)-1 downto 0)
    ) is
      alias opcode : std_logic_vector(6 downto 0) is inst(6 downto 0);
      alias rd     : std_logic_vector(4 downto 0) is inst(11 downto 7);
      alias funct3 : std_logic_vector(2 downto 0) is inst(14 downto 12);
      alias rs1    : std_logic_vector(4 downto 0) is inst(19 downto 15);
      alias rs2    : std_logic_vector(4 downto 0) is inst(24 downto 20);
      alias funct7 : std_logic_vector(6 downto 0) is inst(31 downto 25);

      variable v_imm_i : std_logic_vector(31 downto 0);
      variable v_imm_s : std_logic_vector(31 downto 0);
      variable v_imm_b : std_logic_vector(31 downto 0);
      variable v_imm_u : std_logic_vector(31 downto 0);
      variable v_imm_j : std_logic_vector(31 downto 0);
      variable v_op1   : signed(31 downto 0);
      variable v_op2   : signed(31 downto 0);
      variable v_res   : std_logic_vector(31 downto 0);
      variable v_npc   : std_logic_vector(IMEM_ADDR_WIDTH-1 downto 0);
      variable v_addr  : std_logic_vector(DMEM_ADDR_WIDTH-1 downto 0);
      variable v_shamt : integer;
    begin
      -- Initialize outputs
      mem_req   := false;
      mem_we    := '0';
      mem_addr  := (others => '0');
      mem_wdata := (others => '0');
      mem_be    := (others => '0');

      -- Immediates
      v_imm_i := std_logic_vector(resize(signed(inst(31 downto 20)), 32));
      v_imm_s := std_logic_vector(resize(signed(std_logic_vector'(inst(31 downto 25) & inst(11 downto 7))), 32));
      v_imm_b := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8) & '0')), 32));
      v_imm_u := inst(31 downto 12) & x"000";
      v_imm_j := std_logic_vector(resize(signed(std_logic_vector'(inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21) & '0')), 32));

      -- Operands
      if unsigned(rs1) = 0 then v_op1 := (others => '0'); else v_op1 := signed(regs_r(to_integer(unsigned(rs1)))); end if;
      if unsigned(rs2) = 0 then v_op2 := (others => '0'); else v_op2 := signed(regs_r(to_integer(unsigned(rs2)))); end if;

      -- Default Next PC
      v_npc := std_logic_vector(unsigned(pc_r) + 4);
      v_res := (others => '0');

      case opcode is
        when OPC_LUI =>
          v_res := v_imm_u;
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        when OPC_AUIPC =>
          v_res := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_u));
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        when OPC_JAL =>
          v_res := std_logic_vector(unsigned(pc_r) + 4);
          v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_j));
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        when OPC_JALR =>
          v_res := std_logic_vector(unsigned(pc_r) + 4);
          v_npc := std_logic_vector(unsigned(unsigned(v_op1) + unsigned(v_imm_i)) and x"FFFFFFFE");
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        when OPC_BRANCH =>
          case funct3 is
            when F3_BEQ  => if v_op1 = v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if;
            when F3_BNE  => if v_op1 /= v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if;
            when F3_BLT  => if v_op1 < v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if;
            when F3_BGE  => if v_op1 >= v_op2 then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if;
            when F3_BLTU => if unsigned(v_op1) < unsigned(v_op2) then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if;
            when F3_BGEU => if unsigned(v_op1) >= unsigned(v_op2) then v_npc := std_logic_vector(unsigned(pc_r) + unsigned(v_imm_b)); end if;
            when others => null;
          end case;
          pc_r := v_npc;

        when OPC_LOAD =>
          v_addr := std_logic_vector(resize(unsigned(v_op1) + unsigned(v_imm_i), DMEM_ADDR_WIDTH));
          mem_req  := true;
          mem_we   := '0';
          mem_addr := v_addr;
          
          -- Save state for completion
          pending_load_rd       := rd;
          pending_load_funct3   := funct3;
          pending_load_byte_off := to_integer(unsigned(v_addr(1 downto 0)));
          
          pc_r := v_npc;

        when OPC_STORE =>
          v_addr := std_logic_vector(resize(unsigned(v_op1) + unsigned(v_imm_s), DMEM_ADDR_WIDTH));
          mem_req   := true;
          mem_we    := '1';
          mem_addr  := v_addr;
          v_shamt   := to_integer(unsigned(v_addr(1 downto 0))) * 8;
          mem_wdata := std_logic_vector(shift_left(unsigned(v_op2), v_shamt));
          
          case funct3 is
            when F3_SB  => mem_be := std_logic_vector(shift_left(unsigned'("0001"), to_integer(unsigned(v_addr(1 downto 0)))));
            when F3_SH  => mem_be := std_logic_vector(shift_left(unsigned'("0011"), to_integer(unsigned(v_addr(1 downto 0)))));
            when F3_SW  => mem_be := (others => '1');
            when others => mem_be := (others => '0');
          end case;
          pc_r := v_npc;

        when OPC_OP_IMM | OPC_OP =>
          case funct3 is
            when F3_ADD  => -- ADD / SUB / ADDI
              if opcode = OPC_OP and funct7(5) = '1' then v_res := std_logic_vector(v_op1 - v_op2);
              else                                        v_res := std_logic_vector(v_op1 + v_op2); -- Includes ADDI
              end if;
            when F3_SLL  => v_res := std_logic_vector(shift_left(unsigned(v_op1), to_integer(unsigned(v_op2(4 downto 0)))));
            when F3_SLT  => if v_op1 < v_op2 then v_res := x"00000001"; else v_res := (others => '0'); end if;
            when F3_SLTU => if unsigned(v_op1) < unsigned(v_op2) then v_res := x"00000001"; else v_res := (others => '0'); end if;
            when F3_XOR  => v_res := std_logic_vector(v_op1 xor v_op2);
            when F3_SRL  => -- SRL / SRA / SRLI / SRAI
              if funct7(5) = '1' then v_res := std_logic_vector(shift_right(v_op1, to_integer(unsigned(v_op2(4 downto 0))))); -- SRA
              else                    v_res := std_logic_vector(shift_right(unsigned(v_op1), to_integer(unsigned(v_op2(4 downto 0))))); -- SRL
              end if;
            when F3_OR   => v_res := std_logic_vector(v_op1 or v_op2);
            when F3_AND  => v_res := std_logic_vector(v_op1 and v_op2);
            when others  => null;
          end case;
          
          if unsigned(rd) /= 0 then regs_r(to_integer(unsigned(rd))) := v_res; end if;
          pc_r := v_npc;

        when others => -- NOP, FENCE, SYSTEM
          pc_r := v_npc;
      end case;
    end procedure;

    procedure complete_load(
      mem_rdata     : in std_logic_vector(DMEM_DATA_WIDTH-1 downto 0)
    ) is
      variable v_shamt : integer;
      variable v_rdata : std_logic_vector(31 downto 0);
      variable v_res   : std_logic_vector(31 downto 0);
    begin
      v_shamt := pending_load_byte_off * 8;
      v_rdata := std_logic_vector(shift_right(unsigned(mem_rdata), v_shamt));
      
      case pending_load_funct3 is
        when F3_LB  => v_res := std_logic_vector(resize(signed(v_rdata(7 downto 0)), 32));
        when F3_LH  => v_res := std_logic_vector(resize(signed(v_rdata(15 downto 0)), 32));
        when F3_LW  => v_res := std_logic_vector(resize(unsigned(mem_rdata), 32));
        when F3_LBU => v_res := std_logic_vector(resize(unsigned(v_rdata(7 downto 0)), 32));
        when F3_LHU => v_res := std_logic_vector(resize(unsigned(v_rdata(15 downto 0)), 32));
        when others => v_res := std_logic_vector(resize(unsigned(mem_rdata), 32));
      end case;

      if unsigned(pending_load_rd) /= 0 then 
        regs_r(to_integer(unsigned(pending_load_rd))) := v_res; 
      end if;
    end procedure;

  end protected body;

end package body;