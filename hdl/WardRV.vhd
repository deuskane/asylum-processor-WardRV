-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV.vhd
-- Author     : mrosiere
-- Company    : 
-- Created    : 2026-01-24
-- Last update: 2026-01-28
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2026-01-24  0.0      mrosiere Add parameter for sync/async regfile
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
library asylum;
use     asylum.math_pkg.all;
use     asylum.RV_pkg.all;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_debug_pkg.all;

entity WardRV is
  -- =====[ Parameters ]==========================
  generic (
    
     RESET_ADDR        : std_logic_vector(32-1 downto 0) := (others => '0');
     IT_ADDR           : std_logic_vector(32-1 downto 0) := (others => '0');
     BIG_ENDIAN        : boolean          := false;
     DM_ENABLE         : boolean          := true;
     DEBUG             : boolean          := false
     );
  -- =====[ Interfaces ]==========================
  port (
    clk_i             : in  std_logic;
    arst_b_i          : in  std_logic;

    -- Instructions
    inst_ini_o       : out   inst_ini_t;
    inst_tgt_i       : in    inst_tgt_t;
    
    -- Bus
    sbi_ini_o        : out   sbi_ini_t;
    sbi_tgt_i        : in    sbi_tgt_t;

    -- To/From IT Ctrl
    it_val_i         : in    std_logic;
    it_ack_o         : out   std_logic;

    -- JTAG
    jtag_ini_i       : in    jtag_ini_t;
    jtag_tgt_o       : out   jtag_tgt_t
    );
end WardRV;

architecture rtl of WardRV is

  -- Microcode Types
  type uop_alu_t is (
    UOP_ALU_ADD,
    UOP_ALU_SUB,
    UOP_ALU_AND,
    UOP_ALU_OR,
    UOP_ALU_XOR,
    UOP_ALU_SLT,
    UOP_ALU_SLTU,
    UOP_ALU_SLL,
    UOP_ALU_SRL,
    UOP_ALU_SRA,
    UOP_ALU_COPY_B
  );

  type uop_mem_t is (
    UOP_MEM_NONE,
    UOP_MEM_RD,
    UOP_MEM_WR
  );

  type uop_rf_rd_t is (
    UOP_RF_RD_NONE,
    UOP_RF_RD_RS1,
    UOP_RF_RD_RS2
  );

  type uop_src_a_t is (
    UOP_SRC_A_PC,
    UOP_SRC_A_RF,
    UOP_SRC_A_TMP,
    UOP_SRC_A_ZERO
  );

  type uop_src_b_t is (
    UOP_SRC_B_RF,
    UOP_SRC_B_IMM,
    UOP_SRC_B_4,
    UOP_SRC_B_ZERO
  );

  type uop_rf_wr_t is (
    UOP_RF_NONE,
    UOP_RF_ALU,
    UOP_RF_MEM,
    UOP_RF_PC_PLUS_4
  );

  type uop_seq_t is (
    UOP_SEQ_GOTO,
    UOP_SEQ_DISPATCH,
    UOP_SEQ_WAIT_INST,
    UOP_SEQ_WAIT_SBI
  );

  type uop_inst_t is record
    alu_op : uop_alu_t;
    src_a  : uop_src_a_t;
    src_b  : uop_src_b_t;
    mem_op : uop_mem_t;
    rf_rd  : uop_rf_rd_t;
    rf_wr  : uop_rf_wr_t;
    pc_wr  : boolean;
    seq    : uop_seq_t;
    next_uop : integer range 0 to 15;
  end record;

  -- Microcode Addresses
  constant UOP_ADDR_FETCH   : integer := 0;
  constant UOP_ADDR_DECODE  : integer := 1;
  constant UOP_ADDR_LUI     : integer := 2;
  constant UOP_ADDR_AUIPC   : integer := 3;
  constant UOP_ADDR_JAL     : integer := 4;
  constant UOP_ADDR_JALR    : integer := 5;
  constant UOP_ADDR_BRANCH_RS2 : integer := 6;
  constant UOP_ADDR_LOAD    : integer := 7;
  constant UOP_ADDR_LOAD_WB : integer := 8;
  constant UOP_ADDR_STORE_RS2 : integer := 9;
  constant UOP_ADDR_ALU_IMM : integer := 10;
  constant UOP_ADDR_ALU_REG_RS2 : integer := 11;
  constant UOP_ADDR_ALU_REG_EXEC : integer := 12;
  constant UOP_ADDR_BRANCH_EXEC : integer := 13;
  constant UOP_ADDR_STORE_EXEC  : integer := 14;
  constant UOP_ADDR_DBG_HALT    : integer := 15;

  -- Microcode Table (ROM)
  type uop_rom_t is array (0 to 15) of uop_inst_t;
  constant UOP_ROM : uop_rom_t := (
    -- 0: FETCH (Request)
    UOP_ADDR_FETCH   => (UOP_ALU_ADD,    UOP_SRC_A_PC,   UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_NONE,      false, UOP_SEQ_WAIT_INST, UOP_ADDR_DECODE),
    -- 1: DECODE (Wait Inst & Decode & Read RS1)
    UOP_ADDR_DECODE  => (UOP_ALU_ADD,    UOP_SRC_A_PC,   UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_RS1,  UOP_RF_NONE,      false, UOP_SEQ_DISPATCH,  UOP_ADDR_FETCH),

    -- 2: LUI (RF = Imm)
    UOP_ADDR_LUI     => (UOP_ALU_COPY_B, UOP_SRC_A_ZERO, UOP_SRC_B_IMM,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_ALU,       true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 3: AUIPC (RF = PC + Imm)
    UOP_ADDR_AUIPC   => (UOP_ALU_ADD,    UOP_SRC_A_PC,   UOP_SRC_B_IMM,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_ALU,       true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 4: JAL (RF = PC+4, PC = PC + Imm)
    UOP_ADDR_JAL     => (UOP_ALU_ADD,    UOP_SRC_A_PC,   UOP_SRC_B_IMM,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_PC_PLUS_4, true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 5: JALR (RF = PC+4, PC = RS1 + Imm)
    UOP_ADDR_JALR    => (UOP_ALU_ADD,    UOP_SRC_A_RF,   UOP_SRC_B_IMM,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_PC_PLUS_4, true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 6: BRANCH (Read RS2)
    UOP_ADDR_BRANCH_RS2 => (UOP_ALU_ADD, UOP_SRC_A_ZERO, UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_RS2,  UOP_RF_NONE,      false, UOP_SEQ_GOTO,      UOP_ADDR_BRANCH_EXEC),
    -- 7: LOAD (Addr = RS1 + Imm)
    UOP_ADDR_LOAD    => (UOP_ALU_ADD,    UOP_SRC_A_RF,   UOP_SRC_B_IMM,  UOP_MEM_RD,   UOP_RF_RD_NONE, UOP_RF_NONE,      false, UOP_SEQ_WAIT_SBI,  UOP_ADDR_LOAD_WB),
    -- 8: LOAD WB
    UOP_ADDR_LOAD_WB => (UOP_ALU_ADD,    UOP_SRC_A_ZERO, UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_MEM,       true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 9: STORE (Read RS2)
    UOP_ADDR_STORE_RS2 => (UOP_ALU_ADD,  UOP_SRC_A_ZERO, UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_RS2,  UOP_RF_NONE,      false, UOP_SEQ_GOTO,      UOP_ADDR_STORE_EXEC),
    -- 10: OP_IMM
    UOP_ADDR_ALU_IMM => (UOP_ALU_ADD,    UOP_SRC_A_RF,   UOP_SRC_B_IMM,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_ALU,       true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 11: OP_REG (Read RS2)
    UOP_ADDR_ALU_REG_RS2 => (UOP_ALU_ADD, UOP_SRC_A_ZERO, UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_RS2, UOP_RF_NONE,      false, UOP_SEQ_GOTO,      UOP_ADDR_ALU_REG_EXEC),
    -- 12: OP_REG (Exec)
    UOP_ADDR_ALU_REG_EXEC => (UOP_ALU_ADD, UOP_SRC_A_TMP, UOP_SRC_B_RF,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_ALU,       true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 13: BRANCH (Exec)
    UOP_ADDR_BRANCH_EXEC => (UOP_ALU_ADD, UOP_SRC_A_TMP,  UOP_SRC_B_RF,  UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_NONE,      true,  UOP_SEQ_GOTO,      UOP_ADDR_FETCH),
    -- 14: STORE (Exec)
    UOP_ADDR_STORE_EXEC => (UOP_ALU_ADD,  UOP_SRC_A_TMP,  UOP_SRC_B_IMM, UOP_MEM_WR,   UOP_RF_RD_NONE, UOP_RF_NONE,      true,  UOP_SEQ_WAIT_SBI,  UOP_ADDR_FETCH),
    -- 15: DBG HALT
    UOP_ADDR_DBG_HALT   => (UOP_ALU_ADD,  UOP_SRC_A_ZERO, UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_NONE,      false, UOP_SEQ_GOTO,      UOP_ADDR_DBG_HALT),

    others           => (UOP_ALU_ADD,    UOP_SRC_A_ZERO, UOP_SRC_B_ZERO, UOP_MEM_NONE, UOP_RF_RD_NONE, UOP_RF_NONE,      false, UOP_SEQ_GOTO,      UOP_ADDR_FETCH)
  );

  -- Registers
  signal pc_r, pc_next         : std_logic_vector(31 downto 0);
  signal ir_r                  : std_logic_vector(31 downto 0);
  signal uop_pc_r, uop_pc_next : integer range 0 to 15;

  -- Register File (Inferred as RAM)
  type reg_file_t is array (0 to 31) of std_logic_vector(31 downto 0);
  signal regs         : reg_file_t := (others => (others => '0')); -- Now accessed via process
  signal rf_rdata     : std_logic_vector(31 downto 0) := (others => '0');
  signal rf_addr      : std_logic_vector(4 downto 0);
  signal reg_a        : std_logic_vector(31 downto 0) := (others => '0'); -- Temp register for RS1

  -- Memory Access Signals
  signal mem_wdata         : std_logic_vector(31 downto 0);
  signal mem_be            : std_logic_vector(3 downto 0);
  signal mem_rdata_aligned : std_logic_vector(31 downto 0);

  -- Debug
  signal loc_arst_b : std_logic;
  signal dbg_req    : dbg_req_t;
  signal dbg_rsp    : dbg_rsp_t;

  -- Internal Signals
  signal imm_val      : std_logic_vector(31 downto 0);
  signal alu_res      : std_logic_vector(31 downto 0);
  signal alu_op_a     : std_logic_vector(31 downto 0);
  signal alu_op_b     : std_logic_vector(31 downto 0);
  signal branch_taken : std_logic;
  signal uop_inst     : uop_inst_t;

  -- Decoding
  alias opcode : std_logic_vector(6 downto 0) is ir_r(6 downto 0);
  alias rd     : std_logic_vector(4 downto 0) is ir_r(11 downto 7);
  alias funct3 : std_logic_vector(2 downto 0) is ir_r(14 downto 12);
  alias rs1    : std_logic_vector(4 downto 0) is ir_r(19 downto 15);
  alias rs2    : std_logic_vector(4 downto 0) is ir_r(24 downto 20);
  alias funct7 : std_logic_vector(6 downto 0) is ir_r(31 downto 25);

begin

  -- ==========================================================================
  -- 1. Micro-Sequencer & Control
  -- ==========================================================================
  uop_inst <= UOP_ROM(uop_pc_r);

  p_seq_comb : process(uop_pc_r, uop_inst, opcode, inst_tgt_i, sbi_tgt_i, dbg_req)
  begin
    uop_pc_next <= uop_pc_r; -- Default hold

    if uop_pc_r = UOP_ADDR_DBG_HALT then
      if dbg_req.resume = '1' then
        uop_pc_next <= UOP_ADDR_FETCH;
      else
        uop_pc_next <= UOP_ADDR_DBG_HALT;
      end if;
    elsif dbg_req.halt = '1' and uop_pc_r = UOP_ADDR_FETCH then
      uop_pc_next <= UOP_ADDR_DBG_HALT;
    else
      case uop_inst.seq is
        when UOP_SEQ_GOTO =>
          uop_pc_next <= uop_inst.next_uop;

        when UOP_SEQ_WAIT_INST =>
          if inst_tgt_i.ready = '1' then
            uop_pc_next <= uop_inst.next_uop;
          end if;

        when UOP_SEQ_WAIT_SBI =>
          if sbi_tgt_i.ready = '1' then
            uop_pc_next <= uop_inst.next_uop;
          end if;

        when UOP_SEQ_DISPATCH =>
          case opcode is
            when OPC_LUI      => uop_pc_next <= UOP_ADDR_LUI;
            when OPC_AUIPC    => uop_pc_next <= UOP_ADDR_AUIPC;
            when OPC_JAL      => uop_pc_next <= UOP_ADDR_JAL;
            when OPC_JALR     => uop_pc_next <= UOP_ADDR_JALR;
            when OPC_BRANCH   => uop_pc_next <= UOP_ADDR_BRANCH_RS2;
            when OPC_LOAD     => uop_pc_next <= UOP_ADDR_LOAD;
            when OPC_STORE    => uop_pc_next <= UOP_ADDR_STORE_RS2;
            when OPC_OP_IMM   => uop_pc_next <= UOP_ADDR_ALU_IMM;
            when OPC_OP       => uop_pc_next <= UOP_ADDR_ALU_REG_RS2;
            when others       => uop_pc_next <= UOP_ADDR_FETCH; -- NOP/Trap
          end case;
      end case;
    end if;
  end process;

  -- ==========================================================================
  -- 2. Datapath: Immediate Generation
  -- ==========================================================================
  process(ir_r, opcode)
  begin
    case opcode is
      when OPC_LUI | OPC_AUIPC              => imm_val <= ir_r(31 downto 12) & x"000";
      when OPC_JAL                          => imm_val <= (31 downto 20 => ir_r(31)) & ir_r(19 downto 12) & ir_r(20) & ir_r(30 downto 21) & '0';
      when OPC_JALR | OPC_LOAD | OPC_OP_IMM => imm_val <= (31 downto 11 => ir_r(31)) & ir_r(30 downto 20);
      when OPC_STORE                        => imm_val <= (31 downto 11 => ir_r(31)) & ir_r(30 downto 25) & ir_r(11 downto 7);
      when OPC_BRANCH                       => imm_val <= (31 downto 12 => ir_r(31)) & ir_r(7) & ir_r(30 downto 25) & ir_r(11 downto 8) & '0';
      when others                           => imm_val <= (others => '0');
    end case;
  end process;

  -- ==========================================================================
  -- 3. Datapath: ALU
  -- ==========================================================================
  -- Mux A
  with uop_inst.src_a select
    alu_op_a <= pc_r            when UOP_SRC_A_PC,
                rf_rdata        when UOP_SRC_A_RF,
                reg_a           when UOP_SRC_A_TMP,
                (others => '0') when others;

  -- Mux B
  with uop_inst.src_b select
    alu_op_b <= rf_rdata                             when UOP_SRC_B_RF,
                imm_val                              when UOP_SRC_B_IMM,
                std_logic_vector(to_unsigned(4, 32)) when UOP_SRC_B_4,
                (others => '0')                      when others;

  -- ALU Logic
  process(alu_op_a, alu_op_b, uop_inst, funct3, funct7, opcode)
    variable v_op  : uop_alu_t;
    variable v_sub : boolean;
  begin
    v_op  := uop_inst.alu_op;
    v_sub := false;

    -- Dynamic ALU Op override for OP_IMM and OP
    if uop_inst.alu_op = UOP_ALU_ADD and (opcode = OPC_OP or opcode = OPC_OP_IMM) then
      case funct3 is
        when F3_ADD_SUB => 
          if opcode = OPC_OP and funct7(5) = '1' then v_op := UOP_ALU_SUB; else v_op := UOP_ALU_ADD; end if;
        when F3_SLL     => v_op := UOP_ALU_SLL;
        when F3_SLT     => v_op := UOP_ALU_SLT;
        when F3_SLTU    => v_op := UOP_ALU_SLTU;
        when F3_XOR     => v_op := UOP_ALU_XOR;
        when F3_SRL_SRA => 
          if funct7(5) = '1' then v_op := UOP_ALU_SRA; else v_op := UOP_ALU_SRL; end if;
        when F3_OR      => v_op := UOP_ALU_OR;
        when F3_AND     => v_op := UOP_ALU_AND;
        when others     => v_op := UOP_ALU_ADD;
      end case;
    end if;

    -- Branch comparison logic reuse
    if opcode = OPC_BRANCH then v_op := UOP_ALU_SUB; end if;

    case v_op is
      when UOP_ALU_ADD    => alu_res <= std_logic_vector(unsigned(alu_op_a) + unsigned(alu_op_b));
      when UOP_ALU_SUB    => alu_res <= std_logic_vector(unsigned(alu_op_a) - unsigned(alu_op_b));
      when UOP_ALU_AND    => alu_res <= alu_op_a and alu_op_b;
      when UOP_ALU_OR     => alu_res <= alu_op_a or alu_op_b;
      when UOP_ALU_XOR    => alu_res <= alu_op_a xor alu_op_b;
      when UOP_ALU_SLT    => if signed(alu_op_a) < signed(alu_op_b) then alu_res <= x"00000001"; else alu_res <= (others => '0'); end if;
      when UOP_ALU_SLTU   => if unsigned(alu_op_a) < unsigned(alu_op_b) then alu_res <= x"00000001"; else alu_res <= (others => '0'); end if;
      when UOP_ALU_COPY_B => alu_res <= alu_op_b;
      -- Shifters (Behavioral for brevity, synthesis will optimize)
      when UOP_ALU_SLL    => alu_res <= std_logic_vector(shift_left(unsigned(alu_op_a), to_integer(unsigned(alu_op_b(4 downto 0)))));
      when UOP_ALU_SRL    => alu_res <= std_logic_vector(shift_right(unsigned(alu_op_a), to_integer(unsigned(alu_op_b(4 downto 0)))));
      when UOP_ALU_SRA    => alu_res <= std_logic_vector(shift_right(signed(alu_op_a), to_integer(unsigned(alu_op_b(4 downto 0)))));
      when others         => alu_res <= (others => '0');
    end case;
  end process;

  -- Branch Condition
  process(funct3, alu_res)
    variable v_zero : boolean;
    variable v_neg  : boolean;
  begin
    v_zero := (unsigned(alu_res) = 0);
    v_neg  := (alu_res(31) = '1');
    branch_taken <= '0';
    case funct3 is
      when F3_BEQ  => if v_zero then branch_taken <= '1'; end if;
      when "001"   => if not v_zero then branch_taken <= '1'; end if; -- BNE
      when F3_SLT  => if v_neg then branch_taken <= '1'; end if;      -- BLT
      when F3_SLTU => if v_neg then branch_taken <= '1'; end if;      -- BLTU (using SUB result)
      when "101"   => if not v_neg then branch_taken <= '1'; end if;  -- BGE
      when "111"   => if not v_neg then branch_taken <= '1'; end if;  -- BGEU
      when others  => null;
    end case;
  end process;

  -- ==========================================================================
  -- 4. Sequential Logic (PC, Regs, State)
  -- ==========================================================================
  loc_arst_b <= arst_b_i and not dbg_req.ndmreset;

  process(clk_i, loc_arst_b)
  begin
    if loc_arst_b = '0' then
      uop_pc_r <= UOP_ADDR_FETCH;
      pc_r     <= RESET_ADDR;
      ir_r     <= (others => '0');
    elsif rising_edge(clk_i) then
      uop_pc_r <= uop_pc_next;

      -- PC Update
      if uop_inst.pc_wr then
        if uop_pc_r = UOP_ADDR_BRANCH_EXEC then
          if branch_taken = '1' then
            pc_r <= std_logic_vector(unsigned(pc_r) + unsigned(imm_val));
          else
            pc_r <= std_logic_vector(unsigned(pc_r) + 4);
          end if;
        elsif opcode = OPC_JAL or opcode = OPC_JALR then
           pc_r <= alu_res; -- JAL/JALR target calculated in ALU
        else
           pc_r <= std_logic_vector(unsigned(pc_r) + 4);
        end if;
      end if;

      -- IR Update
      if uop_pc_r = UOP_ADDR_FETCH and inst_tgt_i.ready = '1' then
        ir_r <= inst_tgt_i.inst;
      end if;

      -- Temp Register A Capture (Save RS1 when switching to RS2)
      if uop_inst.rf_rd = UOP_RF_RD_RS2 then
        reg_a <= rf_rdata;
      end if;
    end if;
  end process;

  -- Register File (Synchronous Single Port)
  rf_addr <= rs1 when uop_inst.rf_rd = UOP_RF_RD_RS1 else rs2;

  process(clk_i)
  begin
    if rising_edge(clk_i) then
      -- Write
      if uop_inst.rf_wr /= UOP_RF_NONE and rd /= "00000" then
        if uop_inst.rf_wr = UOP_RF_MEM then
          regs(to_integer(unsigned(rd))) <= mem_rdata_aligned;
        elsif uop_inst.rf_wr = UOP_RF_PC_PLUS_4 then
          regs(to_integer(unsigned(rd))) <= std_logic_vector(unsigned(pc_r) + 4);
        else
          regs(to_integer(unsigned(rd))) <= alu_res;
        end if;
      end if;

      -- Read
      rf_rdata <= regs(to_integer(unsigned(rf_addr)));
    end if;
  end process;

  -- ==========================================================================
  -- 5. Memory Interface Logic (Alignment & Endianness)
  -- ==========================================================================
  
  -- Write Data & Byte Enable Generation
  process(funct3, rf_rdata, alu_res)
    variable v_off   : integer;
    variable v_byte  : std_logic_vector(7 downto 0);
    variable v_half  : std_logic_vector(15 downto 0);
    variable v_wdata : std_logic_vector(31 downto 0);
  begin
    v_off   := to_integer(unsigned(alu_res(1 downto 0)));
    v_byte  := rf_rdata(7 downto 0);
    v_wdata := (others => '0');
    mem_be  <= "0000";

    -- Prepare Half-Word (Swap if Big Endian)
    if BIG_ENDIAN then
      v_half := rf_rdata(7 downto 0) & rf_rdata(15 downto 8);
    else
      v_half := rf_rdata(15 downto 0);
    end if;

    case funct3 is
      when F3_SB =>
        mem_be(v_off) <= '1';
        v_wdata := v_byte & v_byte & v_byte & v_byte; -- Replicate byte to all lanes

      when F3_SH =>
        if v_off = 0 then mem_be <= "0011"; else mem_be <= "1100"; end if;
        v_wdata := v_half & v_half; -- Replicate half to both positions

      when F3_SW =>
        mem_be <= "1111";
        if BIG_ENDIAN then
          v_wdata(31 downto 24) := rf_rdata(7 downto 0);
          v_wdata(23 downto 16) := rf_rdata(15 downto 8);
          v_wdata(15 downto 8)  := rf_rdata(23 downto 16);
          v_wdata(7 downto 0)   := rf_rdata(31 downto 24);
        else
          v_wdata := rf_rdata;
        end if;

      when others => 
        mem_be <= "0000";
    end case;
    mem_wdata <= v_wdata;
  end process;

  -- Read Data Alignment & Sign Extension
  process(funct3, sbi_tgt_i.rdata, alu_res)
    variable v_off   : integer;
    variable v_rdata : std_logic_vector(31 downto 0);
    variable v_byte  : std_logic_vector(7 downto 0);
    variable v_half  : std_logic_vector(15 downto 0);
  begin
    v_off   := to_integer(unsigned(alu_res(1 downto 0)));
    
    -- Shift data to LSB based on address
    v_rdata := std_logic_vector(shift_right(unsigned(sbi_tgt_i.rdata), v_off * 8));
    v_byte  := v_rdata(7 downto 0);
    v_half  := v_rdata(15 downto 0);

    -- Swap if Big Endian (for sub-word accesses)
    if BIG_ENDIAN then
      v_half := v_half(7 downto 0) & v_half(15 downto 8);
    end if;

    mem_rdata_aligned <= (others => '0');

    case funct3 is
      when F3_LB  => mem_rdata_aligned <= std_logic_vector(resize(signed(v_byte), 32));
      when F3_LBU => mem_rdata_aligned <= std_logic_vector(resize(unsigned(v_byte), 32));
      when F3_LH  => mem_rdata_aligned <= std_logic_vector(resize(signed(v_half), 32));
      when F3_LHU => mem_rdata_aligned <= std_logic_vector(resize(unsigned(v_half), 32));
      when F3_LW  => 
        if BIG_ENDIAN then
          mem_rdata_aligned(31 downto 24) <= sbi_tgt_i.rdata(7 downto 0);
          mem_rdata_aligned(23 downto 16) <= sbi_tgt_i.rdata(15 downto 8);
          mem_rdata_aligned(15 downto 8)  <= sbi_tgt_i.rdata(23 downto 16);
          mem_rdata_aligned(7 downto 0)   <= sbi_tgt_i.rdata(31 downto 24);
        else
          mem_rdata_aligned <= sbi_tgt_i.rdata;
        end if;
      when others => null;
    end case;
  end process;

  -- Outputs
  inst_ini_o.valid <= '1' when uop_pc_r = UOP_ADDR_FETCH else '0';
  inst_ini_o.addr  <= pc_r;

  -- Debug Process
  p_debug : process(clk_i)
    function to_hex_string(slv : std_logic_vector) return string is
      variable hex_digits : string(1 to 16) := "0123456789abcdef";
      variable result     : string(1 to slv'length/4);
      variable nibble     : integer;
    begin
      for i in result'range loop
        nibble := to_integer(unsigned(slv(slv'length - (i-1)*4 - 1 downto slv'length - i*4)));
        result(i) := hex_digits(nibble + 1);
      end loop;
      return result;
    end function;
  begin
    if rising_edge(clk_i) then
      if DEBUG then
        -- Microcode Instruction
        report "UOP PC: " & integer'image(uop_pc_r) & 
               " | ALU: " & uop_alu_t'image(uop_inst.alu_op) &
               " | SEQ: " & uop_seq_t'image(uop_inst.seq);

        -- Instruction Fetched
        if uop_pc_r = UOP_ADDR_FETCH and inst_tgt_i.ready = '1' then
          report "FETCH: Inst=0x" & to_hex_string(inst_tgt_i.inst) & " @ PC=0x" & to_hex_string(pc_r);
        end if;

        -- Memory Access
        if (uop_pc_r = UOP_ADDR_LOAD or uop_pc_r = UOP_ADDR_STORE_EXEC) then
           if uop_inst.mem_op = UOP_MEM_WR then
             report "MEM WRITE: Data=0x" & to_hex_string(mem_wdata) & " @ Addr=0x" & to_hex_string(alu_res);
           elsif uop_inst.mem_op = UOP_MEM_RD then
             report "MEM READ Req @ Addr=0x" & to_hex_string(alu_res);
           end if;
        end if;
      end if;
    end if;
  end process;

  sbi_ini_o.valid  <= '1' when (uop_pc_r = UOP_ADDR_LOAD or uop_pc_r = UOP_ADDR_STORE_EXEC) else '0';
  sbi_ini_o.addr   <= alu_res;
  sbi_ini_o.wdata  <= mem_wdata;
  sbi_ini_o.we     <= '1' when uop_inst.mem_op = UOP_MEM_WR else '0';
  sbi_ini_o.be     <= mem_be;

  it_ack_o <= '0'; -- Stub

  -- Debug Module
  gen_dm : if DM_ENABLE generate
    u_debug : WardRV_debug
      generic map (
        IDCODE_VALUE => x"10000001"
      )
      port map (
        jtag_ini_i => jtag_ini_i,
        jtag_tgt_o => jtag_tgt_o,
        clk_i     => clk_i,
        arst_b_i  => arst_b_i,
        dbg_req_o => dbg_req,
        dbg_rsp_i => dbg_rsp
      );
  end generate gen_dm;

  gen_no_dm : if not DM_ENABLE generate
    dbg_req.halt     <= '0';
    dbg_req.resume   <= '0';
    dbg_req.ndmreset <= '0';
    jtag_tgt_o.tdo   <= '0';
    jtag_tgt_o.tdo_en<= '0';
  end generate gen_no_dm;

  dbg_rsp.halted  <= '1' when uop_pc_r = UOP_ADDR_DBG_HALT else '0';
  dbg_rsp.running <= '1' when uop_pc_r /= UOP_ADDR_DBG_HALT else '0';

end rtl;
