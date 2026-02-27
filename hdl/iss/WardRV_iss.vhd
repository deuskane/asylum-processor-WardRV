-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_iss.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-------------------------------------------------------------------------------

library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

library asylum;
use     asylum.WardRV_pkg.all;
use     asylum.WardRV_iss_pkg.all;

entity WardRV_iss is
  generic (
    RESET_ADDR : std_logic_vector(31 downto 0) := (others => '0');
    VERBOSE    : boolean                       := false
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

architecture rtl of WardRV_iss is

    type state_t is (S_FETCH, S_EXECUTE);
    shared variable iss : iss_t;
    signal   state      : state_t;
    shared variable mem_access : boolean;
    shared variable mem_we     : std_logic;
    shared variable mem_be     : std_logic_vector(3 downto 0);
    shared variable mem_addr   : std_logic_vector(31 downto 0);
    shared variable mem_wdata  : std_logic_vector(31 downto 0);
    shared variable mem_rdata  : std_logic_vector(31 downto 0);
begin
    -- Combinational logic for instruction fetch
    inst_ini_o.valid <= '1' when state = S_FETCH else '0';
    
    
    -- Combinational logic for instruction execution
    -- Always execute the instruction to determine if it is a memory access, but only assert valid when in EXECUTE state
    process(all)
    begin
        iss.execute_instruction(inst_tgt_i.inst, 
                                mem_access      ,
                                mem_we          ,     
                                mem_addr        , 
                                mem_wdata       , 
                                mem_be          );
        sbi_ini_o.valid <= '1' when (state = S_EXECUTE) and mem_access else '0';
        sbi_ini_o.we    <= mem_we;
        sbi_ini_o.be    <= mem_be;
        sbi_ini_o.addr  <= mem_addr;
        sbi_ini_o.wdata <= mem_wdata;

        if mem_access and (mem_we = '0') then
            iss.complete_load(sbi_tgt_i.rdata);
        end if;
    end process;

    process(clk_i, arst_b_i)
    begin
        if arst_b_i = '0' 
        then
            iss.reset(RESET_ADDR);
            iss.set_verbose(VERBOSE);
            state <= S_FETCH;
        elsif rising_edge(clk_i) 
        then
            case state is
                when S_FETCH =>

                    if inst_tgt_i.ready = '1' 
                    then
                        state <= S_EXECUTE;
                    end if;

                when S_EXECUTE =>

                    if not mem_access or (mem_access and (sbi_tgt_i.ready = '1')) 
                    then
                        iss.complete;
                        state <= S_FETCH;
                    end if;
            end case;

        end if;
        inst_ini_o.addr  <= iss.get_pc;
    end process;

end architecture rtl;