-------------------------------------------------------------------------------
-- Title      : WardRV
-- Project    : 
-------------------------------------------------------------------------------
-- File       : WardRV_pkg.vhd
-- Author     : Mathieu Rosiere
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2026
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author   Description
-- 2026-02-01  1.0      mrosiere Created
-- 2026-05-09  1.1      mrosiere Rename recorded types
-- 2026-11-22  1.2      mrosiere JTAG records moved to asylum.jtag_pkg
--                               (asylum-debug-jtag)
-- 2026-11-23  1.3      mrosiere vote : majority of three buses (TMR)
-------------------------------------------------------------------------------

library IEEE;
use     IEEE.STD_LOGIC_1164.ALL;
use     IEEE.NUMERIC_STD.ALL;

library asylum;
use     asylum.ft_pkg.all;

package WardRV_pkg is

  type imem_ini_t is record
    valid        : std_logic;                       -- Access Valid
    addr         : std_logic_vector(32-1 downto 0); -- Instruction Address
  end record imem_ini_t;

  type imem_tgt_t is record
    ready        : std_logic;                       -- Access Ready
    inst         : std_logic_vector(32-1 downto 0); -- Instruction
  end record imem_tgt_t;

  type dmem_ini_t is record
    valid        : std_logic;
    addr         : std_logic_vector(32-1 downto 0);
    wdata        : std_logic_vector(32-1 downto 0);
    we           : std_logic;
    be           : std_logic_vector(4-1 downto 0);
  end record dmem_ini_t;

  type dmem_tgt_t is record
    ready        : std_logic;
    rdata        : std_logic_vector(32-1 downto 0);
    err          : std_logic;
  end record dmem_tgt_t;

  -- Majority vote of three copies of a bus (TMR : three CPUs in lockstep) :
  -- each bit of each field takes the value of at least two copies
  function vote (i0, i1, i2 : imem_ini_t) return imem_ini_t;
  function vote (i0, i1, i2 : imem_tgt_t) return imem_tgt_t;
  function vote (i0, i1, i2 : dmem_ini_t) return dmem_ini_t;
  function vote (i0, i1, i2 : dmem_tgt_t) return dmem_tgt_t;

-- [COMPONENT_INSERT][BEGIN]
component WardRV_iss is
  -- =====[ Parameters ]==========================
  generic (
    
     RESET_ADDR        : std_logic_vector(32-1 downto 0) := (others => '0')
     --IT_ADDR           : std_logic_vector(32-1 downto 0) := (others => '0');
     --BIG_ENDIAN        : boolean          := false;
     --DM_ENABLE         : boolean          := true;
     --DEBUG             : boolean          := false
     );
  -- =====[ Interfaces ]==========================
  port (
    clk_i             : in  std_logic;
    arst_b_i          : in  std_logic;

    -- Instructions
    imem_ini_o       : out   imem_ini_t;
    imem_tgt_i       : in    imem_tgt_t;
    
    -- Bus
    dmem_ini_o       : out   dmem_ini_t;
    dmem_tgt_i       : in    dmem_tgt_t

    ---- To/From IT Ctrl
    --it_val_i         : in    std_logic;
    --it_ack_o         : out   std_logic;
--
    ---- JTAG
    --jtag_ini_i       : in    jtag_ini_t;
    --jtag_tgt_o       : out   jtag_tgt_t
    );
end component WardRV_iss;

-- [COMPONENT_INSERT][END]

end WardRV_pkg;

package body WardRV_pkg is

  -- Each field is voted by the TMR decoder of ft_pkg : decode(c2 & c1 & c0,
  -- FT_TMR).data is the bitwise majority of the copies c0, c1 and c2 (the
  -- layout of encode(data, FT_TMR)), a std_logic as a vector of 1 bit
  function vote (i0, i1, i2 : imem_ini_t) return imem_ini_t is
    variable z : imem_ini_t;
  begin
    z.valid := decode(i2.valid & i1.valid & i0.valid, FT_TMR).data(0);
    z.addr  := decode(i2.addr  & i1.addr  & i0.addr , FT_TMR).data;
    return z;
  end function vote;

  function vote (i0, i1, i2 : imem_tgt_t) return imem_tgt_t is
    variable z : imem_tgt_t;
  begin
    z.ready := decode(i2.ready & i1.ready & i0.ready, FT_TMR).data(0);
    z.inst  := decode(i2.inst  & i1.inst  & i0.inst , FT_TMR).data;
    return z;
  end function vote;

  function vote (i0, i1, i2 : dmem_ini_t) return dmem_ini_t is
    variable z : dmem_ini_t;
  begin
    z.valid := decode(i2.valid & i1.valid & i0.valid, FT_TMR).data(0);
    z.addr  := decode(i2.addr  & i1.addr  & i0.addr , FT_TMR).data;
    z.wdata := decode(i2.wdata & i1.wdata & i0.wdata, FT_TMR).data;
    z.we    := decode(i2.we    & i1.we    & i0.we   , FT_TMR).data(0);
    z.be    := decode(i2.be    & i1.be    & i0.be   , FT_TMR).data;
    return z;
  end function vote;

  function vote (i0, i1, i2 : dmem_tgt_t) return dmem_tgt_t is
    variable z : dmem_tgt_t;
  begin
    z.ready := decode(i2.ready & i1.ready & i0.ready, FT_TMR).data(0);
    z.rdata := decode(i2.rdata & i1.rdata & i0.rdata, FT_TMR).data;
    z.err   := decode(i2.err   & i1.err   & i0.err  , FT_TMR).data(0);
    return z;
  end function vote;

end package body WardRV_pkg;
