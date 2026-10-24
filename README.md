<!--
  README GENERATION INSTRUCTIONS (for the next regeneration run)
  ----------------------------------------------------------------
  This README follows the common Asylum IP model. Regenerate it from the
  sources, never from the previous README text alone.

  Sources of truth (in priority order):
    1. hdl/*.vhd            : entities, generics, ports, packages
    2. hdl/csr/*.hjson      : register map (regtool); *_csr.md/.h are generated
    3. <IP>.core            : VLNV (name), filesets, targets, depends, revisions
    4. mk/targets.txt       : target list shown by `make help`; mk/defs.mk
    5. sim/, syn/, esw/, boards/ : testbenches, constraints, software
  Section order (keep it, same headings in every IP):
    CI badge / Title + one-line description + VLNV / Table of Contents /
    Introduction (Key Features) / Block Diagram / Top-Level (Parameters,
    Ports, Instantiation Example) / HDL Modules / Register Map /
    Verification / Synthesis / Design Notes (optional) /
    Directory Structure / Dependencies
  Rules:
    - Language: English. Tables: Parameters = Name|Type|Default|Description,
      Ports = Name|Direction|Type|Description (grouped by interface).
    - Register Map: link to the generated hdl/csr/<X>_csr.md (plus the
      .hjson source and _csr.h header); never copy register tables here.
    - Top-Level = sbi_* wrapper if present, else the entity used by the
      `default` target, else the main entity (libraries: list packages).
    - Write "This IP has no software-visible registers." / "No dedicated
      synthesis target ..." instead of removing a section.
    - Keep still-accurate hand-written content (ISA tables, results,
      images) in "Design Notes"; drop anything not backed by the sources.
    - Block diagram: doc/<NAME>.drawio (NAME = 4th field of the VLNV),
      top entity box with generics on top, inputs left, outputs right,
      bus interfaces as bold arrows, internal blocks colour-coded
      (CSR yellow, FIFO/memory green, core logic blue, external grey).
      Update it whenever ports/generics/sub-blocks change.
    - Do not edit generated files (hdl/csr/*_csr.*) or the CI badge URL.
-->
[![CI](https://github.com/deuskane/asylum-processor-WardRV/actions/workflows/ci.yml/badge.svg)](https://github.com/deuskane/asylum-processor-WardRV/actions/workflows/ci.yml)

# asylum-processor-WardRV

**Academic 32-bit RISC-V processor (RV32I + Zicsr, machine mode) with a multi-cycle FSM core, an SBI wrapper and a behavioural instruction-set simulator.**

VLNV: `asylum:processor:WardRV:0.0.5`

## Table of Contents

1. [Introduction](#introduction)
2. [Block Diagram](#block-diagram)
3. [Top-Level](#top-level)
4. [HDL Modules](#hdl-modules)
5. [Debug Unit](#debug-unit)
6. [Register Map](#register-map)
7. [Verification](#verification)
8. [Synthesis](#synthesis)
9. [Design Notes](#design-notes)
10. [Directory Structure](#directory-structure)
11. [Dependencies](#dependencies)

## Introduction

WardRV is a small RISC-V processor written for teaching and for the Asylum SoC. The implementation used in hardware, `WardRV_fsm`, is a multi-cycle machine: a single FSM (FETCH, DECODE, EXECUTE, BRANCH_DECISION, MEMORY, WRITEBACK, TRAP) sequences one shared 32-bit ALU, a 32 x 32-bit register file, a minimal machine-mode CSR bank and separate instruction (`imem`) and data (`dmem`) interfaces. `WardRV_iss` is a behavioural instruction-set simulator with the same interfaces, used as a reference model in the testbench.

`sbi_WardRV_fsm` packages the FSM core with the same port list as `sbi_OpenBlaze8` (instruction memory port + 8-bit SBI bus + interrupt) so that the PicoSoC can select either CPU (`cpu_wrapper`, `CPU_MODEL = "WardRV_fsm"`). Its `dmem2sbi` bridge splits each 32-bit data access into up to four 8-bit SBI accesses.

### Key Features

- RV32I base instructions: LUI, AUIPC, JAL, JALR, BEQ / BNE / BLT / BGE / BLTU / BGEU, LB / LH / LW / LBU / LHU, SB / SH / SW, OP-IMM and OP arithmetic, logic, shifts and comparisons
- Zicsr: CSRRW / CSRRS / CSRRC and immediate variants; MRET
- Machine-mode CSRs: `mstatus`, `mie`, `mip`, `mtvec`, `mscratch`, `mepc`, `mcause`, `mtval`, `mhartid` (`HARTID` generic)
- Machine external interrupt (`meip_i` -> `mip.MEIP`), trap to `mtvec` (direct mode), `mcause = 0x8000000B`
- Multi-cycle FSM sharing a single ALU (PC + 4, address computation, branch comparison, execution)
- Register file in a `ram_2r1w` (2 asynchronous read ports, 1 write port), `x0` forced to zero
- Byte / half-word / word loads and stores with byte enables and sign / zero extension
- Configurable reset address (`RESET_ADDR`) and instruction address width / alignment in the SBI wrapper
- Simulation-only execution trace (`exec_fsm.log`, `exec_iss.log`) and instruction statistics
- Optional RISC-V debug unit (Debug Specification 1.0, see [Debug Unit](#debug-unit)): JTAG TAP (IEEE 1149.1) + DTM, DMI clock domain crossing, execution-based Debug Module (halt / resume / step, abstract register access, program buffer, optional System Bus Access) and core debug mode (`DEBUG_ENABLE`), mcontrol6 triggers (`NB_TRIGGERS`), EBREAK breakpoint exception (`EBREAK_EXCEPTION`), halt on reset (`RESETHALTREQ_ENABLE`)
- Verified with a self-checking C test and the RISC-V architecture (compliance) tests: 39 ACT3 tests (riscv-arch-test 3.10.0) with signature comparison and 45 ACT4 self-checking tests (riscv-arch-test 4.1.0, I and Zicsr)

## Block Diagram

Diagram: [doc/WardRV.drawio](doc/WardRV.drawio) (open with diagrams.net or the VS Code Draw.io extension).

- `WardRV_fsm_fetch` requests the instruction at `pc` on `imem` and registers it; in `sbi_WardRV_fsm` the instruction is ready one cycle after `ics_o` (`ready = cke_i and ics_r`), which fits a synchronous ROM.
- `WardRV_fsm_decode` (combinational) produces the immediates, register addresses and all control signals; `WardRV_fsm_regfile` provides `rs1` / `rs2`.
- The FSM drives the shared `WardRV_fsm_alu`: PC + 4 in FETCH, the instruction operation in EXECUTE (result kept in `alu_res_r`), `rs1 - rs2` in BRANCH_DECISION (zero, borrow and signed less-than flags).
- `WardRV_fsm_memory` aligns store data / byte enables and extracts load data; `dmem2sbi` turns each `dmem` request into one SBI byte access per enabled byte.
- `WardRV_fsm_csr` holds the machine CSRs, raises the trap request on an enabled external interrupt and provides `mtvec` / `mepc` for trap entry and MRET.

## Top-Level

Top-level entity: **`sbi_WardRV_fsm`** ([hdl/fsm/sbi_WardRV_fsm.vhd](hdl/fsm/sbi_WardRV_fsm.vhd)), library `asylum` (no component declaration, instantiate with `entity asylum.sbi_WardRV_fsm`).

The `default` and `lint_nxmap` targets of [WardRV.core](WardRV.core) use `sbi_WardRV_fsm` as toplevel; it is also the entity instantiated by the PicoSoC.

### Parameters

| Name | Type | Default | Description |
|------|------|---------|-------------|
| `HARTID` | std_logic_vector(31 downto 0) | `(others => '0')` | Value of the `mhartid` CSR |
| `RESET_ADDR` | std_logic_vector(31 downto 0) | `(others => '0')` | PC after reset |
| `IADDR_WIDTH` | natural | `32` | Width of `iaddr_o` |
| `IADDR_ALIGN_BITS` | natural | `0` | Number of PC LSBs dropped on `iaddr_o` (2 = word address). `IADDR_WIDTH + IADDR_ALIGN_BITS` must not exceed 32 |
| `DEBUG_ENABLE` | boolean | `false` | Debug mode, debug CSRs and debug memory bus (see [Debug Unit](#debug-unit)) |
| `NB_TRIGGERS` | natural | `0` | Number of mcontrol6 triggers (needs `DEBUG_ENABLE`) |
| `EBREAK_EXCEPTION` | boolean | `false` | EBREAK raises a breakpoint exception (`mcause = 3`) instead of being a NOP |
| `RESETHALTREQ_ENABLE` | boolean | `false` | Halt before the first instruction after reset when the DM requests it |

### Ports

#### Clock & Reset

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `clk_i` | in | std_logic | Clock |
| `cke_i` | in | std_logic | Clock enable, only used to qualify the instruction ready (`ready = cke_i and ics_r`) |
| `arstn_i` | in | std_logic | Asynchronous reset, active low |

#### Instruction

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `ics_o` | out | std_logic | Instruction fetch request (`imem.valid`) |
| `iaddr_o` | out | std_logic_vector(IADDR_WIDTH-1 downto 0) | Instruction address, `pc(IADDR_WIDTH-1+IADDR_ALIGN_BITS downto IADDR_ALIGN_BITS)` |
| `idata_i` | in | std_logic_vector(31 downto 0) | Instruction, sampled one cycle after `ics_o` |

#### Bus (SBI)

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `sbi_ini_o` | out | sbi_ini_t | SBI request from `dmem2sbi`: byte accesses (`cs`, `re`, `we`, `addr` = word address & byte index, 8-bit `wdata`) |
| `sbi_tgt_i` | in | sbi_tgt_t | SBI response (`ready`, 8-bit `rdata`) |

#### Interrupts

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `interrupt_i` | in | std_logic | Machine external interrupt request (`meip_i`) |
| `interrupt_ack_o` | out | std_logic | Not implemented, tied to `'0'` |

#### Debug

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `dbg_hart_req_i` | in | dbg_hart_req_t | From `WardRV_dbg_dm` (`haltreq`, `resethaltreq`), default `DBG_HART_REQ_NULL` |
| `dbg_sbi_ini_o` | out | dbg_sbi_ini_t | Debug memory accesses (11-bit byte address, 32-bit data), to `WardRV_dbg_dm` |
| `dbg_sbi_tgt_i` | in | dbg_sbi_tgt_t | Debug memory response, default `DBG_SBI_TGT_NULL` |

### Instantiation Example

```vhdl
library asylum;
use     asylum.sbi_pkg.all;

  ins_cpu : entity asylum.sbi_WardRV_fsm
    generic map
    ( HARTID           => x"00000000"
     ,RESET_ADDR       => x"00000000"
     ,IADDR_WIDTH      => 10
     ,IADDR_ALIGN_BITS => 2
    )
    port map
    ( clk_i           => clk
     ,cke_i           => '1'
     ,arstn_i         => arst_b
     ,ics_o           => inst_cs
     ,iaddr_o         => inst_addr        -- word address (9 downto 0)
     ,idata_i         => inst_data        -- (31 downto 0)
     ,sbi_ini_o       => sbi_ini          -- sbi_ini_t(addr(7 downto 0), wdata(7 downto 0))
     ,sbi_tgt_i       => sbi_tgt          -- sbi_tgt_t(rdata(7 downto 0))
     ,interrupt_i     => it
     ,interrupt_ack_o => open
    );
```

The PicoSoC instantiates it through `cpu_wrapper` with `RESET_ADDR => x"00000000"`, `IADDR_WIDTH => iaddr_o'length` and `IADDR_ALIGN_BITS => 2`. The instruction ROM is generated by the `rvcc_gen` generator of `asylum:utils:generators` (default entity `WardRV_ROM`).

## HDL Modules

| File | Unit | Kind | Role |
|------|------|------|------|
| [hdl/RV_pkg.vhd](hdl/RV_pkg.vhd) | `RV_pkg` | package | RISC-V encodings: opcodes, funct3 / funct7 / funct12 (RV32I, system, C, Zba / Zbb constants) and machine CSR addresses |
| [hdl/WardRV_pkg.vhd](hdl/WardRV_pkg.vhd) | `WardRV_pkg` | package | Interface records `imem_ini_t` / `imem_tgt_t`, `dmem_ini_t` / `dmem_tgt_t`, `jtag_ini_t` / `jtag_tgt_t`; component `WardRV_iss` |
| [hdl/WardRV_stats_pkg.vhd](hdl/WardRV_stats_pkg.vhd) | `WardRV_stats_pkg` | package | Instruction types (`inst_type_t`), trace record `inst_t` and `print_inst`, protected type `WardRV_stats` (per-instruction counters) |
| [hdl/bridge/dmem2sbi.vhd](hdl/bridge/dmem2sbi.vhd) | `dmem2sbi` | entity | 32-bit `dmem` to 8-bit SBI bridge (one SBI access per enabled byte) |
| [hdl/bridge/imem2sbi.vhd](hdl/bridge/imem2sbi.vhd) | `imem2sbi` | entity | 32-bit `imem` to 8-bit SBI bridge (four SBI reads per fetch); not instantiated in this repository |
| [hdl/iss/WardRV_iss_pkg.vhd](hdl/iss/WardRV_iss_pkg.vhd) | `WardRV_iss_pkg` | package | Protected type `iss_t`: RV32I instruction-set simulator (reset, execute, load completion, trace, statistics) |
| [hdl/iss/WardRV_iss.vhd](hdl/iss/WardRV_iss.vhd) | `WardRV_iss` | entity | ISS wrapped with the `imem` / `dmem` interfaces (behavioural, FETCH / EXECUTE) |
| [hdl/fsm/WardRV_decode_pkg.vhd](hdl/fsm/WardRV_decode_pkg.vhd) | `WardRV_decode_pkg` | package | ALU source, PC source and write-back source selection constants |
| [hdl/fsm/WardRV_fsm_fetch.vhd](hdl/fsm/WardRV_fsm_fetch.vhd) | `WardRV_fsm_fetch` | entity | Instruction fetch handshake and instruction register |
| [hdl/fsm/WardRV_fsm_alu.vhd](hdl/fsm/WardRV_fsm_alu.vhd) | `WardRV_fsm_alu_pkg` | package | ALU operation type `alu_op_t` |
| [hdl/fsm/WardRV_fsm_alu.vhd](hdl/fsm/WardRV_fsm_alu.vhd) | `WardRV_fsm_alu` | entity | 32-bit ALU with zero / sign / carry flags |
| [hdl/fsm/WardRV_fsm_decode.vhd](hdl/fsm/WardRV_fsm_decode.vhd) | `WardRV_fsm_decode` | entity | Combinational instruction decoder |
| [hdl/fsm/WardRV_fsm_regfile.vhd](hdl/fsm/WardRV_fsm_regfile.vhd) | `WardRV_fsm_regfile` | entity | 32 x 32-bit register file (`ram_2r1w`) |
| [hdl/fsm/WardRV_fsm_memory.vhd](hdl/fsm/WardRV_fsm_memory.vhd) | `WardRV_fsm_memory` | entity | Load / store alignment and `dmem` handshake |
| [hdl/fsm/WardRV_fsm_csr.vhd](hdl/fsm/WardRV_fsm_csr.vhd) | `WardRV_fsm_csr` | entity | Machine CSRs, trap / MRET update, interrupt request |
| [hdl/fsm/WardRV_fsm.vhd](hdl/fsm/WardRV_fsm.vhd) | `WardRV_fsm` | entity | Multi-cycle RV32I core (FSM, PC, datapath muxes) |
| [hdl/fsm/sbi_WardRV_fsm.vhd](hdl/fsm/sbi_WardRV_fsm.vhd) | `sbi_WardRV_fsm` | entity | Top-level: `WardRV_fsm` + `dmem2sbi` with the `sbi_OpenBlaze8`-compatible interface |
| [hdl/fsm/WardRV_fsm_debug.vhd](hdl/fsm/WardRV_fsm_debug.vhd) | `WardRV_fsm_debug` | entity | Core debug mode: dcsr / dpc / dscratch0-1, debug mode entry / exit, step, EBREAK exception, trigger kill |
| [hdl/fsm/WardRV_fsm_trigger.vhd](hdl/fsm/WardRV_fsm_trigger.vhd) | `WardRV_fsm_trigger` | entity | Trigger module: tselect / tdata1 / tdata2 / tdata3 / tinfo, mcontrol6 address match (execute, load, store) |
| [hdl/debug/WardRV_debug_pkg.vhd](hdl/debug/WardRV_debug_pkg.vhd) | `WardRV_debug_pkg` | package | JTAG instruction codes, DMI records `dmi_req_t` / `dmi_rsp_t`, DM register addresses, `dbg_hart_req_t`, debug memory map and `dbg_sbi_ini_t` / `dbg_sbi_tgt_t`, RV32I encoders, components |
| [hdl/debug/WardRV_dbg_rom_pkg.vhd](hdl/debug/WardRV_dbg_rom_pkg.vhd) | `WardRV_dbg_rom_pkg` | package | Debug ROM, generated from `esw/debug_rom/debug_rom.S` by `gen_rom.py` |
| [hdl/debug/WardRV_dbg_tap.vhd](hdl/debug/WardRV_dbg_tap.vhd) | `WardRV_dbg_tap` | entity | IEEE 1149.1 TAP controller: IR, IDCODE, BYPASS, user data register interface (TCK domain) |
| [hdl/debug/WardRV_dbg_dtm.vhd](hdl/debug/WardRV_dbg_dtm.vhd) | `WardRV_dbg_dtm` | entity | RISC-V DTM: `dtmcs` and `dmi` data registers (TCK domain) |
| [hdl/debug/WardRV_dbg_dmi_cdc.vhd](hdl/debug/WardRV_dbg_dmi_cdc.vhd) | `WardRV_dbg_dmi_cdc` | entity | TCK <-> clk 4-phase handshake of the DMI (`sync2dffrn`) |
| [hdl/debug/WardRV_dbg_jtag.vhd](hdl/debug/WardRV_dbg_jtag.vhd) | `WardRV_dbg_jtag` | entity | TAP + DTM + CDC: JTAG pins in, DMI record out |
| [hdl/debug/WardRV_dbg_dm.vhd](hdl/debug/WardRV_dbg_dm.vhd) | `WardRV_dbg_dm` | entity | Debug Module (clk domain): DM registers, abstract commands, hart status |
| [hdl/debug/WardRV_dbg_dm_mem.vhd](hdl/debug/WardRV_dbg_dm_mem.vhd) | `WardRV_dbg_dm_mem` | entity | Debug memory seen by the core (ROM, flags, WHERETO, abstract program, program buffer, data) |
| [hdl/debug/WardRV_dbg_sba.vhd](hdl/debug/WardRV_dbg_sba.vhd) | `WardRV_dbg_sba` | entity | System Bus Access (`sbcs`, `sbaddress0`, `sbdata0`) through `dmem2sbi` |
| [hdl/debug/WardRV_debug.vhd](hdl/debug/WardRV_debug.vhd) | `WardRV_debug` | entity | Convenience top: `WardRV_dbg_jtag` + `WardRV_dbg_dm` |

`hdl/fsm/README.md` is an empty file.

### WardRV_fsm

| Kind | Name | Type | Default / Description |
|------|------|------|-----------------------|
| generic | `HARTID` | std_logic_vector(31 downto 0) | `(others => '0')`, `mhartid` value |
| generic | `RESET_ADDR` | std_logic_vector(31 downto 0) | `(others => '0')`, reset PC |
| generic | `VERBOSE` | boolean | `true`, simulation trace in `exec_fsm.log` (not forwarded by `sbi_WardRV_fsm`, so the default applies) |
| generic | `DEBUG_ENABLE` | boolean | `false`, debug mode (all the debug logic is removed when false) |
| generic | `NB_TRIGGERS` | natural | `0`, mcontrol6 triggers |
| generic | `EBREAK_EXCEPTION` | boolean | `false`, EBREAK raises a breakpoint exception |
| generic | `RESETHALTREQ_ENABLE` | boolean | `false`, halt-on-reset request |
| in | `clk_i` | std_logic | Clock |
| in | `arst_b_i` | std_logic | Asynchronous reset, active low |
| out | `imem_ini_o` | imem_ini_t | Instruction request (`valid`, `addr` = PC) |
| in | `imem_tgt_i` | imem_tgt_t | Instruction response (`ready`, `inst`) |
| out | `dmem_ini_o` | dmem_ini_t | Data request (`valid`, `addr`, `wdata`, `we`, `be`) |
| in | `dmem_tgt_i` | dmem_tgt_t | Data response (`ready`, `rdata`, `err` unused) |
| in | `meip_i` | std_logic | Machine external interrupt pending |
| in | `dbg_hart_req_i` | dbg_hart_req_t | Debug Module requests (`haltreq`, `resethaltreq`), default inactive |
| out | `dbg_sbi_ini_o` | dbg_sbi_ini_t | Debug memory bus (debug mode only) |
| in | `dbg_sbi_tgt_i` | dbg_sbi_tgt_t | Debug memory response, default inactive |

### WardRV_iss

Same interfaces as `WardRV_fsm` without `meip_i`; generics `RESET_ADDR`, `HARTID` and `VERBOSE` (default `false`, trace in `exec_iss.log`). Behavioural model built on the protected type `iss_t` (shared variable): FETCH waits for `imem_tgt_i.ready`, EXECUTE waits for `dmem_tgt_i.ready` when the instruction accesses memory. CSR instructions only return `mhartid`; FENCE, ECALL, EBREAK and MRET are executed as NOPs.

### dmem2sbi / imem2sbi

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `clk_i` | in | std_logic | Clock |
| `arst_b_i` | in | std_logic | Asynchronous reset, active low |
| `dmem_ini_i` / `imem_ini_i` | in | dmem_ini_t / imem_ini_t | Request from the core |
| `dmem_tgt_o` / `imem_tgt_o` | out | dmem_tgt_t / imem_tgt_t | Response, `ready` pulsed one cycle when all bytes are done (`err = '0'`) |
| `sbi_ini_o` | out | sbi_ini_t | SBI byte access; address = `addr(SBI_ADDR_WIDTH-1 downto 2) & byte index` (dmem) or word address + byte counter (imem) |
| `sbi_tgt_i` | in | sbi_tgt_t | SBI response |

`dmem2sbi` (IDLE / TRANSFER / DONE) skips disabled bytes and shifts the write data register by one byte per lane; read bytes are accumulated in the same register. `imem2sbi` always performs four reads.

### WardRV_fsm sub-modules

| Entity | Ports | Function |
|--------|-------|----------|
| `WardRV_fsm_fetch` | `clk_i`, `arst_b_i`, `imem_valid_i`, `pc_i`, `imem_ready_o`, `inst_r_o`, `imem_ini_o`, `imem_tgt_i` | `imem.valid` = registered request, `imem.addr` = PC, instruction registered on `valid and ready` |
| `WardRV_fsm_decode` | `inst_i`; `imm_i_o`, `imm_s_o`, `imm_b_o`, `imm_u_o`, `imm_j_o`, `imm_csr_o`; `rd_addr_o`, `rs1_addr_o`, `rs2_addr_o`, `rd_src_o`, `rd_we_o`, `rs1_re_o`, `rs2_re_o`; `alu_op_o`, `alu_src_a_sel_o`, `alu_src_b_sel_o`; `mem_req_o`, `mem_we_o`, `mem_be_o`, `mem_data_unsigned_o`; `is_branch_o`, `branch_use_flag_zero_o`, `branch_use_flag_carry_o`, `branch_use_flag_sign_o`, `branch_flag_is_set_o`, `pc_sel_o`; `csr_we_o`, `csr_re_o`, `csr_addr_o`; `inst_is_mret_o`, `inst_is_ebreak_o`, `inst_is_dret_o`, `inst_type_o` | Immediates, register addresses and control signals; `rd_we = 0` when `rd = x0` |
| `WardRV_fsm_alu` | `src_a_i`, `src_b_i`, `op_i` (`alu_op_t`), `res_o`, `zero_o`, `sign_o`, `carry_o` | ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND, CLR (`src_b and not src_a`, CSRRC / CSRRCI); carry = carry / borrow of ADD / SUB (unsigned less-than after SUB); sign = `res(31) xor overflow` after SUB (signed less-than, correct on signed overflow), `res(31)` otherwise |
| `WardRV_fsm_regfile` | `clk_i`, `arst_b_i`, `rs1_re_i`, `rs1_addr_i`, `rs1_rdata_o`, `rs2_re_i`, `rs2_addr_i`, `rs2_rdata_o`, `rd_addr_i`, `rd_wdata_i`, `rd_we_i` | `ram_2r1w` (WIDTH 32, DEPTH 32, asynchronous read); reads of `x0` return 0 |
| `WardRV_fsm_memory` | `clk_i`, `arst_b_i`, `dmem_valid_i`, `addr_i`, `wdata_i`, `we_i`, `be_i`, `data_unsigned_i`, `dmem_ready_o`, `dmem_rdata_r_o`, `dmem_ini_o`, `dmem_tgt_i` | Byte enables and write data shifted by `addr(1:0)`, read data shifted back, sign / zero extended and registered |
| `WardRV_fsm_csr` | `clk_i`, `arst_b_i`, `csr_addr_i`, `csr_we_i`, `csr_re_i`, `csr_wdata_i`, `csr_rdata_o`, `csr_mtvec_o`, `trap_i`, `trap_cause_i`, `trap_pc_i`, `trap_mtval_i`, `inst_is_mret_i`, `meip_i`, `trap_mirq_o` (generic `HARTID`) | CSR read mux / write, trap entry (`MPIE <= MIE`, `MIE <= 0`, `mepc`, `mcause`, `mtval`), MRET (`MIE <= MPIE`, `MPIE <= 1`, applied once in WRITEBACK), `trap_mirq = MEIP and MEIE and MIE` |

### WardRV_debug

Convenience top of the debug unit, see [Debug Unit](#debug-unit).

## Debug Unit

RISC-V Debug Specification 1.0 (ratified), usable with OpenOCD / GDB. Everything is optional: with `DEBUG_ENABLE = false` (default) the core is unchanged.

### Architecture

```
            TCK domain                        |              clk domain
 JTAG   +--------------+ user DR +------------+  dmi_req_t  +------------------+
 pins ->| WardRV_dbg_  |<------->| WardRV_dbg_|-----------> | WardRV_dbg_dm    |--> ndmreset_o
        | tap (1149.1) |         | dtm + dmi_ |<----------- | DM registers     |--> sba_sbi_ini_o
        +--------------+         | cdc        |  dmi_rsp_t  | debug memory     |    (SBA_ENABLE)
        WardRV_dbg_jtag          +------------+             +------------------+
                                              |  dbg_hart_req_t |  ^ dbg_sbi (32 bits, 0xFFFF_F800)
                                              |                 v  |
                                              |             +------------------+
                                              |             | WardRV_fsm       |
                                              |             | WardRV_fsm_debug |
                                              |             | WardRV_fsm_trigger
                                              |             +------------------+
```

- `WardRV_dbg_tap`: 16-state TAP FSM (transitions on the rising edge of TCK, Test-Logic-Reset after 5 TMS = 1), asynchronous TRST* (`TRST_ENABLE`) combined with the power-on reset, dedicated IR shift register (Capture-IR = `00001`, IR updated on the falling edge of TCK in Update-IR, reset value IDCODE), DR capture / shift on the rising edge, update on the falling edge, TDO and `tdo_en` retimed on the falling edge (`tdo_en` only in Shift-IR / Shift-DR). No boundary scan: IR `00001` IDCODE, `10000` DTMCS, `10001` DMI, all-ones and any other opcode BYPASS. Static design (TCK can be stopped).
- `WardRV_dbg_dtm`: `dtmcs` (version 1, abits 7, sticky `dmistat`, `idle = DTM_IDLE_CYCLES`, `dmireset`, `dmihardreset`) and `dmi` (41 bits). A request is launched in Update-DR only for op 1 / 2, when the sticky status is 0 and the DTM is not busy; otherwise the status becomes busy (3) and the request is ignored. Capture-DR returns the address of the last request, the data of the last response and the sticky status. `dmihardreset` clears the sticky status and the `dmi` address / data and ignores the result of the transaction in progress; the CDC handshake is not broken, so the DTM stays busy until it ends (the DM always answers while clk runs).
- `WardRV_dbg_dmi_cdc`: 4-phase req / ack handshake through `sync2dffrn`, payloads stable while req / ack are high. Reusable by another DTM. The handshake only progresses while TCK runs: the debugger stays `dtmcs.idle` cycles in Run-Test/Idle.
- DMI records (`WardRV_debug_pkg`): `dmi_req_t` (`valid`, `op`, `addr(6:0)`, `data`) and `dmi_rsp_t` (`ready`, `valid`, `op` 0 success / 2 failed, `data`), one transaction in flight.
- `WardRV_dbg_dm`: DM registers `data0..`, `dmcontrol`, `dmstatus` (version 3, authenticated), `hartinfo`, `abstractcs`, `command` (Access Register: GPR `0x1000-0x101F`, CSR `0x0000-0x0FFF`, `transfer`, `write`, `postexec`, `aarpostincrement`, `aarsize <= 2`), `abstractauto`, `nextdm`, `progbuf0..`, `sbcs` / `sbaddress0` / `sbdata0` (`SBA_ENABLE`), `haltsum0`. The DMI response is given in the request cycle. `dmactive = 0` keeps the DM in reset; the DM is never reset by `ndmreset`. `cmderr`: 1 (busy) for an access to `command` / `abstractcs` / `abstractauto` / `data` / `progbuf` during a command, reported when the command completes; 2 for another `cmdtype`, `aarsize > 2` or `postexec` without program buffer; 3 for a `regno` that does not exist (outside the GPR / CSR ranges) or an exception; 4 if the hart is not halted. CSRs that the core does not implement read as 0 without error (the core has no illegal instruction exception). `haltreq`, `resumereq`, `ackhavereset` and `set/clrresethaltreq` writes are ignored while a command is executing.
- Core (`WardRV_fsm_debug`): debug mode entry at the end of WRITEBACK (or in S_RESET / S_TRAP), priorities trigger (cause 2) > ebreak (1) > resethaltreq (5) > haltreq (3) > step (4) (the spec allows haltreq / resethaltreq at another position as long as resethaltreq > haltreq and trigger > ebreak > step); `dpc` = address of the instruction for ebreak and triggers (the instruction has no effect), next PC otherwise. In debug mode: interrupts masked, triggers disabled, EBREAK returns to the halt entry, DRET returns to `dpc`. `dcsr.step` executes one instruction then halts (interrupts masked during the step when `stepie = 0`).
- Triggers (`WardRV_fsm_trigger`, `NB_TRIGGERS`): mcontrol6, match `=`, `>=`, `<` on the address of the executed instruction or of a load / store, timing before, M-mode, action 1 (debug mode) only with `dmode = 1`, action 0 (breakpoint exception) only with `EBREAK_EXCEPTION`; `dmode` writable in debug mode only. `tdata1` never holds `dmode = 0` with action 1: action is 0 at reset and for a `dmode = 0` write (without `EBREAK_EXCEPTION`, `m` is then forced to 0 and the trigger cannot fire).

### Generics

| Block | Generic | Default | Description |
|-------|---------|---------|-------------|
| `WardRV_dbg_jtag` / `WardRV_debug` | `IDCODE_VALUE` | `x"10000001"` | IDCODE (bit 0 = 1, manufacturer 0x7F forbidden: checked by an assertion) |
| | `TRST_ENABLE` | `true` | Use TRST* (`jtag_ini_i.trst_n`) |
| | `DTM_IDLE_CYCLES` | `5` | `dtmcs.idle` |
| `WardRV_dbg_dm` / `WardRV_debug` | `PROGBUF_SIZE` | `2` | Program buffer words (0..16, 16 needs `IMPEBREAK = false`) |
| | `DATA_COUNT` | `2` | Data registers (1..12) |
| | `IMPEBREAK` | `true` | Implicit ebreak after the program buffer |
| | `SBA_ENABLE` | `false` | System Bus Access |
| | `SBA_ADDR_WIDTH` | `8` | SoC SBI address width (`sbcs.sbasize`) |
| | `RESETHALTREQ_ENABLE` | `false` | `setresethaltreq` / `clrresethaltreq` |
| | `HARTID` | `0` | Reserved (one hart) |
| `WardRV_fsm` / `sbi_WardRV_fsm` | `DEBUG_ENABLE` | `false` | Debug mode |
| | `NB_TRIGGERS` | `0` | Triggers |
| | `EBREAK_EXCEPTION` | `false` | Breakpoint exception |
| | `RESETHALTREQ_ENABLE` | `false` | Halt on reset |

### Debug memory

2 KB window at `0xFFFF_F800`, mapped on `dbg_sbi` only in debug mode (no SoC address space is used). Every word is reachable with a x0-relative load / store / jalr (12-bit immediate between -2048 and -1). 32-bit SBI target without wait state (`ready = cs`); the core writes only full words.

| Offset | Content | Core access |
|--------|---------|-------------|
| `0x000` | ROM: halt entry | R |
| `0x008` | ROM: exception entry | R |
| `0x100` / `0x104` / `0x108` / `0x10C` | HALTED / GOING / RESUMING / EXCEPTION | W |
| `0x200` | WHERETO: `jal` to ABSTRACT | R |
| `0x300` | ABSTRACT[0..7], generated by the DM | R |
| `0x340` | PROGBUF[0..PROGBUF_SIZE-1], then `ebreak` (impebreak) | R |
| `0x380` | DATA[0..DATA_COUNT-1] | R/W |
| `0x400` | FLAGS (bit 0 go, bit 1 resume) | R |

Debug ROM ([esw/debug_rom/debug_rom.S](esw/debug_rom/debug_rom.S)): save s0 in dscratch0, write the hartid to HALTED, poll FLAGS; go: write GOING, restore s0, jump to WHERETO; resume: write RESUMING, restore s0, `dret`; exception: write EXCEPTION, `ebreak`. `make` in `esw/debug_rom` regenerates `hdl/debug/WardRV_dbg_rom_pkg.vhd` with the self-contained assembler `gen_rom.py` (no toolchain); `make check` compares it with GNU as. Abstract commands: GPR `sw` / `lw xN, data0(zero)`, CSR through s0 saved in dscratch1, then `jal` PROGBUF (postexec) or `ebreak`. dscratch0 / dscratch1 are therefore not accessible with abstract commands; they are free during the program buffer execution (`hartinfo.nscratch = 2`).

### Integration notes

- JTAG: the pull-ups on TMS, TDI and TRST* belong to the pads. TDO is valid only when `tdo_en = 1` (tri-state at the pad).
- `arst_b_i` of `WardRV_debug` is the power-on reset of the debug unit, never `ndmreset_o`. `ndmreset_o` goes to the SoC reset controller (it resets the core and the peripherals, not the debug unit); `hart_rst_b_i` is the reset of the core (`havereset`, `unavail`).
- Connect `dbg_hart_req_o`, `dbg_sbi_ini_i` and `dbg_sbi_tgt_o` to `sbi_WardRV_fsm` / `WardRV_fsm` synthesized with `DEBUG_ENABLE = true`; the core and the DM share `clk_i`.
- TCK and clk are asynchronous: the debugger must use `dtmcs.idle` Run-Test/Idle cycles (OpenOCD does) and retry on busy. The CDC payload paths are quasi-static (max delay / false path constraints).
- `sba_sbi_ini_o` / `sba_sbi_tgt_i` (`SBA_ENABLE`): 8-bit SBI initiator, to connect as an extra master of the SoC interconnect.
- TRST* and the power-on reset are released asynchronously in the TCK domain: keep TMS = 1 (pull-up) while they are released. Avoid asserting TRST* alone during a DMI transaction (the clk side of the CDC is not reset by TRST*).

## Register Map

This IP has no software-visible registers.

Programmer-visible state: 32 general-purpose registers `x0`..`x31` (`x0` = 0), the PC (reset value `RESET_ADDR`) and the machine CSRs below (all other CSR addresses read as 0 and ignore writes).

| CSR | Address | Reset | Notes |
|-----|---------|-------|-------|
| `mstatus` | `0x300` | `0x00001800` | MPP = 11; MIE (bit 3) / MPIE (bit 7) updated on trap and MRET |
| `mie` | `0x304` | `0` | MEIE (bit 11) enables the external interrupt |
| `mtvec` | `0x305` | `0` | Trap address (direct mode) |
| `mscratch` | `0x340` | `0` | Scratch |
| `mepc` | `0x341` | `0` | Return address (MRET), bits 1..0 read-only zero |
| `mcause` | `0x342` | `0` | `0x8000000B` on external interrupt |
| `mtval` | `0x343` | `0` | Written with 0 on trap |
| `mip` | `0x344` | `0` | MEIP (bit 11) = `meip_i` (registered), read-only |
| `mhartid` | `0xF14` | `HARTID` | Read-only |

Debug CSRs (`DEBUG_ENABLE`, accessible in debug mode only, otherwise read as 0):

| CSR | Address | Reset | Notes |
|-----|---------|-------|-------|
| `dcsr` | `0x7B0` | `0x40000003` | debugver = 4, `ebreakm` (15), `stepie` (11), `cause` (8:6, read-only), `step` (2), prv = 3 |
| `dpc` | `0x7B1` | `0` | PC of the debug mode entry, DRET target |
| `dscratch0` / `dscratch1` | `0x7B2` / `0x7B3` | `0` | Used by the debug ROM / abstract commands |

Trigger CSRs (`NB_TRIGGERS > 0`): `tselect` (`0x7A0`, WARL `0..NB_TRIGGERS-1`), `tdata1` (`0x7A1`, mcontrol6), `tdata2` (`0x7A2`, address), `tdata3` (`0x7A3`, 0), `tinfo` (`0x7A4`, `0x01000040`).

With `EBREAK_EXCEPTION`, EBREAK (when not handled by the debug mode) traps with `mcause = 3`, `mepc` = address of the EBREAK, `mtval = 0`.

## Verification

### Testbenches

| File | DUT | Description |
|------|-----|-------------|
| [sim/tb_WardRV.vhd](sim/tb_WardRV.vhd) + [sim/tb_WardRV_pkg.vhd](sim/tb_WardRV_pkg.vhd) | `WardRV_fsm` (`MODEL = "FSM"`, default) or `WardRV_iss` (`MODEL = "ISS"`) | UVVM testbench, answers `imem` / `dmem` requests in one cycle (`HARTID = 0x900DC0DE`). `TEST_ENV = "ACT3"` (default): loads `FIRMWARE_FILE` (hex) in a memory at `0x80000000` (`RESET_ADDR`), ends when the program writes `tohost` (`0x80200000`): `1` = TEST PASSED, other = TEST FAILED. When `SIGNATURE_FILE` and `GOLDEN_FILE` are set (ACT3 compliance targets), dumps the signature area (`0x80202104`..`0x80203000`) to `SIGNATURE_FILE` and compares it word by word with `GOLDEN_FILE` (`TB_ERROR` on the first mismatch). Timeout 500 us at 10 ns. `TEST_ENV = "ACT4"`: memory of 512 KB at `0x00004000`, the bytes written at `0x10000000` are printed (console), ends when the program writes `0x20000000`: `123456789` = TEST PASSED, other = TEST FAILED. Timeout 10 ms |
| [sim/tb_WardRV_iss.vhd](sim/tb_WardRV_iss.vhd) | `iss_t` (procedural) | Same memory model driving the ISS protected type directly from a process (no target uses it as toplevel) |
| [sim/tb_WardRV_dbg_jtag.vhd](sim/tb_WardRV_dbg_jtag.vhd) | `WardRV_dbg_jtag` | JTAG VIP + fake DM, clk 10x faster and 10x slower than TCK: Test-Logic-Reset (TRST*, 5 TMS = 1), Capture-IR, IDCODE, BYPASS and undecoded opcodes, TDO / tdo_en / IR on the falling edge, dtmcs, DMI read / write, busy, failed, dmireset, dmihardreset |
| [sim/tb_WardRV_dbg_dm.vhd](sim/tb_WardRV_dbg_dm.vhd) | `WardRV_dbg_dm` | DMI record + behavioural hart model on the debug memory + `sbi_ram` on the SBA: dmcontrol / dmstatus, dmactive, ndmreset, halt / resume, abstract commands, cmderr (busy, not supported, exception, halt / resume), abstractauto, postincrement, resethaltreq, SBA (8 / 16 / 32 bits, autoincrement, readonaddr, readondata, sberror, sbbusyerror) |
| [sim/tb_WardRV_debug.vhd](sim/tb_WardRV_debug.vhd) | `WardRV_fsm` + `WardRV_debug` | Full system driven through JTAG like a debugger (firmware `esw/debug/debug_fw.hex`): halt, dcsr / dpc, GPR / CSR access, cmderr, abstractauto, memory through the program buffer, single step, software breakpoint, execute / store / load triggers, resume, resethaltreq with ndmreset |
| [sim/tb_WardRV_openocd.vhd](sim/tb_WardRV_openocd.vhd) | `WardRV_fsm` + `WardRV_debug` (SBA) | Co-simulation with OpenOCD and GDB (target `cosim_openocd`): remote_bitbang server called every `TCK_DIV` clock cycles (VHPIDIRECT `rbb_tick`), RAM of 64 KB at `0x80000000` loaded with `esw/openocd/fw.hex` and shared by the core and the 8-bit SBA, `ndmreset_o` resets the core, end when OpenOCD quits, watchdog `TIMEOUT_US` (250 ms of simulated time), see [OpenOCD + GDB co-simulation](#openocd--gdb-co-simulation) |
| [sim/WardRV_vips.vhd](sim/WardRV_vips.vhd) | - | VIP package: reset pulse, JTAG procedures (init, clock, idle, reset, shift LSB first, IR / DR scans, IDCODE, dtmcs, DMI scan with status, DMI access with busy retry and dmireset, DMI read / write), TCK period as parameter |
| sim/save/tb_WardRV.vhd | - | Older copy of `tb_WardRV` (`SIGNATURE_FILE := "signature.output"`), not in the core |

Software:

- [esw/testcase/](esw/testcase/): `start.S` + `main.c` (RV32I arithmetic, shifts, comparisons, immediates, loads / stores of every width, `check()` writing `tohost`), `link.ld` (128 KB at `0x80000000`), `Makefile` (`riscv64-unknown-elf-gcc`, `-march=rv32i_zicsr -mabi=ilp32`); the prebuilt `firmware.hex` / `firmware.lst` are committed.
- [esw/openocd/](esw/openocd/): firmware of `cosim_openocd` (`start.S`, `main.c`, `link.ld`: 64 KB at `0x80000000`, `-O0 -g`): loop calling `bp_func` and writing `watched`, `hb_func`, `step_test` (straight-line code and a jump), `next_test`, sum of `pattern[0 .. limit-1]`, `end_marker`; `fw.elf` (loaded by GDB), `fw.hex`, `fw.lst` are committed (rebuilt with `make -C esw/openocd`, RISC-V toolchain).
- [esw/compliance_act3/](esw/compliance_act3/): ACT3 flow (riscv-arch-test 3.10.0, `rv32i_m/I`, `-march=rv32i`). `make full` downloads the pinned tools in `riscv-compliance-ws/` (xPack GCC 13.3.0-1 / binutils 2.42, Sail 0.13.1), builds the 39 tests into `.hex` / `.lst`, runs them on Sail (RV32I, [config/wardrv/sail_rv32i.json](esw/compliance_act3/config/wardrv/sail_rv32i.json)) to get the reference `.signature`, and compares everything with the committed `benchs/` (byte identical). DUT configuration in [config/wardrv/](esw/compliance_act3/config/wardrv/) (`link.ld`, `model_test.h`). `docker/` builds an Ubuntu 24.04 image containing all the tools, so that the generation does not depend on the host distribution nor on the network: `make build` (network needed once), `make generate` (runs `make full` without network), `make save` / `make load` (keep the image in a `.tar.gz`); `ENGINE=docker` (default) or `ENGINE=podman` (rootless podman).
- [esw/compliance_act4/](esw/compliance_act4/): ACT4 flow (riscv-arch-test 4.1.0, extensions `I,Zicsr`). `make full` downloads the pinned tools in `riscv-compliance-ws/` (xPack GCC 15.2.0-1 / binutils 2.45, Sail 0.13.1, mise for uv / Python / Ruby), generates the 45 self-checking ELFs, converts them into `.hex` / `.lst` (debug information removed) and compares with the committed `benchs/`. DUT configuration in [config/wardrv/](esw/compliance_act4/config/wardrv/) (UDB `wardrv.yaml`: I 2.1, Zicsr 2.0, Sm 1.13.0, MXLEN 32; `sail.json`, `test_config.yaml`, `link.ld`, `rvmodel_macros.h`). `docker/` builds an Ubuntu 24.04 image containing all the tools, so that the generation does not depend on the host distribution nor on the network: `make build` (network needed once), `make generate` (runs `make full` without network), `make save` / `make load` (keep the image in a `.tar.gz`); `ENGINE=docker` (default) or `ENGINE=podman` (rootless podman).

### Targets

| Target | Toplevel | Description |
|--------|----------|-------------|
| `default` | `sbi_WardRV_fsm` | HDL fileset only |
| `sim` | `tb_WardRV` | Base of the simulation targets, "DON'T RUN" (GHDL `-Wall -fsynopsys -frelaxed --no-vital-checks`, `--ieee-asserts=disable`) |
| `lint_nxmap` | `sbi_WardRV_fsm` | NanoXplore nxmap on NG-MEDIUM (`program: False`, flag `TARGET = NANOXPLORE_NG_MEDIUM`) |
| `lint_ghdl` | `asylum.WardRV_wrapper` | Lint: GHDL `-Wall` analysis and elaboration (empty simulation) of the core + debug unit (`-Wall -fsynopsys -frelaxed --no-vital-checks`) |
| `lint_ghdl_synth_nodebug` | `asylum.WardRV_wrapper` | Synthesis check (see below), without debug unit: `DEBUG_ENABLE=false` |
| `lint_ghdl_synth_debug` | `asylum.WardRV_wrapper` | Synthesis check, debug unit without trigger: `DEBUG_ENABLE=true`, `NB_TRIGGERS=0` |
| `lint_ghdl_synth_debug_trig2` | `asylum.WardRV_wrapper` | Synthesis check, debug unit with 2 triggers: `DEBUG_ENABLE=true`, `NB_TRIGGERS=2` |
| `lint_ghdl_synth_debug_sba` | `asylum.WardRV_wrapper` | Synthesis check, debug unit with system bus access: `DEBUG_ENABLE=true`, `NB_TRIGGERS=0`, `SBA_ENABLE=true` |
| `lint_ghdl_synth_debug_full` | `asylum.WardRV_wrapper` | Synthesis check, debug unit, every option: `DEBUG_ENABLE=true`, `NB_TRIGGERS=2`, `SBA_ENABLE=true`, `EBREAK_EXCEPTION=true`, `RESETHALTREQ_ENABLE=true` |
| `lint_ghdl_synth_debug_large` | `asylum.WardRV_wrapper` | Synthesis check, debug unit, 4 triggers, large program buffer: `DEBUG_ENABLE=true`, `NB_TRIGGERS=4`, `PROGBUF_SIZE=8`, `DATA_COUNT=12` |
| `sim_basic` | `tb_WardRV` | `esw/testcase` firmware (`FIRMWARE_FILE=firmware.hex`, `VERBOSE=false`) |
| `sim_debug_tap` | `tb_WardRV_dbg_jtag` | Debug unit: JTAG TAP + DTM + CDC |
| `sim_debug_dm` | `tb_WardRV_dbg_dm` | Debug unit: Debug Module (with SBA) |
| `sim_debug_system` | `tb_WardRV_debug` | Debug unit: system test through JTAG (`FIRMWARE_FILE=debug/debug_fw.hex`) |
| `cosim_openocd` | `tb_WardRV_openocd` | Debug unit: OpenOCD + GDB co-simulation (`FIRMWARE_FILE=openocd/fw.hex`, hooks `pre_build` / `pre_run` / `post_run`), not in `nonreg_sim` nor in the CI, see [OpenOCD + GDB co-simulation](#openocd--gdb-co-simulation) |
| `sim_compliance_act4_<test>` (45 targets) | `tb_WardRV` | `FIRMWARE_FILE=act4/<test>.hex`, `TEST_ENV=ACT4`, `VERBOSE=false`; `<test>` = `i_<instruction>_00` (39 tests: the RV32I instructions, `fence` and `nop`) and `zicsr_<instruction>_00` (`csrrc`, `csrrci`, `csrrs`, `csrrsi`, `csrrw`, `csrrwi`) |
| `sim_compliance_act3_<test>` (39 targets) | `tb_WardRV` | `FIRMWARE_FILE=act3/<test>.hex`, `GOLDEN_FILE=act3/<test>.signature`, `SIGNATURE_FILE=signature.output`, `TEST_ENV=ACT3`, `VERBOSE=false`; `<test>` = `add_01`, `addi_01`, `and_01`, `andi_01`, `auipc_01`, `beq_01`, `bge_01`, `bgeu_01`, `blt_01`, `bltu_01`, `bne_01`, `fence_01`, `jal_01`, `jalr_01`, `lb_align_01`, `lbu_align_01`, `lh_align_01`, `lhu_align_01`, `lui_01`, `lw_align_01`, `misalign1_jalr_01`, `or_01`, `ori_01`, `sb_align_01`, `sh_align_01`, `sll_01`, `slli_01`, `slt_01`, `slti_01`, `sltiu_01`, `sltu_01`, `sra_01`, `srai_01`, `srl_01`, `srli_01`, `sub_01`, `sw_align_01`, `xor_01`, `xori_01` |

A `lint_ghdl_synth_*` target is `lint_ghdl` with the generics of its configuration (FuseSoC `parameters`) and a `pre_run` hook, `ghdl_synth.sh` of `asylum:utils:generators` (>= 2.8.0, `tools/ghdl/`), which adds `include ghdl_synth.mk` to the Makefile generated by edalize. The run step (`make run EXTRA_OPTIONS=<generics>`) then runs, in the build directory:

1. `ghdl --synth` of the toplevel (rule `ghdl_synth` of `ghdl_synth.mk`), with the options of the target: `STD`, `ANALYZE_OPTIONS` (`-Wall -fsynopsys -frelaxed --no-vital-checks`), `TOPLEVEL_LIBS`, `TOPLEVEL` and the generics. It writes `WardRV_wrapper_synth.vhd` (netlist) and `WardRV_wrapper_synth.log`, prints the warnings, and fails on a synthesis error (an inferred latch is one);
2. `ghdl -r --no-run`: elaboration with the same generics, without simulation.

To add a configuration, add a target with its `parameters` (the generics are declared in the `parameters` section of `WardRV.core`).

The core parameters are `FIRMWARE_FILE`, `GOLDEN_FILE`, `SIGNATURE_FILE`, `TEST_ENV`, `VERBOSE` and `EBREAK_EXCEPTION` (generics). `tb_WardRV` also has the generics `DEBUG_ENABLE` and `NB_TRIGGERS` (core options without Debug Module) for non-regression runs with the debug logic enabled. The ACT3 compliance targets set `SIGNATURE_FILE`, so a test passes only if it writes `tohost = 1` and its signature matches the reference (the `riscv-arch-test` programs always write `tohost = 1`; the result comes from the signature). The ACT4 tests check their results themselves and write PASS / FAIL at `0x20000000`. Each compliance target has its own fileset (`files_act3_<test>` / `files_act4_<test>`) so only its `.hex` (and `.signature`) is copied in the build directory.

### How to Run

The default tool is GHDL (`mk/defs.mk`: `TOOL ?= ghdl`, `TARGET ?= sim_basic`).

```bash
make help                          # variables, rules and target list (mk/targets.txt)
make sim_basic                     # run one target (log in log/)
make nonreg_sim                    # run every sim_* target
make TARGETS_FILTER=^sim_compliance_act4 nonreg_sim   # run a filtered subset
make lint_ghdl                     # GHDL -Wall lint
make lint_ghdl_synth_nodebug       # GHDL -Wall lint + ghdl --synth -Wall check of one configuration
make nonreg_lint                   # run lint_ghdl, lint_ghdl_synth_* and lint_nxmap
make clean                         # remove build/ and log/
```

Equivalent FuseSoC command:

```bash
fusesoc --cores-root . run --build-root build --target sim_compliance_act3_add_01 asylum:processor:WardRV:0.0.5
```

The firmware and reference files are committed, so simulation only needs GHDL and UVVM (`bitvis:verification:uvvm`). Rebuilding them needs a RISC-V toolchain (`RISCV_PREFIX ?= riscv64-unknown-elf-`, see `esw/testcase/Makefile`); the compliance tests are regenerated with `make full` in `esw/compliance_act3` and `esw/compliance_act4`, which install their own pinned tools. Outside CI, GHDL writes a waveform `dut.fst`.

### OpenOCD + GDB co-simulation

The target `cosim_openocd` validates the debug unit with a real debugger: GDB → OpenOCD (riscv target, `remote_bitbang` adapter) → TCP → `tb_WardRV_openocd` (GHDL, VHPIDIRECT). It is not a `sim_` target, so it is neither in `nonreg_sim` nor in the CI: it needs `openocd` (riscv target, `remote_bitbang`) and `gdb-multiarch` (with Python) in the `PATH` (validated with the Ubuntu 24.04 packages `openocd 0.12.0-1build2` and `gdb-multiarch 15.0.50.20240403-0ubuntu1`).

```bash
make cosim_openocd                 # co-simulation
make -C esw/openocd                # rebuild the firmware (RISC-V toolchain)
```

- Hooks of the target ([sim/openocd/](sim/openocd/), copied in the work root): `pre_build` builds `librbb.so` ([remote_bitbang.c](sim/openocd/remote_bitbang.c): TCP server on `RBB_PORT`, default 9823, `rbb_tick` called by the testbench every `TCK_DIV` clock cycles, one pin write per call, declared in [rbb_pkg.vhd](sim/openocd/rbb_pkg.vhd)); `pre_run` starts [launch.sh](sim/openocd/launch.sh) in the background: it waits for the port, starts OpenOCD ([wardrv_sim.cfg](sim/openocd/wardrv_sim.cfg), `openocd.log`) and GDB ([scenario.py](sim/openocd/scenario.py), `gdb.log`), writes `scenario.result`, then sends `shutdown` to OpenOCD, which sends `Q`: the simulation ends; `post_run` ([check.sh](sim/openocd/check.sh)) prints the logs and fails unless `scenario.result` is `PASS`.
- Scenario (firmware [esw/openocd/](esw/openocd/), each check printed `ok` / `ERROR`, each step `PASS` / `FAIL`):
  1. connection: IDCODE `0x1DEB0001`, RISC-V core examined, `dmstatus.version = 3`, `abstractcs`, `sbcs`;
  2. `monitor reset halt` (ndmreset + haltreq): pc = `RESET_ADDR`;
  3. image overwritten then `load` + `compare-sections`, through the program buffer, then through the System Bus Access (`riscv set_mem_access sysbus`), cross-check;
  4. GPR, `mhartid`, `mscratch`, `dcsr` (debugver, prv, stepie, read-only cause), `dpc` / pc;
  5. `break` (ebreak, `dcsr.cause = 1`), argument and globals;
  6. `watch` (store trigger: value after the store) and `hbreak` (execute trigger, `dcsr.cause = 2`);
  7. `stepi` (pc and t0 after each instruction, a jump), `monitor step` (`dcsr.step`, `dcsr.cause = 4`), `next` over calls;
  8. `x/` and `set var` (8, 16 and 32-bit writes), read back through the program buffer and the SBA;
  9. `continue` to `end_marker`: the result depends on the `set var` of step 8.
- A run takes about 2.5 minutes (about 34 ms of simulated time, TCK = clk / 2 with `TCK_DIV = 1`, 1.6 million TCK cycles). GDB 15 single-steps RISC-V with a breakpoint on the next instruction (OpenOCD 0.12 does not report `vContSupported`), so the hardware step (`dcsr.step`) is checked with `monitor step`.

## Synthesis

- `lint_nxmap` runs NanoXplore nxmap for the NG-MEDIUM FPGA with the synthesizable `sbi_WardRV_fsm` as toplevel (the `files_hdl` fileset still contains the behavioural ISS and the simulation-only statistics package, which are not instantiated by the FSM core).
- `lint_ghdl` analyzes (`-Wall`) and elaborates the same top (empty simulation).
- `lint_ghdl_synth_*` run `ghdl --synth -Wall` (GHDL 6.0.0, no vendor tool) on [syn/WardRV_wrapper.vhd](syn/WardRV_wrapper.vhd): `sbi_WardRV_fsm` (default `IADDR_*` generics) + `WardRV_debug` connected as in a SoC, with flat constrained ports (`sbi_WardRV_fsm` has unconstrained SBI ports and cannot be a synthesis top), one target per configuration (no debug; debug without trigger; 2 triggers; SBA; 2 triggers + SBA + `EBREAK_EXCEPTION` + `RESETHALTREQ_ENABLE`; 4 triggers, `PROGBUF_SIZE=8`, `DATA_COUNT=12`, see [Targets](#targets)). The run step synthesizes the wrapper with the generics of the target (`WardRV_wrapper_synth.vhd/.log` in the work root, warnings printed, fails on a synthesis error or an inferred latch), then elaborates it (`ghdl -r --no-run`).
- The synthesizable core is `WardRV_fsm` / `sbi_WardRV_fsm`: plain RTL, register file in `ram_2r1w`, the execution trace process is enclosed in `synthesis translate_off / translate_on`. It is synthesized in the PicoSoC emulation targets (`emu_ng_medium_soc*_wardrv_fsm*` for NanoXplore NG-MEDIUM, `emu_basys_soc1_wardrv_fsm_c_identity` for Digilent Basys) of `asylum:soc:PicoSoC`.

## Design Notes

### FSM

| State | Action | Next |
|-------|--------|------|
| `S_RESET` | - | `S_FETCH` |
| `S_FETCH` | `imem` request at `pc`; ALU computes `pc + 4` (stored in `pc_seq_r`) | `S_DECODE` when `imem` ready |
| `S_DECODE` | Combinational decode of `inst_r` | `S_EXECUTE` |
| `S_EXECUTE` | ALU executes the instruction, result latched in `alu_res_r` (value, address, jump / branch target) | `S_BRANCH_DECISION` (branch), `S_MEMORY` (load / store) or `S_WRITEBACK` |
| `S_BRANCH_DECISION` | ALU computes `rs1 - rs2`, branch condition latched (BEQ / BNE: zero, BLT / BGE: signed less-than `N xor V`, BLTU / BGEU: borrow) | `S_WRITEBACK` |
| `S_MEMORY` | `dmem` request at `alu_res_r` | `S_WRITEBACK` when `dmem` ready |
| `S_WRITEBACK` | Register write (ALU / memory / `pc + 4` / CSR), CSR write, MRET effect, PC update | `S_FETCH` on debug mode entry / DRET / EBREAK in debug mode, `S_TRAP` if an interrupt is pending or on a breakpoint exception, else `S_FETCH` |
| `S_TRAP` | `pc <= mtvec`, `mepc <= pc` (next instruction), `mcause`, `mstatus` updated | `S_FETCH` |

Next PC: `mtvec` on trap, `mepc` on MRET, `alu_res_r` with bit 0..1 cleared for JAL / JALR / taken branches, else `pc + 4`. An instruction therefore takes at least four cycles (FETCH, DECODE, EXECUTE, WRITEBACK) plus the memory wait states.

### Implementation limits

- Only the external interrupt causes a trap (and EBREAK when `EBREAK_EXCEPTION = true`): there is no exception for illegal instructions, ECALL or misaligned accesses; unknown instructions, FENCE, ECALL, WFI (and EBREAK by default, DRET outside debug mode) execute as NOPs.
- `WardRV_iss` only implements the CSR read of `mhartid`; the other CSR instructions are NOPs (the FSM core implements the CSRs listed in the register map).
- `sbi_WardRV_fsm` drives `interrupt_ack_o` to `'0'` and only uses `cke_i` on the instruction ready; the SBI bus is assumed 8-bit wide (`dmem2sbi`).

## Directory Structure

```
asylum-processor-WardRV/
├── WardRV.core                 # FuseSoC core (asylum:processor:WardRV)
├── Makefile                    # Common Asylum Makefile (FuseSoC wrapper)
├── mk/
│   ├── defs.mk                 # FILE_CORE, default TARGET and TOOL
│   └── targets.txt             # Target list (generated from the .core)
├── .github/workflows/ci.yml    # CI: one job per sim_* target (make ci_generate)
├── doc/
│   └── WardRV.drawio           # Block diagram
├── hdl/
│   ├── RV_pkg.vhd
│   ├── WardRV_pkg.vhd
│   ├── WardRV_stats_pkg.vhd
│   ├── bridge/                 # dmem2sbi.vhd, imem2sbi.vhd
│   ├── debug/                  # WardRV_debug_pkg.vhd, WardRV_dbg_{tap,dtm,dmi_cdc,jtag,dm,dm_mem,sba}.vhd,
│   │                           # WardRV_dbg_rom_pkg.vhd (generated), WardRV_debug.vhd
│   ├── fsm/                    # WardRV_decode_pkg.vhd, WardRV_fsm_{fetch,alu,decode,regfile,memory,csr,trigger,debug}.vhd,
│   │                           # WardRV_fsm.vhd, sbi_WardRV_fsm.vhd, README.md (empty)
│   └── iss/                    # WardRV_iss_pkg.vhd, WardRV_iss.vhd
├── sim/
│   ├── tb_WardRV.vhd
│   ├── tb_WardRV_pkg.vhd
│   ├── tb_WardRV_iss.vhd
│   ├── tb_WardRV_dbg_jtag.vhd   # Debug : JTAG unit test
│   ├── tb_WardRV_dbg_dm.vhd     # Debug : Debug Module unit test
│   ├── tb_WardRV_debug.vhd      # Debug : system test
│   ├── tb_WardRV_openocd.vhd    # Debug : OpenOCD + GDB co-simulation (cosim_openocd)
│   ├── openocd/                # remote_bitbang.c, rbb_pkg.vhd, wardrv_sim.cfg, scenario.py, launch.sh, check.sh
│   ├── WardRV_vips.vhd
│   └── save/tb_WardRV.vhd      # Old testbench (not in the core)
├── syn/
│   └── WardRV_wrapper.vhd      # Lint / synthesis check wrapper (lint_ghdl, lint_ghdl_synth_*)
├── esw/                        # FUSESOC_IGNORE
│   ├── testcase/               # start.S, main.c, link.ld, Makefile, firmware.hex/.lst
│   ├── debug_rom/              # debug_rom.S, gen_rom.py, Makefile (-> hdl/debug/WardRV_dbg_rom_pkg.vhd)
│   ├── debug/                  # gen_debug_fw.py + debug_fw.hex/.lst (sim_debug_system)
│   ├── compliance_act3/        # Makefile, config/wardrv/, docker/, benchs/ (39 x .hex/.lst/.signature)
│   ├── compliance_act4/        # Makefile, config/wardrv/, docker/, benchs/ (45 x .hex/.lst)
│   └── openocd/                # start.S, main.c, link.ld, Makefile, fw.elf/.hex/.lst (cosim_openocd)
```

## Dependencies

| Core | Used by (fileset) | Purpose |
|------|-------------------|---------|
| `asylum:utils:pkg` (`>=1.0.0`) | `files_hdl` | Common packages (`sbi_pkg`) |
| `asylum:component:ram` (`>=1.0.0`) | `files_hdl` | `ram_2r1w` register file (`ram_pkg`); `sbi_ram` in `tb_WardRV_dbg_dm` |
| `asylum:target:techmap` (`>=1.0.0`) | `files_hdl` | `sync2dffrn` synchronizers of the debug unit (`techmap_pkg`) |
| `bitvis:verification:uvvm` | `files_sim` | UVVM utility library of the testbench |
| `asylum:utils:generators` (`>=2.8.0`) | `syn` | `ghdl_synth.sh` / `ghdl_synth.mk` (`pre_run` hook of `lint_ghdl_synth_*`) |
