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

VLNV: `asylum:processor:WardRV:1.0.0`

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
- Optional RISC-V debug unit (Debug Specification 1.0, see [Debug Unit](#debug-unit)) for GDB + OpenOCD: JTAG TAP and DTM, Debug Module (halt, resume, single step, register access, program buffer, System Bus Access), hardware breakpoints / watchpoints (triggers, optionally address ranges), breakpoint exception, halt on reset. Every option is a generic, all off by default (the core is then unchanged)
- Verified with a self-checking C test, an interrupt test and the RISC-V architecture (compliance) tests: 39 ACT3 tests (riscv-arch-test 3.10.0) with signature comparison and 45 ACT4 self-checking tests (riscv-arch-test 4.1.0, I and Zicsr); the debug unit with unit tests, a system test through JTAG and an OpenOCD + GDB co-simulation of 9 configurations

## Block Diagram

Diagram: [doc/WardRV.drawio](doc/WardRV.drawio) (open with diagrams.net or the VS Code Draw.io extension).

- `WardRV_fsm_fetch` requests the instruction at `pc` on `imem` and registers it; in `sbi_WardRV_fsm` the instruction is ready one cycle after `ics_o` (`ready = cke_i and ics_r`), which fits a synchronous ROM.
- `WardRV_fsm_decode` (combinational) produces the immediates, register addresses and all control signals; `WardRV_fsm_regfile` provides `rs1` / `rs2`.
- The FSM drives the shared `WardRV_fsm_alu`: PC + 4 in FETCH, the instruction operation in EXECUTE (result kept in `alu_res_r`), `rs1 - rs2` in BRANCH_DECISION (zero, borrow and signed less-than flags).
- `WardRV_fsm_memory` aligns store data / byte enables and extracts load data; `dmem2sbi` turns each `dmem` request into one SBI byte access per enabled byte.
- `WardRV_fsm_csr` holds the machine CSRs, raises the trap request on an enabled external interrupt and provides `mtvec` / `mepc` for trap entry and MRET.

## Top-Level

Top-level entity: **`sbi_WardRV_fsm`** ([hdl/fsm/sbi_WardRV_fsm.vhd](hdl/fsm/sbi_WardRV_fsm.vhd)), library `asylum` (no component declaration, instantiate with `entity asylum.sbi_WardRV_fsm`).

The `default` target of [WardRV.core](WardRV.core) uses `sbi_WardRV_fsm` as toplevel; it is also the entity instantiated by the PicoSoC.

### Parameters

| Name | Type | Default | Description |
|------|------|---------|-------------|
| `HARTID` | std_logic_vector(31 downto 0) | `(others => '0')` | Value of the `mhartid` CSR |
| `RESET_ADDR` | std_logic_vector(31 downto 0) | `(others => '0')` | PC after reset |
| `IADDR_WIDTH` | natural | `32` | Width of `iaddr_o` |
| `IADDR_ALIGN_BITS` | natural | `0` | Number of PC LSBs dropped on `iaddr_o` (2 = word address), `IADDR_WIDTH + IADDR_ALIGN_BITS <= 32` |
| `DEBUG_ENABLE` | boolean | `false` | Debug mode and debug memory port, for the Debug Module (see [Debug Unit](#debug-unit)) |
| `NB_TRIGGERS` | natural | `0` | Number of triggers (hardware breakpoints / watchpoints), needs `DEBUG_ENABLE` |
| `EBREAK_EXCEPTION` | boolean | `false` | EBREAK (without `dcsr.ebreakm`) raises a breakpoint exception (`mcause = 3`) instead of being a NOP |
| `RESETHALTREQ_ENABLE` | boolean | `false` | Halt before the first instruction after reset on request of the Debug Module |
| `TRIGGER_RANGE_ENABLE` | boolean | `false` | Triggers: match `>=` / `<` and `chain` (address ranges), needs `NB_TRIGGERS` |

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

#### Debug (`DEBUG_ENABLE`)

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `dbg_hart_req_i` | in | dbg_hart_req_t | Halt requests of `WardRV_debug` (`haltreq`, `resethaltreq`), default `DBG_HART_REQ_NULL` |
| `dbg_mem_ini_o` | out | dmem_ini_t | Accesses of the core to the debug memory (debug mode only) |
| `dbg_mem_tgt_i` | in | dmem_tgt_t | Debug memory response, default `DMEM_TGT_NULL` |

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
| [hdl/fsm/WardRV_fsm_debug.vhd](hdl/fsm/WardRV_fsm_debug.vhd) | `WardRV_fsm_debug` | entity | Debug support of the core: debug mode, `dcsr` / `dpc` / `dscratch0`, debug mode entry / exit, breakpoint exception |
| [hdl/fsm/WardRV_fsm_trigger.vhd](hdl/fsm/WardRV_fsm_trigger.vhd) | `WardRV_fsm_trigger` | entity | Triggers (mcontrol6): `tselect`, `tdata1-3`, `tinfo`, address match |
| [hdl/fsm/WardRV_fsm_debug_bus.vhd](hdl/fsm/WardRV_fsm_debug_bus.vhd) | `WardRV_fsm_debug_bus` | entity | In debug mode, the accesses to `0xFFFF_F800` go to the debug memory |
| [hdl/fsm/WardRV_fsm.vhd](hdl/fsm/WardRV_fsm.vhd) | `WardRV_fsm` | entity | Multi-cycle RV32I core (FSM, PC, datapath muxes) |
| [hdl/fsm/sbi_WardRV_fsm.vhd](hdl/fsm/sbi_WardRV_fsm.vhd) | `sbi_WardRV_fsm` | entity | Top-level: `WardRV_fsm` + `dmem2sbi` with the `sbi_OpenBlaze8`-compatible interface |
| [hdl/debug/WardRV_debug_pkg.vhd](hdl/debug/WardRV_debug_pkg.vhd) | `WardRV_debug_pkg` | package | JTAG instructions, DMI request, DM register addresses, `dbg_hart_req_t`, debug memory map |
| [hdl/debug/WardRV_dbg_tap.vhd](hdl/debug/WardRV_dbg_tap.vhd) | `WardRV_dbg_tap` | entity | IEEE 1149.1 TAP: IDCODE, BYPASS, user registers (TCK domain) |
| [hdl/debug/WardRV_dbg_dtm.vhd](hdl/debug/WardRV_dbg_dtm.vhd) | `WardRV_dbg_dtm` | entity | RISC-V DTM: `dtmcs` and `dmi` registers (TCK domain) |
| [hdl/debug/WardRV_dbg_dmi_cdc.vhd](hdl/debug/WardRV_dbg_dmi_cdc.vhd) | `WardRV_dbg_dmi_cdc` | entity | TCK -> clk 4-phase handshake of the DMI (`sync2dffrn`) |
| [hdl/debug/WardRV_dbg_jtag.vhd](hdl/debug/WardRV_dbg_jtag.vhd) | `WardRV_dbg_jtag` | entity | JTAG transport: TAP + DTM + CDC |
| [hdl/debug/WardRV_dbg_dm.vhd](hdl/debug/WardRV_dbg_dm.vhd) | `WardRV_dbg_dm` | entity | Debug Module: DM registers, abstract commands, hart halt / resume |
| [hdl/debug/WardRV_dbg_dm_mem.vhd](hdl/debug/WardRV_dbg_dm_mem.vhd) | `WardRV_dbg_dm_mem` | entity | Debug memory seen by the core: ROM, flags, abstract command program, program buffer, data |
| [hdl/debug/WardRV_dbg_rom_pkg.vhd](hdl/debug/WardRV_dbg_rom_pkg.vhd) | `WardRV_dbg_rom_pkg` | package | Debug ROM, generated from [esw/debug_rom/debug_rom.S](esw/debug_rom/debug_rom.S) |
| [hdl/debug/WardRV_dbg_sba.vhd](hdl/debug/WardRV_dbg_sba.vhd) | `WardRV_dbg_sba` | entity | System Bus Access (`sbcs`, `sbaddress0`, `sbdata0`) |
| [hdl/debug/WardRV_debug.vhd](hdl/debug/WardRV_debug.vhd) | `WardRV_debug` | entity | Debug unit: `WardRV_dbg_jtag` + `WardRV_dbg_dm` |

`hdl/fsm/README.md` is an empty file.

### WardRV_fsm

| Kind | Name | Type | Default / Description |
|------|------|------|-----------------------|
| generic | `HARTID` | std_logic_vector(31 downto 0) | `(others => '0')`, `mhartid` value |
| generic | `RESET_ADDR` | std_logic_vector(31 downto 0) | `(others => '0')`, reset PC |
| generic | `VERBOSE` | boolean | `true`, simulation trace in `exec_fsm.log` (not forwarded by `sbi_WardRV_fsm`, so the default applies) |
| generic | `DEBUG_ENABLE`, `NB_TRIGGERS`, `EBREAK_EXCEPTION`, `RESETHALTREQ_ENABLE`, `TRIGGER_RANGE_ENABLE` | | As `sbi_WardRV_fsm` |
| in | `clk_i` | std_logic | Clock |
| in | `arst_b_i` | std_logic | Asynchronous reset, active low |
| out | `imem_ini_o` | imem_ini_t | Instruction request (`valid`, `addr` = PC) |
| in | `imem_tgt_i` | imem_tgt_t | Instruction response (`ready`, `inst`) |
| out | `dmem_ini_o` | dmem_ini_t | Data request (`valid`, `addr`, `wdata`, `we`, `be`) |
| in | `dmem_tgt_i` | dmem_tgt_t | Data response (`ready`, `rdata`, `err` unused) |
| in | `meip_i` | std_logic | Machine external interrupt pending |
| in / out / in | `dbg_hart_req_i`, `dbg_mem_ini_o`, `dbg_mem_tgt_i` | | As `sbi_WardRV_fsm` |

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

`dmem2sbi` (IDLE / TRANSFER / DONE) skips disabled bytes and shifts the write data register by one byte per lane; read bytes are accumulated in the same register. `imem2sbi` always performs four reads (`cs` = `re` in TRANSFER).

### WardRV_fsm sub-modules

| Entity | Ports | Function |
|--------|-------|----------|
| `WardRV_fsm_fetch` | `clk_i`, `arst_b_i`, `imem_valid_i`, `pc_i`, `imem_ready_o`, `inst_r_o`, `imem_ini_o`, `imem_tgt_i` | `imem.valid` = registered request, `imem.addr` = PC, instruction registered on `valid and ready` |
| `WardRV_fsm_decode` | `inst_i`; `imm_i_o`, `imm_s_o`, `imm_b_o`, `imm_u_o`, `imm_j_o`, `imm_csr_o`; `rd_addr_o`, `rs1_addr_o`, `rs2_addr_o`, `rd_src_o`, `rd_we_o`, `rs1_re_o`, `rs2_re_o`; `alu_op_o`, `alu_src_a_sel_o`, `alu_src_b_sel_o`; `mem_req_o`, `mem_we_o`, `mem_be_o`, `mem_data_unsigned_o`; `is_branch_o`, `branch_use_flag_zero_o`, `branch_use_flag_carry_o`, `branch_use_flag_sign_o`, `branch_flag_is_set_o`, `pc_sel_o`; `csr_we_o`, `csr_re_o`, `csr_addr_o`; `inst_is_mret_o`, `inst_is_ebreak_o`, `inst_is_dret_o`, `inst_type_o` | Immediates, register addresses and control signals; `rd_we = 0` when `rd = x0` |
| `WardRV_fsm_alu` | `src_a_i`, `src_b_i`, `op_i` (`alu_op_t`), `res_o`, `zero_o`, `sign_o`, `carry_o` | ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND, CLR (`src_b and not src_a`, CSRRC / CSRRCI); carry = carry / borrow of ADD / SUB (unsigned less-than after SUB); sign = `res(31) xor overflow` after SUB (signed less-than, correct on signed overflow), `res(31)` otherwise |
| `WardRV_fsm_regfile` | `clk_i`, `arst_b_i`, `rs1_re_i`, `rs1_addr_i`, `rs1_rdata_o`, `rs2_re_i`, `rs2_addr_i`, `rs2_rdata_o`, `rd_addr_i`, `rd_wdata_i`, `rd_we_i` | `ram_2r1w` (WIDTH 32, DEPTH 32, asynchronous read); reads of `x0` return 0 |
| `WardRV_fsm_memory` | `clk_i`, `arst_b_i`, `dmem_valid_i`, `addr_i`, `wdata_i`, `we_i`, `be_i`, `data_unsigned_i`, `dmem_ready_o`, `dmem_rdata_r_o`, `dmem_ini_o`, `dmem_tgt_i` | Byte enables and write data shifted by `addr(1:0)`, read data shifted back, sign / zero extended and registered |
| `WardRV_fsm_csr` | `clk_i`, `arst_b_i`, `csr_addr_i`, `csr_we_i`, `csr_re_i`, `csr_wdata_i`, `csr_rdata_o`, `csr_mtvec_o`, `trap_i`, `trap_cause_i`, `trap_pc_i`, `trap_mtval_i`, `inst_is_mret_i`, `meip_i`, `trap_mirq_o` (generic `HARTID`) | CSR read mux / write, trap entry (`MPIE <= MIE`, `MIE <= 0`, `mepc`, `mcause`, `mtval`), MRET (`MIE <= MPIE`, `MPIE <= 1`, applied once, in WRITEBACK), `trap_mirq = MEIP and MEIE and MIE` (MEIE / MIE as written by the CSR instruction in WRITEBACK, so the instruction that clears them is never followed by the trap) |

| `WardRV_fsm_debug` | FSM state, `pc_i`, `pc_next_i`, `inst_is_ebreak_i`, `inst_is_dret_i`, `mem_req_i` / `mem_we_i` / `mem_addr_i`, `irq_i`, CSR interface, `dbg_hart_req_i`; `debug_mode_o`, `irq_mask_o`, `exception_o`, `trap_bkpt_o`, `pc_we_o`, `pc_o` | See [Debug Unit](#debug-unit) |
| `WardRV_fsm_trigger` | `debug_mode_i`, CSR interface, `pc_i`, `mem_req_i` / `mem_we_i` / `mem_addr_i`; `hit_o`, `hit_debug_o` | See [Debug Unit](#debug-unit) |
| `WardRV_fsm_debug_bus` | `debug_mode_i`, the `imem` / `dmem` of the fetch and memory units, the `imem` / `dmem` of the core, `dbg_mem` | Without `DEBUG_ENABLE`: wires |

## Debug Unit

RISC-V Debug Specification 1.0 (ratified), execution-based, for GDB + OpenOCD on the JTAG port: halt / resume / single step, register and memory access, hardware breakpoints and watchpoints, halt on reset. Two parts, both optional (generics, off by default: the core is then the one described above):

- in the core (`WardRV_fsm` / `sbi_WardRV_fsm`, `DEBUG_ENABLE`): debug mode, debug CSRs, triggers (`NB_TRIGGERS`, ranges with `TRIGGER_RANGE_ENABLE`), breakpoint exception (`EBREAK_EXCEPTION`), halt on reset (`RESETHALTREQ_ENABLE`);
- outside the core, `WardRV_debug`: JTAG transport and Debug Module (DM).

### Architecture

```
            TCK domain                     |                 clk domain
 JTAG   +-------------+  user  +-----------+  dmi_req   +----------------------+
 pins ->| WardRV_dbg_ |  DR    | dbg_dtm + |----------->| WardRV_dbg_dm        |--> ndmreset_o (reset of the system)
        | tap (1149.1)|<------>| dmi_cdc   |<-----------| dbg_dm_mem, dbg_sba  |--> sba_dmem   (System Bus Access)
        +-------------+        +-----------+  dmi_rdata +----------------------+
         WardRV_dbg_jtag                                   | dbg_hart_req   ^ dbg_mem (debug memory, 0xFFFF_F800)
                                                           v                |
                                                  +----------------------------------+
                                                  | WardRV_fsm                       |
                                                  |   WardRV_fsm_debug (+ _trigger)  |
                                                  |   WardRV_fsm_debug_bus           |
                                                  +----------------------------------+
```

How a debugger works with it (execution-based debug: the core executes the debug code itself):

1. **Halt**: the DM raises `haltreq`. At the end of the current instruction the core enters debug mode: `dpc` = next pc, `dcsr.cause` = 3, pc = `0xFFFF_F800`, the debug ROM. The ROM saves s0 in `dscratch0` and writes HALTED: `dmstatus.allhalted` = 1. It then waits for GO or RESUME.
2. **Register access** (abstract command): the DM builds a small program at ABSTRACT (for example `sw x5, data0(zero)` to read x5) and sets GO. The ROM jumps to it; the program ends with an ebreak, which brings the core back to the ROM: HALTED ends the command. With `postexec` the program falls into the program buffer (instructions written by the debugger, for example to access memory), which also ends with an ebreak.
3. **Resume**: the DM sets RESUME, the ROM writes RESUMING (`dmstatus.allresumeack`) and executes `dret`: pc = `dpc`, debug mode left.

The debug memory is reached with x0-relative loads / stores / jumps (immediate = offset - 0x800): the ROM needs no register for the addresses.

### Core side

`WardRV_fsm` keeps the debug logic in two entities, instantiated in one commented "Debug" section; the hooks in the FSM are the CSR read data (`csr_rdata_core or dbg_csr_rdata`), the interrupt mask, the cause of the trap (breakpoint), the pc write (`dbg_pc_we`, `dbg_pc`) and two branches of `S_EXECUTE`.

`WardRV_fsm_debug` (Debug Spec 4, Sdext):

| Debug mode entry | `dcsr.cause` | When | `dpc` |
|------------------|--------------|------|-------|
| trigger with `dmode` = 1 | 2 | `S_EXECUTE`, the instruction is abandoned | pc |
| `ebreak` with `dcsr.ebreakm` | 1 | `S_EXECUTE`, the instruction is abandoned | pc |
| `resethaltreq` / `haltreq` at reset | 5 / 3 | `S_RESET`, before the first instruction | `RESET_ADDR` |
| `haltreq` | 3 | end of `S_WRITEBACK`, or `S_TRAP` when an interrupt is taken | next pc (`mtvec` in `S_TRAP`) |
| step (`dcsr.step`) | 4 | same, one instruction after `dret` | next pc |

- In debug mode: interrupts are masked (also during a step when `dcsr.stepie` = 0), triggers do not fire, `ebreak` goes back to the ROM, `dret` leaves debug mode at `dpc`. The core raises no exception in debug mode, so the ROM has no exception entry.
- CSRs, accessible in debug mode only: `dcsr` (debugver 4, `ebreakm`, `stepie`, `cause`, `step`, prv = 3), `dpc` (bits 1..0 zero), `dscratch0` (used by the ROM and the abstract commands).
- `EBREAK_EXCEPTION`: an `ebreak` without `dcsr.ebreakm` (or a trigger with `dmode` = 0) traps from `S_EXECUTE` to `S_TRAP`: `mcause` = 3, `mepc` = address of the instruction, `mtval` = 0. Without it, `ebreak` is a NOP.

`WardRV_fsm_trigger` (Debug Spec 5, Sdtrig): `NB_TRIGGERS` triggers of type mcontrol6, match on the address of the instruction (execute) or of a load / store, timing before, M-mode. The match is "equal"; with `TRIGGER_RANGE_ENABLE`, also `match` = 2 (`>=`) and 3 (`<`), and `chain`: a chain of triggers (`chain` = 1 on all but the last one) fires only when all its triggers match, with the action of its last trigger, so two chained triggers `>= A` and `< B` stop on the range [A, B). `chain` is WARL: it stays 0 on the last trigger and from a trigger of M-mode into a trigger of the debugger, and a write that sets `dmode` after an M-mode trigger with `chain` is ignored. Without `TRIGGER_RANGE_ENABLE`, `match` and `chain` read back 0. `action` = `dmode`: the triggers of the debugger (`dmode` = 1, writable in debug mode only) enter debug mode, the triggers of M-mode software (`dmode` = 0) raise a breakpoint exception and can only fire with `EBREAK_EXCEPTION` (otherwise their `m` bit stays 0). `tselect` ignores an index out of range, `tinfo` = `0x01000040`.

`WardRV_fsm_debug_bus`: in debug mode, the fetches and the loads / stores of `0xFFFF_F800 - 0xFFFF_FFFF` go to the debug memory of the DM (`dbg_mem`, no wait state); every other access keeps `imem` / `dmem`.

### WardRV_debug

| Generic | Type | Default | Description |
|---------|------|---------|-------------|
| `IDCODE_VALUE` | std_logic_vector(31 downto 0) | `x"10000001"` | JTAG IDCODE (bit 0 = 1, manufacturer 0x7F forbidden: assertion) |
| `TRST_ENABLE` | boolean | `true` | Use TRST* |
| `DTM_IDLE_CYCLES` | natural | `5` | `dtmcs.idle`: Run-Test/Idle cycles advised to the debugger between two DMI scans |
| `PROGBUF_SIZE` | natural | `2` | Program buffer words (0..16; 0: register access only, `postexec` not supported). `dmstatus.impebreak` = 1: the word after the program buffer is always an ebreak |
| `DATA_COUNT` | positive | `2` | Data registers (1..12) |
| `AARPOSTINCREMENT_ENABLE` | boolean | `false` | `aarpostincrement` of the Access Register command: `regno` incremented after each transfer, also with `abstractauto` (otherwise `cmderr` 2) |
| `SBA_ENABLE` | boolean | `false` | System Bus Access (memory access without the core, also while it runs) |
| `RESETHALTREQ_ENABLE` | boolean | `false` | `setresethaltreq` / `clrresethaltreq` (with the same generic in the core) |

| Name | Direction | Type | Description |
|------|-----------|------|-------------|
| `jtag_ini_i` | in | jtag_ini_t | TCK, TMS, TDI, TRST* (TCK domain) |
| `jtag_tgt_o` | out | jtag_tgt_t | TDO, `tdo_en` (TDO driven only in Shift-IR / Shift-DR) |
| `clk_i` | in | std_logic | Clock of the core |
| `arst_b_i` | in | std_logic | Power-on reset of the debug unit (never `ndmreset_o`) |
| `hart_rst_b_i` | in | std_logic | Reset of the core (`dmstatus.havereset`, `unavail`) |
| `dbg_hart_req_o` | out | dbg_hart_req_t | To the core: `haltreq`, `resethaltreq` |
| `dbg_mem_ini_i` / `dbg_mem_tgt_o` | in / out | dmem_ini_t / dmem_tgt_t | Debug memory, from / to the core |
| `ndmreset_o` | out | std_logic | Reset request of the system (core and peripherals, not the debug unit) |
| `sba_dmem_ini_o` / `sba_dmem_tgt_i` | out / in | dmem_ini_t / dmem_tgt_t | System Bus Access, 32-bit requests with byte enables (`dmem2sbi` converts them for an SBI bus) |

Blocks:

- `WardRV_dbg_tap`: IEEE 1149.1 TAP controller (Test-Logic-Reset after 5 TCK with TMS = 1, TRST*), IR of 5 bits (Capture-IR `00001`), IDCODE `00001`, DTMCS `10000`, DMI `10001`, any other opcode BYPASS. IR, user registers and TDO change on the falling edge of TCK.
- `WardRV_dbg_dtm`: `dtmcs` (version 1, abits 7, dmistat, idle, dmireset, dmihardreset) and `dmi` (address 7 bits, data, op). A scan while the previous request is in progress sets the sticky busy status (op = 3): the request is ignored until `dmireset`. The DM never fails a request (unimplemented registers read 0).
- `WardRV_dbg_dmi_cdc`: 4-phase req / ack handshake TCK <-> clk through `sync2dffrn`, the read data is kept in the TCK domain. It moves only while TCK runs: the debugger waits `dtmcs.idle` cycles in Run-Test/Idle and retries on busy (OpenOCD does).
- `WardRV_dbg_dm`: `data0..`, `dmcontrol`, `dmstatus`, `hartinfo`, `abstractcs`, `command`, `abstractauto`, `progbuf0..`, `sbcs` / `sbaddress0` / `sbdata0`, `haltsum0`. Access Register commands (`aarsize` 2) of the GPRs (`0x1000-0x101F`) and CSRs (`0x0000-0x0FFF`); `cmderr` 2 for another command type, `aarsize`, `aarpostincrement` without `AARPOSTINCREMENT_ENABLE` or `postexec` without program buffer, 3 for a register that does not exist, 4 if the core is not halted, 1 for an access during a command. CSRs that the core does not implement read 0 without error. `dmactive` = 0 resets the DM.
- `WardRV_dbg_dm_mem`: the debug memory (below), combinational.
- `WardRV_dbg_sba`: 8, 16 and 32-bit accesses, `sbautoincrement`, `sbreadonaddr`, `sbreadondata`, `sberror` 3 / 4, `sbbusyerror`; `sbasize` = 32.

### Debug memory

2 KB at `0xFFFF_F800`, seen by the core in debug mode only (`WardRV_debug_pkg`):

| Offset | Content | Core access |
|--------|---------|-------------|
| `0x000` | Debug ROM ([esw/debug_rom/debug_rom.S](esw/debug_rom/debug_rom.S), 12 instructions), halt entry | R |
| `0x100` / `0x104` | HALTED / RESUMING | W |
| `0x108` / `0x10C` | GO / RESUME flags | R |
| `0x200` | Abstract command program (5 words): `csrw dscratch0, s0`, the access, `csrr s0, dscratch0`, then `ebreak` (or the program buffer with `postexec`) | R |
| `0x214` | Program buffer (`PROGBUF_SIZE` words), then `ebreak` | R |
| `0x300` | Data registers (`DATA_COUNT` words), `hartinfo.dataaddr` = `0xB00` | R/W |

The ROM package `hdl/debug/WardRV_dbg_rom_pkg.vhd` is generated by `make` in `esw/debug_rom` (RISC-V toolchain, then `rom_pkg.awk`); `debug_rom.hex`, `debug_rom.lst` and the package are committed.

### What GDB / OpenOCD can do in each configuration

Checked by the co-simulation targets `sim_dbg_openocd_*` (OpenOCD 0.12.0, GDB 15.1):

| Program buffer | SBA | Triggers | Memory, core halted (`load`, `x`, `set var`) | Memory, core running | Software breakpoint (`break`) | `hbreak` / `watch` | `stepi` / `next` | Halt, resume, registers, `monitor step` |
|---|---|---|---|---|---|---|---|---|
| 0 | no | 0 | no (error) | no | no (error) | no (error) | no: GDB single-steps in software, it reads the instruction | yes |
| 0 | no | 2 | no (error) | no | no (error) | `hbreak` yes; `watch` stops the core but GDB sees a SIGTRAP (OpenOCD reads the store instruction to identify the watchpoint) | no | yes |
| 0 | yes | 0 / 2 | yes (System Bus Access) | yes | yes (written through the SBA) | yes with triggers, else refused | yes | yes |
| 2 | no | 0 / 2 | yes (program buffer) | no (error) | yes | yes with triggers, else refused | yes | yes |
| 2 | yes | 0 / 2 | yes (both) | yes (SBA) | yes | yes with triggers, else refused | yes | yes |
| 16, 12 data, `aarpostincrement` | yes | 8 with ranges | yes (both) | yes (SBA) | yes | yes, several together (4 `hbreak` + 1 `watch`) | yes | yes |

OpenOCD 0.12 uses neither `aarpostincrement` (one Access Register command per register) nor the range triggers (a `watch` on a 16-byte object is one "equal" trigger on its first address): with GDB + OpenOCD 0.12 they change nothing, the co-simulation only checks that they do not disturb it.

Without triggers, GDB does not fall back to a software watchpoint: the hardware watchpoint is refused when the core resumes. `monitor step` uses `dcsr.step` (the hardware single step); GDB 15 + OpenOCD 0.12 single-step with a breakpoint on the next instruction (OpenOCD does not report `vContSupported`), which needs memory access.

### Using GDB + OpenOCD on a board

1. Integration in the SoC (see [syn/WardRV_wrapper.vhd](syn/WardRV_wrapper.vhd), `DEBUG_ENABLE = true`):
   - `sbi_WardRV_fsm` with `DEBUG_ENABLE` (and `NB_TRIGGERS`, ...) and `WardRV_debug` with the same `clk_i`; connect `dbg_hart_req`, `dbg_mem_ini`, `dbg_mem_tgt`;
   - `arst_b_i` of `WardRV_debug` = power-on reset; reset of the core and of the peripherals = power-on reset and not `ndmreset_o`, also on `hart_rst_b_i`;
   - JTAG on 4 (5 with TRST*) FPGA pins, pull-ups on TMS, TDI and TRST*, TDO tri-stated by `tdo_en`. TCK is a clock: give it its own clock constraint, the CDC paths are false paths (`sync2dffrn`) or quasi-static (payload);
   - `SBA_ENABLE`: `sba_dmem` through `dmem2sbi` to an extra master port of the SoC interconnect.
2. OpenOCD: take [sim/openocd/wardrv.cfg](sim/openocd/wardrv.cfg), replace the `remote_bitbang` lines by the configuration of the JTAG probe (for example `adapter driver ftdi` and its layout) and an `adapter speed`, set `-expected-id` to `IDCODE_VALUE`. If TCK is fast compared to `clk_i`, raise `DTM_IDLE_CYCLES` (OpenOCD also retries on busy).
3. GDB: `gdb-multiarch firmware.elf`, then `target extended-remote localhost:3333`, `monitor reset halt`, `load`, `break main`, `continue`, `hbreak`, `watch`, `x`, `set var`, `monitor step`...

## Register Map

This IP has no software-visible registers.

Programmer-visible state: 32 general-purpose registers `x0`..`x31` (`x0` = 0), the PC (reset value `RESET_ADDR`) and the machine CSRs below (all other CSR addresses read as 0 and ignore writes).

| CSR | Address | Reset | Notes |
|-----|---------|-------|-------|
| `mstatus` | `0x300` | `0x00001800` | MPP = 11; MIE (bit 3) / MPIE (bit 7) updated on trap and MRET |
| `mie` | `0x304` | `0` | MEIE (bit 11) enables the external interrupt |
| `mtvec` | `0x305` | `0` | Trap address, MODE (bits 1..0) read-only zero (direct mode only) |
| `mscratch` | `0x340` | `0` | Scratch |
| `mepc` | `0x341` | `0` | Return address (MRET), bits 1..0 read-only zero |
| `mcause` | `0x342` | `0` | `0x8000000B` on external interrupt |
| `mtval` | `0x343` | `0` | Written with 0 on trap |
| `mip` | `0x344` | `0` | MEIP (bit 11) = `meip_i` (registered), read-only |
| `mhartid` | `0xF14` | `HARTID` | Read-only |

Debug CSRs (`DEBUG_ENABLE`, accessible in debug mode only, otherwise read as 0):

| CSR | Address | Reset | Notes |
|-----|---------|-------|-------|
| `dcsr` | `0x7B0` | `0x40000003` | debugver 4, `ebreakm` (15), `stepie` (11), `cause` (8:6, read-only), `step` (2), prv 3 |
| `dpc` | `0x7B1` | `0` | Address of the debug mode entry, `dret` target, bits 1..0 zero |
| `dscratch0` | `0x7B2` | `0` | Used by the debug ROM and the abstract commands |

Trigger CSRs (`NB_TRIGGERS` > 0, also in M-mode): `tselect` (`0x7A0`, `0..NB_TRIGGERS-1`), `tdata1` (`0x7A1`, mcontrol6, reset `0x60000000`, `match` 0 or with `TRIGGER_RANGE_ENABLE` 2 / 3, and `chain`), `tdata2` (`0x7A2`, address), `tdata3` (`0x7A3`, 0), `tinfo` (`0x7A4`, `0x01000040`).

With `EBREAK_EXCEPTION`, a breakpoint exception gives `mcause = 3`, `mepc` = address of the instruction, `mtval = 0`.

## Verification

### Testbenches

| File | DUT | Description |
|------|-----|-------------|
| [sim/tb_WardRV.vhd](sim/tb_WardRV.vhd) + [sim/tb_WardRV_pkg.vhd](sim/tb_WardRV_pkg.vhd) | `WardRV_fsm` (`MODEL = "FSM"`, default) or `WardRV_iss` (`MODEL = "ISS"`) | UVVM testbench, answers `imem` / `dmem` requests in one cycle (`HARTID = 0x900DC0DE`). `TEST_ENV = "ACT3"` (default): loads `FIRMWARE_FILE` (hex) in a memory at `0x80000000` (`RESET_ADDR`), ends when the program writes `tohost` (`0x80200000`): `1` = TEST PASSED, other = TEST FAILED. When `SIGNATURE_FILE` and `GOLDEN_FILE` are set (ACT3 compliance targets), dumps the signature area (`0x80202104`..`0x80203000`) to `SIGNATURE_FILE` and compares it word by word with `GOLDEN_FILE` (`TB_ERROR` on the first mismatch). Timeout 500 us at 10 ns. `TEST_ENV = "ACT4"`: memory of 512 KB at `0x00004000`, the bytes written at `0x10000000` are printed (console), ends when the program writes `0x20000000`: `123456789` = TEST PASSED, other = TEST FAILED. Timeout 10 ms A write at `0x80300000` (`C_MEIP_ADDR`) sets `meip_i` to `wdata(0)` at the next instruction fetch (the interrupt is pending during the instruction after the store). |
| [sim/tb_WardRV_iss.vhd](sim/tb_WardRV_iss.vhd) | `iss_t` (procedural) | Same memory model driving the ISS protected type directly from a process (no target uses it as toplevel) |
| [sim/WardRV_vips.vhd](sim/WardRV_vips.vhd) | - | VIP package: reset pulse, JTAG (init, clock, idle, reset, IR / DR scan), DTM (`dtmcs`, `dmi` scan, DMI read / write with the busy retry of a debugger) |
| [sim/tb_WardRV_dbg_jtag.vhd](sim/tb_WardRV_dbg_jtag.vhd) + [sim/tb_WardRV_dbg_jtag_pkg.vhd](sim/tb_WardRV_dbg_jtag_pkg.vhd) | `WardRV_dbg_jtag` | JTAG VIP, fake Debug Module (128 registers), TDO monitor, clk period `CLK_PERIOD_NS`; scenarios `reset` (IDCODE and `dtmcs` after reset), `tap`, `dmi`, `busy` (generic `TEST`) |
| [sim/tb_WardRV_dbg_dm.vhd](sim/tb_WardRV_dbg_dm.vhd) + [sim/tb_WardRV_dbg_dm_pkg.vhd](sim/tb_WardRV_dbg_dm_pkg.vhd) | `WardRV_dbg_dm` (SBA, `PROGBUF_SIZE` 2 or 0) | The sequencer is the debugger (DMI) and the core (debug memory accesses, abstract programs checked against GNU as encodings); SBA memory with a wait state; scenarios `reset` (`dmcontrol`, `dmstatus`, `abstractcs`, `sbcs`, `hartinfo`, `haltsum0` after reset), `dmcontrol`, `halt_resume`, `abstract`, `cmderr`, `autoexec`, `resethaltreq`, `sba`, `progbuf0` and, with `AARPOSTINCREMENT_ENABLE`, `postincrement` |
| [sim/tb_WardRV_debug.vhd](sim/tb_WardRV_debug.vhd) + [sim/tb_WardRV_debug_pkg.vhd](sim/tb_WardRV_debug_pkg.vhd) | `WardRV_fsm` (2 triggers) + `WardRV_debug` | System test: a debugger on the JTAG port, the core runs `esw/debug/debug_fw.S`; scenarios `reset` (the core runs its firmware, DM idle), `halt_resume`, `registers`, `cmderr`, `progbuf`, `step`, `ebreak`, `trigger_execute`, `trigger_store`, `trigger_load`, `resethaltreq`, `interrupt`, with `EBREAK_EXCEPTION` `exc_ebreak`, `exc_trigger` and, with `TRIGGER_RANGE_ENABLE`, `trigger_ge`, `trigger_lt`, `trigger_range_execute`, `trigger_range_store` |
| [sim/tb_WardRV_openocd.vhd](sim/tb_WardRV_openocd.vhd) + [sim/openocd/](sim/openocd/) | `WardRV_fsm` + `WardRV_debug` | OpenOCD + GDB co-simulation, see [Co-simulation](#openocd--gdb-co-simulation) |
| [sim/tb_imem2sbi.vhd](sim/tb_imem2sbi.vhd) | `imem2sbi` + `sbi_ram` | Two instances (`sbi_ram` with `SYNC_READ = false` / `true`): the RAM is filled through SBI, then the bridge fetches words (aligned, unaligned, address bits above the SBI width) and the instruction is checked; `cs` of the bridge must equal `re` |
| sim/save/tb_WardRV.vhd | - | Older copy of `tb_WardRV` (`SIGNATURE_FILE := "signature.output"`), not in the core |

Software:

- [esw/irq/](esw/irq/): firmware of `sim_irq` in assembly, [irq.S](esw/irq/irq.S), built by its `Makefile` with a RISC-V toolchain (`irq_fw.hex` / `irq_fw.lst` committed, the simulation does not need the toolchain): `meip_i` pending during `csrci mstatus` / `csrc mie` (no trap) and during `csrsi mstatus` / `csrs mie` (trap right after the instruction: `mepc`, `mcause`, `mstatus.MPIE` checked), `mtvec` written with `MODE = 3` read back with `MODE = 0` and the next trap entering the aligned handler, `mret` with `mstatus.MPIE = 0` leaving `MIE = 0`, `tohost` = 1 or the number of the failing check.
- [esw/debug/](esw/debug/): firmware of the debug system test in assembly, [debug_fw.S](esw/debug/debug_fw.S) (counter loop, ebreak when the debugger sets a flag, trap handler counting the interrupts and the breakpoint exceptions), built by its `Makefile` as `esw/irq` (`debug_fw.hex` / `.lst` committed).
- [esw/debug_rom/](esw/debug_rom/): debug ROM of the Debug Module, [debug_rom.S](esw/debug_rom/debug_rom.S); `make` builds `debug_rom.hex` / `.lst` and generates `hdl/debug/WardRV_dbg_rom_pkg.vhd` with `rom_pkg.awk` (all committed).
- [esw/openocd/](esw/openocd/): firmware debugged by GDB in the co-simulation (`start.S`, `main.c`, `link.ld`: 64 KB at `0x80000000`, `-O0 -g`), built by the `pre_build` hook of the targets with the toolchain of the image: nothing generated is committed.
- [esw/testcase/](esw/testcase/): `start.S` + `main.c` (RV32I arithmetic, shifts, comparisons, immediates, loads / stores of every width, `check()` writing `tohost`), `link.ld` (128 KB at `0x80000000`), `Makefile` (`riscv64-unknown-elf-gcc`, `-march=rv32i_zicsr -mabi=ilp32`); the prebuilt `firmware.hex` / `firmware.lst` are committed.
- [esw/compliance_act3/](esw/compliance_act3/): ACT3 flow (riscv-arch-test 3.10.0, `rv32i_m/I`, `-march=rv32i`). `make full` downloads the pinned tools in `riscv-compliance-ws/` (xPack GCC 13.3.0-1 / binutils 2.42, Sail 0.13.1), builds the 39 tests into `.hex` / `.lst`, runs them on Sail (RV32I, [config/wardrv/sail_rv32i.json](esw/compliance_act3/config/wardrv/sail_rv32i.json)) to get the reference `.signature`, and compares everything with the committed `benchs/` (byte identical). DUT configuration in [config/wardrv/](esw/compliance_act3/config/wardrv/) (`link.ld`, `model_test.h`). `docker/` builds an Ubuntu 24.04 image containing all the tools, so that the generation does not depend on the host distribution nor on the network: `make build` (network needed once), `make generate` (runs `make full` without network), `make save` / `make load` (keep the image in a `.tar.gz`); `ENGINE=docker` (default) or `ENGINE=podman` (rootless podman).
- [esw/compliance_act4/](esw/compliance_act4/): ACT4 flow (riscv-arch-test 4.1.0, extensions `I,Zicsr`). `make full` downloads the pinned tools in `riscv-compliance-ws/` (xPack GCC 15.2.0-1 / binutils 2.45, Sail 0.13.1, mise for uv / Python / Ruby), generates the 45 self-checking ELFs, converts them into `.hex` / `.lst` (debug information removed) and compares with the committed `benchs/`. DUT configuration in [config/wardrv/](esw/compliance_act4/config/wardrv/) (UDB `wardrv.yaml`: I 2.1, Zicsr 2.0, Sm 1.13.0, MXLEN 32; `sail.json`, `test_config.yaml`, `link.ld`, `rvmodel_macros.h`). `docker/` builds an Ubuntu 24.04 image containing all the tools, so that the generation does not depend on the host distribution nor on the network: `make build` (network needed once), `make generate` (runs `make full` without network), `make save` / `make load` (keep the image in a `.tar.gz`); `ENGINE=docker` (default) or `ENGINE=podman` (rootless podman).

### Targets

| Target | Toplevel | Description |
|--------|----------|-------------|
| `default` | `sbi_WardRV_fsm` | HDL fileset only |
| `sim` | `tb_WardRV` | Base of the simulation targets, "DON'T RUN" (GHDL `-Wall -fsynopsys -frelaxed --no-vital-checks`, `--ieee-asserts=disable`) |
| `lint_ghdl` | `asylum.WardRV_wrapper` | Lint: GHDL `-Wall` analysis and elaboration (empty simulation) of [syn/WardRV_wrapper.vhd](syn/WardRV_wrapper.vhd) (`-Wall -fsynopsys -frelaxed --no-vital-checks`) |
| `lint_ghdl_synth_default` | `asylum.WardRV_wrapper` | Synthesis check (see below), word address on 32 bits (default generics: `IADDR_WIDTH = 30`, `IADDR_ALIGN_BITS = 2`) |
| `lint_ghdl_synth_byte_addr` | `asylum.WardRV_wrapper` | Synthesis check, byte address on 32 bits: `IADDR_WIDTH=32`, `IADDR_ALIGN_BITS=0` |
| `lint_ghdl_synth_picosoc` | `asylum.WardRV_wrapper` | Synthesis check, PicoSoC (word address, ROM of 1024 words, `addr_width` of `rvcc_gen`): `IADDR_WIDTH=10`, `IADDR_ALIGN_BITS=2` |
| `sim_basic` | `tb_WardRV` | `esw/testcase` firmware (`FIRMWARE_FILE=firmware.hex`, `VERBOSE=false`) |
| `sim_irq` | `tb_WardRV` | Machine external interrupt masked / unmasked by the CSR instruction during which it becomes pending (`FIRMWARE_FILE=irq/irq_fw.hex`) |
| `sim_bridge_imem2sbi` | `tb_imem2sbi` | `imem2sbi` fetching from `sbi_ram` (asynchronous and synchronous read) |
| `sim_dbg_jtag`, `sim_dbg_dm`, `sim_dbg_system` | `tb_WardRV_dbg_jtag`, `tb_WardRV_dbg_dm`, `tb_WardRV_debug` | Scenario `reset` of the debug testbenches (state after reset), and bases of their scenario targets: compile once, choose another scenario at run time (`--TEST=<scenario>`) |
| `sim_dbg_jtag_<scenario>` (4 targets) | `tb_WardRV_dbg_jtag` | `tap`, `dmi`, `busy`, and `slow_dmi` (`dmi` with a clk 40x slower than TCK) |
| `sim_dbg_dm_<scenario>` (9 targets) | `tb_WardRV_dbg_dm` | `dmcontrol`, `halt_resume`, `abstract`, `cmderr`, `autoexec`, `resethaltreq`, `sba`, `progbuf0` (`PROGBUF_SIZE=0`), `postincrement` (`AARPOSTINCREMENT_ENABLE=true`) |
| `sim_dbg_system_<scenario>` (17 targets) | `tb_WardRV_debug` | `FIRMWARE_FILE=debug/debug_fw.hex`; the 11 other scenarios of the system test, `exc_ebreak` and `exc_trigger` with `EBREAK_EXCEPTION=true`, `trigger_ge`, `trigger_lt`, `trigger_range_execute` and `trigger_range_store` with `TRIGGER_RANGE_ENABLE=true` |
| `dbg_openocd` | | Base of the co-simulation targets, "DON'T RUN" alone |
| `sim_dbg_openocd_pb<0\|2>_sba<0\|1>_tr<0\|2>` (8 targets), `sim_dbg_openocd_pb16_sba1_tr8` | `tb_WardRV_openocd` | OpenOCD + GDB co-simulation of a configuration: program buffer size, SBA, number of triggers; `pb16_sba1_tr8`: 16 words, 12 data registers, range triggers, `aarpostincrement`. Needs `openocd` and `gdb-multiarch` |
| `lint_ghdl_synth_ebreak_exception` | `asylum.WardRV_wrapper` | Synthesis check, `EBREAK_EXCEPTION` without debug unit |
| `lint_ghdl_synth_debug`, `_debug_pb0`, `_debug_trig2`, `_debug_sba` | `asylum.WardRV_wrapper` | Synthesis check with `WardRV_debug`: program buffer of 2 words / without program buffer / 2 triggers / System Bus Access |
| `lint_ghdl_synth_debug_full`, `_debug_large`, `_debug_max` | `asylum.WardRV_wrapper` | 2 triggers, SBA, `EBREAK_EXCEPTION`, `RESETHALTREQ_ENABLE` / 4 triggers, program buffer of 8 words, 12 data registers / program buffer of 16 words, 12 data registers, 8 triggers with ranges, SBA, `EBREAK_EXCEPTION`, `RESETHALTREQ_ENABLE`, `AARPOSTINCREMENT_ENABLE` |
| `sim_compliance_act4_<test>` (45 targets) | `tb_WardRV` | `FIRMWARE_FILE=act4/<test>.hex`, `TEST_ENV=ACT4`, `VERBOSE=false`; `<test>` = `i_<instruction>_00` (39 tests: the RV32I instructions, `fence` and `nop`) and `zicsr_<instruction>_00` (`csrrc`, `csrrci`, `csrrs`, `csrrsi`, `csrrw`, `csrrwi`) |
| `sim_compliance_act3_<test>` (39 targets) | `tb_WardRV` | `FIRMWARE_FILE=act3/<test>.hex`, `GOLDEN_FILE=act3/<test>.signature`, `SIGNATURE_FILE=signature.output`, `TEST_ENV=ACT3`, `VERBOSE=false`; `<test>` = `add_01`, `addi_01`, `and_01`, `andi_01`, `auipc_01`, `beq_01`, `bge_01`, `bgeu_01`, `blt_01`, `bltu_01`, `bne_01`, `fence_01`, `jal_01`, `jalr_01`, `lb_align_01`, `lbu_align_01`, `lh_align_01`, `lhu_align_01`, `lui_01`, `lw_align_01`, `misalign1_jalr_01`, `or_01`, `ori_01`, `sb_align_01`, `sh_align_01`, `sll_01`, `slli_01`, `slt_01`, `slti_01`, `sltiu_01`, `sltu_01`, `sra_01`, `srai_01`, `srl_01`, `srli_01`, `sub_01`, `sw_align_01`, `xor_01`, `xori_01` |

A `lint_ghdl_synth_*` target is `lint_ghdl` with the generics of its configuration (FuseSoC `parameters`) and a `pre_run` hook, `ghdl_synth.sh` of `asylum:utils:ghdl_synth`, which adds `include ghdl_synth.mk` to the Makefile generated by edalize. The run step (`make run EXTRA_OPTIONS=<generics>`) then runs, in the build directory:

1. `ghdl --synth` of the toplevel (rule `ghdl_synth` of `ghdl_synth.mk`), with the options of the target: `STD`, `ANALYZE_OPTIONS` (`-Wall -fsynopsys -frelaxed --no-vital-checks`), `TOPLEVEL_LIBS`, `TOPLEVEL` and the generics. It writes `WardRV_wrapper_synth.vhd` (netlist) and `WardRV_wrapper_synth.log`, prints the warnings, and fails on a synthesis error (an inferred latch is one);
2. `ghdl -r --no-run`: elaboration with the same generics, without simulation.

To add a configuration, add a target with its `parameters` (the generics are declared in the `parameters` section of `WardRV.core`).

The core parameters are `FIRMWARE_FILE`, `GOLDEN_FILE`, `SIGNATURE_FILE`, `TEST_ENV` and `VERBOSE` (generics), `TEST` and `CLK_PERIOD_NS` of the debug testbenches, and the debug options (`DEBUG_ENABLE`, `NB_TRIGGERS`, `EBREAK_EXCEPTION`, `RESETHALTREQ_ENABLE`, `TRIGGER_RANGE_ENABLE`, `PROGBUF_SIZE`, `DATA_COUNT`, `SBA_ENABLE`, `AARPOSTINCREMENT_ENABLE`), used by the debug testbenches and the wrapper. The ACT3 compliance targets set `SIGNATURE_FILE`, so a test passes only if it writes `tohost = 1` and its signature matches the reference (the `riscv-arch-test` programs always write `tohost = 1`; the result comes from the signature). The ACT4 tests check their results themselves and write PASS / FAIL at `0x20000000`. Each compliance target has its own fileset (`files_act3_<test>` / `files_act4_<test>`) so only its `.hex` (and `.signature`) is copied in the build directory.

### How to Run

The default tool is GHDL (`mk/defs.mk`: `TOOL ?= ghdl`, `TARGET ?= sim_basic`).

```bash
make help                          # variables, rules and target list (mk/targets.txt)
make sim_basic                     # run one target (log in log/)
make nonreg_sim                    # run every sim_* target
make TARGETS_FILTER=^sim_compliance_act4 nonreg_sim   # run a filtered subset
make lint_ghdl                     # GHDL -Wall lint
make lint_ghdl_synth_default       # GHDL -Wall lint + ghdl --synth -Wall check of one configuration
make nonreg_lint                   # run every lint_* target (lint_ghdl, lint_ghdl_synth_*)
make sim_dbg_system                # debug system test, scenario reset
make sim_dbg_system_step           # another scenario of the debug system test
make clean                         # remove build/ and log/
```

Equivalent FuseSoC command, and a scenario of a debug testbench chosen at run time (the build of the base target is reused):

```bash
fusesoc --cores-root . run --build-root build --target sim_compliance_act3_add_01 asylum:processor:WardRV:1.0.0
fusesoc --cores-root . run --build-root build --no-export --target sim_dbg_system asylum:processor:WardRV:1.0.0 --TEST=interrupt
```

The debug targets do not write a waveform (long simulations): add `--fst=dut.fst` to the `run_options` of the target to get one.

The firmware and reference files are committed, so simulation only needs GHDL and UVVM (`bitvis:verification:uvvm`). Rebuilding them needs a RISC-V toolchain (`RISCV_PREFIX ?= riscv64-unknown-elf-`, see `esw/testcase/Makefile` and `esw/irq/Makefile`); the compliance tests are regenerated with `make full` in `esw/compliance_act3` and `esw/compliance_act4`, which install their own pinned tools. Outside CI, GHDL writes a waveform `dut.fst`.

### OpenOCD + GDB co-simulation

GDB -> OpenOCD (riscv target, `remote_bitbang` adapter) -> TCP -> [sim/openocd/remote_bitbang.c](sim/openocd/remote_bitbang.c), called by `tb_WardRV_openocd` at each clock cycle through GHDL VHPIDIRECT ([rbb_pkg.vhd](sim/openocd/rbb_pkg.vhd)), TCK = clk / 2. The hooks of the targets ([cosim.sh](sim/openocd/cosim.sh), run in the work root) build `librbb.so` and the firmware (`pre_build`), start OpenOCD ([wardrv.cfg](sim/openocd/wardrv.cfg), `openocd.log`) and GDB ([scenario.py](sim/openocd/scenario.py), `gdb.log`) in the background (`pre_run`), then print the logs and fail unless the scenario passed (`post_run`); OpenOCD `shutdown` ends the simulation. The testbench writes its debug options in `cosim.cfg`; the scenario checks them against what OpenOCD finds and adapts (see [What GDB / OpenOCD can do](#what-gdb--openocd-can-do-in-each-configuration)).

Needs `openocd` and `gdb-multiarch` (with Python) in the `PATH`: validated with the Ubuntu 24.04 packages `openocd 0.12.0-1build2` and `gdb-multiarch 15.1-1ubuntu1~24.04.1` added to the `asylum-env` image. Wall-clock time of a target (setup, build and run, one at a time on the validation host): 36 to 41 s without memory access, 60 to 66 s with the SBA only, 93 to 106 s with the program buffer, 136 s for the maximum configuration; 12.3 min for the 9 targets one after the other (each one is its own CI job).

OpenOCD 0.12 limitations found: GDB single-steps with a software breakpoint (no `vContSupported`), so `stepi` / `next` need memory access; without memory access a watchpoint hit is reported as a SIGTRAP; after a refused memory write the next resume fails once ("Failed to read priv register"); a memory read through the SBA while the core runs prints "Failed to read priv register" but works.

## Synthesis

- `lint_ghdl` analyzes (`-Wall`) and elaborates (empty simulation) [syn/WardRV_wrapper.vhd](syn/WardRV_wrapper.vhd): `sbi_WardRV_fsm` with flat constrained ports (`sbi_WardRV_fsm` has unconstrained SBI ports and cannot be a synthesis top), word address of the whole 32-bit space by default (`IADDR_WIDTH = 30`, `IADDR_ALIGN_BITS = 2`), no debug. With `DEBUG_ENABLE`, the wrapper adds `WardRV_debug` connected as in a SoC: JTAG pins, System Bus Access on its own SBI port (`dmem2sbi`), `ndmreset_o` resetting the core.
- `lint_ghdl_synth_*` run `ghdl --synth -Wall` (GHDL 6.0.0, no vendor tool) on the same wrapper, one target per configuration (word address on 32 bits, byte address on 32 bits, PicoSoC, breakpoint exception, debug unit configurations, see [Targets](#targets)): the run step synthesizes the wrapper with the generics of the target (`WardRV_wrapper_synth.vhd/.log` in the work root, warnings printed, fails on a synthesis error or an inferred latch), then elaborates it (`ghdl -r --no-run`).
- The synthesizable core is `WardRV_fsm` / `sbi_WardRV_fsm`: plain RTL, register file in `ram_2r1w`, the execution trace process is enclosed in `synthesis translate_off / translate_on`. It is synthesized in the PicoSoC emulation targets (`emu_ng_medium_soc*_wardrv_fsm*` for NanoXplore NG-MEDIUM, `emu_basys_soc1_wardrv_fsm_c_identity` for Digilent Basys) of `asylum:soc:PicoSoC`.

## Design Notes

### FSM

| State | Action | Next |
|-------|--------|------|
| `S_RESET` | - | `S_FETCH` |
| `S_FETCH` | `imem` request at `pc`; ALU computes `pc + 4` (stored in `pc_seq_r`) | `S_DECODE` when `imem` ready |
| `S_DECODE` | Combinational decode of `inst_r` | `S_EXECUTE` |
| `S_EXECUTE` | ALU executes the instruction, result latched in `alu_res_r` (value, address, jump / branch target) | Debug (the instruction is abandoned): `S_TRAP` on a breakpoint exception, `S_FETCH` on a debug mode entry / `ebreak` / `dret` in debug mode; else `S_BRANCH_DECISION` (branch), `S_MEMORY` (load / store) or `S_WRITEBACK` |
| `S_BRANCH_DECISION` | ALU computes `rs1 - rs2`, branch condition latched (BEQ / BNE: zero, BLT / BGE: signed less-than `N xor V`, BLTU / BGEU: borrow) | `S_WRITEBACK` |
| `S_MEMORY` | `dmem` request at `alu_res_r` | `S_WRITEBACK` when `dmem` ready |
| `S_WRITEBACK` | Register write (ALU / memory / `pc + 4` / CSR), CSR write, PC update | `S_TRAP` if an interrupt is pending, else `S_FETCH` |
| `S_TRAP` | `pc <= mtvec`, `mepc <= pc` (next instruction), `mcause`, `mstatus` updated | `S_FETCH` |

Next PC: `mtvec` on trap, `mepc` on MRET, `alu_res_r` with bit 0..1 cleared for JAL / JALR / taken branches, else `pc + 4`. An instruction therefore takes at least four cycles (FETCH, DECODE, EXECUTE, WRITEBACK) plus the memory wait states.

### Implementation limits

- Only the external interrupt causes a trap (and EBREAK / an M-mode trigger with `EBREAK_EXCEPTION`): there is no exception for illegal instructions, ECALL or misaligned accesses; unknown instructions, FENCE, ECALL, WFI, EBREAK (without `EBREAK_EXCEPTION` nor `dcsr.ebreakm`) and DRET outside debug mode execute as NOPs.
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
├── .github/workflows/ci.yml    # CI: one job per sim_* target
├── doc/
│   └── WardRV.drawio           # Block diagram
├── hdl/
│   ├── RV_pkg.vhd
│   ├── WardRV_pkg.vhd
│   ├── WardRV_stats_pkg.vhd
│   ├── bridge/                 # dmem2sbi.vhd, imem2sbi.vhd
│   ├── debug/                  # WardRV_debug_pkg.vhd, WardRV_dbg_{tap,dtm,dmi_cdc,jtag,dm,dm_mem,sba}.vhd,
│   │                           # WardRV_dbg_rom_pkg.vhd (generated), WardRV_debug.vhd
│   ├── fsm/                    # WardRV_decode_pkg.vhd, WardRV_fsm_{fetch,alu,decode,regfile,memory,csr}.vhd,
│   │                           # WardRV_fsm_{debug,trigger,debug_bus}.vhd,
│   │                           # WardRV_fsm.vhd, sbi_WardRV_fsm.vhd, README.md (empty)
│   └── iss/                    # WardRV_iss_pkg.vhd, WardRV_iss.vhd
├── sim/
│   ├── tb_WardRV.vhd
│   ├── tb_WardRV_pkg.vhd
│   ├── tb_WardRV_iss.vhd
│   ├── tb_imem2sbi.vhd          # Bridge : imem2sbi + sbi_ram (sim_bridge_imem2sbi)
│   ├── tb_WardRV_dbg_jtag{,_pkg}.vhd  # Debug : JTAG unit test
│   ├── tb_WardRV_dbg_dm{,_pkg}.vhd    # Debug : Debug Module unit test
│   ├── tb_WardRV_debug{,_pkg}.vhd     # Debug : system test
│   ├── tb_WardRV_openocd.vhd          # Debug : OpenOCD + GDB co-simulation
│   ├── openocd/                # remote_bitbang.c, rbb_pkg.vhd, wardrv.cfg, scenario.py, cosim.sh
│   ├── WardRV_vips.vhd
│   └── save/tb_WardRV.vhd      # Old testbench (not in the core)
├── syn/
│   └── WardRV_wrapper.vhd      # Lint / synthesis check wrapper (lint_ghdl*)
├── esw/                        # FUSESOC_IGNORE
│   ├── testcase/               # start.S, main.c, link.ld, Makefile, firmware.hex/.lst
│   ├── irq/                    # irq.S + Makefile + irq_fw.hex/.lst (sim_irq)
│   ├── debug/                  # debug_fw.S + Makefile + debug_fw.hex/.lst (sim_dbg_system_*)
│   ├── debug_rom/              # debug_rom.S, rom_pkg.awk, Makefile, debug_rom.hex/.lst (-> hdl/debug/WardRV_dbg_rom_pkg.vhd)
│   ├── openocd/                # start.S, main.c, link.ld, Makefile (sim_dbg_openocd_*, built by the hook)
│   ├── compliance_act3/        # Makefile, config/wardrv/, docker/, benchs/ (39 x .hex/.lst/.signature)
│   └── compliance_act4/        # Makefile, config/wardrv/, docker/, benchs/ (45 x .hex/.lst)
```

## Dependencies

| Core | Used by (fileset) | Purpose |
|------|-------------------|---------|
| `asylum:utils:pkg` (`>=1.0.0`) | `files_hdl` | Common packages (`sbi_pkg`) |
| `asylum:component:ram` (`>=1.0.0`) | `files_hdl` | `ram_2r1w` register file (`ram_pkg`) |
| `asylum:target:techmap` (`>=1.0.0`) | `files_hdl` | `sync2dffrn` synchronizers of the debug unit (DMI CDC, hart reset) |
| `bitvis:verification:uvvm` | `files_sim` | UVVM utility library of the testbench |
| `asylum:utils:ghdl_synth` | `syn` | `ghdl_synth.sh` / `ghdl_synth.mk` (`pre_run` hook of `lint_ghdl_synth_*`) |
