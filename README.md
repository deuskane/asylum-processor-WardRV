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

VLNV: `asylum:processor:WardRV:0.0.4`

## Table of Contents

1. [Introduction](#introduction)
2. [Block Diagram](#block-diagram)
3. [Top-Level](#top-level)
4. [HDL Modules](#hdl-modules)
5. [Register Map](#register-map)
6. [Verification](#verification)
7. [Synthesis](#synthesis)
8. [Design Notes](#design-notes)
9. [Directory Structure](#directory-structure)
10. [Dependencies](#dependencies)

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
- Verified with a self-checking C test, a directed Zicsr / compare corner-case test and the RISC-V architecture (compliance) tests: 39 ACT3 tests (riscv-arch-test 3.10.0) with signature comparison and 45 ACT4 self-checking tests (riscv-arch-test 4.1.0, I and Zicsr)

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
| `IADDR_ALIGN_BITS` | natural | `2` | Number of PC LSBs dropped on `iaddr_o` (2 = word address) |

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
| [hdl/debug/WardRV_debug_pkg.vhd](hdl/debug/WardRV_debug_pkg.vhd) | `WardRV_debug_pkg` | package | Debug request / response records, component `WardRV_debug` (not in the core fileset) |
| [hdl/debug/WardRV_debug.vhd](hdl/debug/WardRV_debug.vhd) | `WardRV_debug` | entity | JTAG TAP / debug transport (IDCODE, DTMCS, DMI, BYPASS) producing halt / resume / ndmreset requests (not in the core fileset, commented out in `files_hdl`) |

`hdl/fsm/README.md` is an empty file.

### WardRV_fsm

| Kind | Name | Type | Default / Description |
|------|------|------|-----------------------|
| generic | `HARTID` | std_logic_vector(31 downto 0) | `(others => '0')`, `mhartid` value |
| generic | `RESET_ADDR` | std_logic_vector(31 downto 0) | `(others => '0')`, reset PC |
| generic | `VERBOSE` | boolean | `true`, simulation trace in `exec_fsm.log` (not forwarded by `sbi_WardRV_fsm`, so the default applies) |
| in | `clk_i` | std_logic | Clock |
| in | `arst_b_i` | std_logic | Asynchronous reset, active low |
| out | `imem_ini_o` | imem_ini_t | Instruction request (`valid`, `addr` = PC) |
| in | `imem_tgt_i` | imem_tgt_t | Instruction response (`ready`, `inst`) |
| out | `dmem_ini_o` | dmem_ini_t | Data request (`valid`, `addr`, `wdata`, `we`, `be`) |
| in | `dmem_tgt_i` | dmem_tgt_t | Data response (`ready`, `rdata`, `err` unused) |
| in | `meip_i` | std_logic | Machine external interrupt pending |

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
| `WardRV_fsm_decode` | `inst_i`; `imm_i_o`, `imm_s_o`, `imm_b_o`, `imm_u_o`, `imm_j_o`, `imm_csr_o`; `rd_addr_o`, `rs1_addr_o`, `rs2_addr_o`, `rd_src_o`, `rd_we_o`, `rs1_re_o`, `rs2_re_o`; `alu_op_o`, `alu_src_a_sel_o`, `alu_src_b_sel_o`; `mem_req_o`, `mem_we_o`, `mem_be_o`, `mem_data_unsigned_o`; `is_branch_o`, `branch_use_flag_zero_o`, `branch_use_flag_carry_o`, `branch_use_flag_sign_o`, `branch_flag_is_set_o`, `pc_sel_o`; `csr_we_o`, `csr_re_o`, `csr_addr_o`; `inst_is_mret_o`, `inst_type_o` | Immediates, register addresses and control signals; `rd_we = 0` when `rd = x0` |
| `WardRV_fsm_alu` | `src_a_i`, `src_b_i`, `op_i` (`alu_op_t`), `res_o`, `zero_o`, `sign_o`, `carry_o` | ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND, CLR (`src_b and not src_a`, CSRRC / CSRRCI); carry = carry / borrow of ADD / SUB (unsigned less-than after SUB); sign = `res(31) xor overflow` after SUB (signed less-than, correct on signed overflow), `res(31)` otherwise |
| `WardRV_fsm_regfile` | `clk_i`, `arst_b_i`, `rs1_re_i`, `rs1_addr_i`, `rs1_rdata_o`, `rs2_re_i`, `rs2_addr_i`, `rs2_rdata_o`, `rd_addr_i`, `rd_wdata_i`, `rd_we_i` | `ram_2r1w` (WIDTH 32, DEPTH 32, asynchronous read); reads of `x0` return 0 |
| `WardRV_fsm_memory` | `clk_i`, `arst_b_i`, `dmem_valid_i`, `addr_i`, `wdata_i`, `we_i`, `be_i`, `data_unsigned_i`, `dmem_ready_o`, `dmem_rdata_r_o`, `dmem_ini_o`, `dmem_tgt_i` | Byte enables and write data shifted by `addr(1:0)`, read data shifted back, sign / zero extended and registered |
| `WardRV_fsm_csr` | `clk_i`, `arst_b_i`, `csr_addr_i`, `csr_we_i`, `csr_re_i`, `csr_wdata_i`, `csr_rdata_o`, `csr_mtvec_o`, `trap_i`, `trap_cause_i`, `trap_pc_i`, `trap_mtval_i`, `inst_is_mret_i`, `meip_i`, `trap_mirq_o` (generic `HARTID`) | CSR read mux / write, trap entry (`MPIE <= MIE`, `MIE <= 0`, `mepc`, `mcause`, `mtval`), MRET (`MIE <= MPIE`, `MPIE <= 1`), `trap_mirq = MEIP and MEIE and MIE` |

### WardRV_debug

Generic `IDCODE_VALUE : std_logic_vector(31 downto 0) := x"10000001"`. Ports: `jtag_ini_i : jtag_ini_t`, `jtag_tgt_o : jtag_tgt_t` (TCK domain), `clk_i`, `arst_b_i`, `dbg_req_o : dbg_req_t` (`halt`, `resume`, `ndmreset`), `dbg_rsp_i : dbg_rsp_t` (`halted`, `running`).

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

## Verification

### Testbenches

| File | DUT | Description |
|------|-----|-------------|
| [sim/tb_WardRV.vhd](sim/tb_WardRV.vhd) + [sim/tb_WardRV_pkg.vhd](sim/tb_WardRV_pkg.vhd) | `WardRV_fsm` (`MODEL = "FSM"`, default) or `WardRV_iss` (`MODEL = "ISS"`) | UVVM testbench, answers `imem` / `dmem` requests in one cycle (`HARTID = 0x900DC0DE`). `TEST_ENV = "ACT3"` (default): loads `FIRMWARE_FILE` (hex) in a memory at `0x80000000` (`RESET_ADDR`), ends when the program writes `tohost` (`0x80200000`): `1` = TEST PASSED, other = TEST FAILED. When `SIGNATURE_FILE` and `GOLDEN_FILE` are set (ACT3 compliance and directed targets), dumps the signature area (`0x80202104`..`0x80203000`) to `SIGNATURE_FILE` and compares it word by word with `GOLDEN_FILE` (`TB_ERROR` on the first mismatch). Timeout 500 us at 10 ns. `TEST_ENV = "ACT4"`: memory of 512 KB at `0x00004000`, the bytes written at `0x10000000` are printed (console), ends when the program writes `0x20000000`: `123456789` = TEST PASSED, other = TEST FAILED. Timeout 10 ms |
| [sim/tb_WardRV_iss.vhd](sim/tb_WardRV_iss.vhd) | `iss_t` (procedural) | Same memory model driving the ISS protected type directly from a process (no target uses it as toplevel) |
| [sim/WardRV_vips.vhd](sim/WardRV_vips.vhd) | - | VIP package: reset pulse, JTAG procedures (init, reset, shift, read IDCODE, write IR, DMI read / write) |
| sim/save/tb_WardRV.vhd | - | Older copy of `tb_WardRV` (`SIGNATURE_FILE := "signature.output"`), not in the core |

Software:

- [esw/testcase/](esw/testcase/): `start.S` + `main.c` (RV32I arithmetic, shifts, comparisons, immediates, loads / stores of every width, `check()` writing `tohost`), `link.ld` (128 KB at `0x80000000`), `Makefile` (`riscv64-unknown-elf-gcc`, `-march=rv32i_zicsr -mabi=ilp32`); the prebuilt `firmware.hex` / `firmware.lst` are committed.
- [esw/directed/](esw/directed/): directed test `csr_branch` (no toolchain needed): [gen_csr_branch.py](esw/directed/gen_csr_branch.py) is a small RV32I / Zicsr assembler plus a Python reference model that writes `csr_branch.hex`, `csr_branch.lst` and the expected signature `csr_branch.signature` (135 words): CSRRW / CSRRS / CSRRC / CSRRWI / CSRRSI / CSRRCI on `mscratch` (old value and new value, `rs1 = x0` / `zimm = 0` no-write cases), BEQ / BNE / BLT / BGE / BLTU / BGEU and SLT / SLTU on 12 operand pairs including signed-overflow cases (`0x80000000` vs `1`, `0x7FFFFFFF` vs `-1`, ...), a backward taken BLT, SLTI / SLTIU with sign-extended immediates. Same memory map as the compliance tests (signature at `0x80202104`, `tohost` at `0x80200000`).
- [esw/compliance_act3/](esw/compliance_act3/): ACT3 flow (riscv-arch-test 3.10.0, `rv32i_m/I`, `-march=rv32i`). `make full` downloads the pinned tools in `riscv-compliance-ws/` (xPack GCC 13.3.0-1 / binutils 2.42, Sail 0.13.1), builds the 39 tests into `.hex` / `.lst`, runs them on Sail (RV32I, [config/wardrv/sail_rv32i.json](esw/compliance_act3/config/wardrv/sail_rv32i.json)) to get the reference `.signature`, and compares everything with the committed `benchs/` (byte identical). DUT configuration in [config/wardrv/](esw/compliance_act3/config/wardrv/) (`link.ld`, `model_test.h`). `docker/` builds an Ubuntu 24.04 image containing all the tools, so that the generation does not depend on the host distribution nor on the network: `make build` (network needed once), `make generate` (runs `make full` without network), `make save` / `make load` (keep the image in a `.tar.gz`); `ENGINE=docker` (default) or `ENGINE=podman` (rootless podman).
- [esw/compliance_act4/](esw/compliance_act4/): ACT4 flow (riscv-arch-test 4.1.0, extensions `I,Zicsr`). `make full` downloads the pinned tools in `riscv-compliance-ws/` (xPack GCC 15.2.0-1 / binutils 2.45, Sail 0.13.1, mise for uv / Python / Ruby), generates the 45 self-checking ELFs, converts them into `.hex` / `.lst` (debug information removed) and compares with the committed `benchs/`. DUT configuration in [config/wardrv/](esw/compliance_act4/config/wardrv/) (UDB `wardrv.yaml`: I 2.1, Zicsr 2.0, Sm 1.13.0, MXLEN 32; `sail.json`, `test_config.yaml`, `link.ld`, `rvmodel_macros.h`). `docker/` builds an Ubuntu 24.04 image containing all the tools, so that the generation does not depend on the host distribution nor on the network: `make build` (network needed once), `make generate` (runs `make full` without network), `make save` / `make load` (keep the image in a `.tar.gz`); `ENGINE=docker` (default) or `ENGINE=podman` (rootless podman).

### Targets

| Target | Toplevel | Description |
|--------|----------|-------------|
| `default` | `sbi_WardRV_fsm` | HDL fileset only |
| `sim` | `tb_WardRV` | Base of the simulation targets, "DON'T RUN" (GHDL `-Wall -fsynopsys -frelaxed --no-vital-checks`, `--ieee-asserts=disable`) |
| `lint_nxmap` | `sbi_WardRV_fsm` | NanoXplore nxmap on NG-MEDIUM (`program: False`, flag `TARGET = NANOXPLORE_NG_MEDIUM`) |
| `sim_basic` | `tb_WardRV` | `esw/testcase` firmware (`FIRMWARE_FILE=firmware.hex`, `VERBOSE=false`) |
| `sim_directed_csr_branch` | `tb_WardRV` | `esw/directed` test (`FIRMWARE_FILE=directed/csr_branch.hex`, `GOLDEN_FILE=directed/csr_branch.signature`, `SIGNATURE_FILE=signature.output`): Zicsr and signed / unsigned compare corner cases |
| `sim_compliance_act4_<test>` (45 targets) | `tb_WardRV` | `FIRMWARE_FILE=act4/<test>.hex`, `TEST_ENV=ACT4`, `VERBOSE=false`; `<test>` = `i_<instruction>_00` (39 tests: the RV32I instructions, `fence` and `nop`) and `zicsr_<instruction>_00` (`csrrc`, `csrrci`, `csrrs`, `csrrsi`, `csrrw`, `csrrwi`) |
| `sim_compliance_act3_<test>` (39 targets) | `tb_WardRV` | `FIRMWARE_FILE=act3/<test>.hex`, `GOLDEN_FILE=act3/<test>.signature`, `SIGNATURE_FILE=signature.output`, `TEST_ENV=ACT3`, `VERBOSE=false`; `<test>` = `add_01`, `addi_01`, `and_01`, `andi_01`, `auipc_01`, `beq_01`, `bge_01`, `bgeu_01`, `blt_01`, `bltu_01`, `bne_01`, `fence_01`, `jal_01`, `jalr_01`, `lb_align_01`, `lbu_align_01`, `lh_align_01`, `lhu_align_01`, `lui_01`, `lw_align_01`, `misalign1_jalr_01`, `or_01`, `ori_01`, `sb_align_01`, `sh_align_01`, `sll_01`, `slli_01`, `slt_01`, `slti_01`, `sltiu_01`, `sltu_01`, `sra_01`, `srai_01`, `srl_01`, `srli_01`, `sub_01`, `sw_align_01`, `xor_01`, `xori_01` |

The core parameters are `FIRMWARE_FILE`, `GOLDEN_FILE`, `SIGNATURE_FILE`, `TEST_ENV` and `VERBOSE` (generics). The ACT3 compliance and directed targets set `SIGNATURE_FILE`, so a test passes only if it writes `tohost = 1` and its signature matches the reference (the `riscv-arch-test` programs always write `tohost = 1`; the result comes from the signature). The ACT4 tests check their results themselves and write PASS / FAIL at `0x20000000`. Each compliance target has its own fileset (`files_act3_<test>` / `files_act4_<test>`) so only its `.hex` (and `.signature`) is copied in the build directory.

### How to Run

The default tool is GHDL (`mk/defs.mk`: `TOOL ?= ghdl`, `TARGET ?= sim_basic`).

```bash
make help                          # variables, rules and target list (mk/targets.txt)
make sim_basic                     # run one target (log in log/)
make nonreg_sim                    # run every sim_* target
make TARGETS_FILTER=^sim_compliance_act4 nonreg_sim   # run a filtered subset
make nonreg_lint                   # run lint_nxmap
make clean                         # remove build/ and log/
```

Equivalent FuseSoC command:

```bash
fusesoc --cores-root . run --build-root build --target sim_compliance_act3_add_01 asylum:processor:WardRV:0.0.4
```

The firmware and reference files are committed (`esw/directed` is regenerated with `python3 esw/directed/gen_csr_branch.py`), so simulation only needs GHDL and UVVM (`bitvis:verification:uvvm`). Rebuilding them needs a RISC-V toolchain (`RISCV_PREFIX ?= riscv64-unknown-elf-`, see `esw/testcase/Makefile`); the compliance tests are regenerated with `make full` in `esw/compliance_act3` and `esw/compliance_act4`, which install their own pinned tools. Outside CI, GHDL writes a waveform `dut.fst`.

## Synthesis

- `lint_nxmap` runs NanoXplore nxmap for the NG-MEDIUM FPGA with the synthesizable `sbi_WardRV_fsm` as toplevel (the `files_hdl` fileset still contains the behavioural ISS and the simulation-only statistics package, which are not instantiated by the FSM core).
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
| `S_WRITEBACK` | Register write (ALU / memory / `pc + 4` / CSR), CSR write, PC update | `S_TRAP` if an interrupt is pending, else `S_FETCH` |
| `S_TRAP` | `pc <= mtvec`, `mepc <= pc` (next instruction), `mcause`, `mstatus` updated | `S_FETCH` |

Next PC: `mtvec` on trap, `mepc` on MRET, `alu_res_r` with bit 0..1 cleared for JAL / JALR / taken branches, else `pc + 4`. An instruction therefore takes at least four cycles (FETCH, DECODE, EXECUTE, WRITEBACK) plus the memory wait states.

### Implementation limits

- Only the external interrupt causes a trap: there is no exception for illegal instructions, ECALL / EBREAK, or misaligned accesses; unknown instructions, FENCE, ECALL, EBREAK and WFI execute as NOPs.
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
├── .github/workflows/ci.yml    # CI: sim_basic, sim_directed_csr_branch and sim_compliance_* jobs
├── doc/
│   └── WardRV.drawio           # Block diagram
├── hdl/
│   ├── RV_pkg.vhd
│   ├── WardRV_pkg.vhd
│   ├── WardRV_stats_pkg.vhd
│   ├── bridge/                 # dmem2sbi.vhd, imem2sbi.vhd
│   ├── debug/                  # WardRV_debug_pkg.vhd, WardRV_debug.vhd (not in the core)
│   ├── fsm/                    # WardRV_decode_pkg.vhd, WardRV_fsm_{fetch,alu,decode,regfile,memory,csr}.vhd,
│   │                           # WardRV_fsm.vhd, sbi_WardRV_fsm.vhd, README.md (empty)
│   └── iss/                    # WardRV_iss_pkg.vhd, WardRV_iss.vhd
├── sim/
│   ├── tb_WardRV.vhd
│   ├── tb_WardRV_pkg.vhd
│   ├── tb_WardRV_iss.vhd
│   ├── WardRV_vips.vhd
│   └── save/tb_WardRV.vhd      # Old testbench (not in the core)
├── esw/                        # FUSESOC_IGNORE
│   ├── testcase/               # start.S, main.c, link.ld, Makefile, firmware.hex/.lst
│   ├── directed/               # gen_csr_branch.py + csr_branch.hex/.lst/.signature
│   ├── compliance_act3/        # Makefile, config/wardrv/, docker/, benchs/ (39 x .hex/.lst/.signature)
│   └── compliance_act4/        # Makefile, config/wardrv/, docker/, benchs/ (45 x .hex/.lst)
```

## Dependencies

| Core | Used by (fileset) | Purpose |
|------|-------------------|---------|
| `asylum:utils:pkg` (`>=1.0.0`) | `files_hdl` | Common packages (`sbi_pkg`) |
| `asylum:component:ram` (`>=1.0.0`) | `files_hdl` | `ram_2r1w` register file (`ram_pkg`) |
| `bitvis:verification:uvvm` | `files_sim` | UVVM utility library of the testbench |
