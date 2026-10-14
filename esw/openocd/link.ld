/*-----------------------------------------------------------------------------
 * WardRV OpenOCD / GDB co-simulation firmware : linker script
 * 64 KB RAM at 0x80000000 (RESET_ADDR, C_RAM_SIZE of sim/tb_WardRV_openocd.vhd)
 *---------------------------------------------------------------------------*/
OUTPUT_ARCH( "riscv" )
ENTRY( _start )

MEMORY
{
  ram (rwx) : ORIGIN = 0x80000000, LENGTH = 64K
}

SECTIONS
{
  .text : {
    *(.text.init)               /* Startup code first (reset vector) */
    *(.text .text.*)
  } > ram

  .rodata : ALIGN(4) {
    *(.rodata .rodata.* .srodata .srodata.*)
  } > ram

  .data : ALIGN(4) {
    *(.data .data.* .sdata .sdata.*)
    . = ALIGN(4);
  } > ram

  .bss (NOLOAD) : ALIGN(4) {
    _bss_start = .;
    *(.bss .bss.* .sbss .sbss.* COMMON)
    . = ALIGN(4);
    _bss_end = .;
  } > ram

  /* Stack at the end of the RAM */
  _stack_top = ORIGIN(ram) + LENGTH(ram);
}
