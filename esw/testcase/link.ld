OUTPUT_ARCH( "riscv" )
ENTRY( _start )

MEMORY
{
  /* 128KB RAM at address 0 */
  ram (rwx) : ORIGIN = 0x00000000, LENGTH = 128K
}

SECTIONS
{
  . = 0x00000000;
  
  .text : {
    *(.text.init) /* Startup code first */
    *(.text)      /* Rest of the code */
  } > ram

  .data : {
    *(.data)
  } > ram

  .bss : {
    *(.bss)
  } > ram

  . = 0x80001000;
  .tohost : { *(.tohost) }

}
