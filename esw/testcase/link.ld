OUTPUT_ARCH( "riscv" )
ENTRY( _start )

MEMORY
{
  /* 128KB RAM at address 0x80000000  */
  ram (rwx) : ORIGIN = 0x80000000, LENGTH = 128K
}

SECTIONS
{
  . = 0x80000000;
  
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

  /* Definition of the stack top at the end of RAM */
  _stack_top = ORIGIN(ram) + LENGTH(ram);

  . = 0x80200000;
  .tohost : { *(.tohost) }

}
