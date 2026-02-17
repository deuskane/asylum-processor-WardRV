OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  /* Adresse de départ (Reset Vector). Modifiez si nécessaire (ex: 0x80000000) */
  . = 0x00000000;

  .text.init : { *(.text.init) }
  .text : { *(.text) }
  
  . = 0x10000;
  .data : { *(.data) }
  
  . = ALIGN(16);
  .data.begin : { *(.data.begin) }
  .data.end : { *(.data.end) }
  
  . = 0x80001000;
  .tohost : { *(.tohost) }
 
  
  .bss : { *(.bss) }
  
  _end = .;
}