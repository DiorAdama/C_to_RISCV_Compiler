OUTPUT_ARCH( "riscv" )
ENTRY( _start )
heap_size = 0x2000;
SECTIONS
{
/* text: test code section */
. = 0x200;
.text :
{
*(.text)
}
. = 0x8000000;
/* data: Initialized data segment */
.data :
{
. = ALIGN(8);
_heap_start = .;
. = . + heap_size;
_heap_end = .;
*(.data)
}
/* End of uninitalized data segement */
_end = .;
}
