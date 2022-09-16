include 'io.asm'

do_clear_stack: equ 1   ; set to 1 if want to clear the stack RAM area


stacktop:   equ 0   ; end of RAM + 1

    ;###################################################
    ; STARTING HERE, WE ARE RUNNING FROM RAM
    ;###################################################

    ld      sp,stacktop

if do_clear_stack
    ; wipe the stack region to make the subsequent dump easier to see
    ld      hl,0xFE00
    ld      (hl),0xA5
    ld      de,0xFE01
    ld      bc,0x01FF
    ldir
endif

    call    sioa_init
    call    do_hexdump

loop:
    nop
    jp      loop

do_hexdump:
    ; dump the RAM copy of the text region 
    ld      hl,0
    ld      bc,_end
    ld      e,1
    call    hexdump

    call    hexdump_crlf
    call    hexdump_crlf

    ; dump the RAM stack region 
    ld      hl,0xff00
    ld      bc,0x100
    ld      e,1
    call    hexdump

    call    hexdump_crlf

    ret


include 'sio.asm'
include 'hexdump.asm'

_end: