include 'io.asm'

stacktop:   equ 0   ; end of RAM + 1


    ;###################################################
    ; STARTING HERE, WE ARE RUNNING FROM RAM
    ;###################################################

    ld      sp,stacktop
    call    sioa_init
    call    spew_loop

loop:
    nop
    jp      loop

;##############################################################
; Print all printable characters in an endless loop on SIO A
;##############################################################
spew_loop:
    ld      b,0x20      ; ascii space character
spew_loop1:
    call    sioa_tx_char
    inc     b
    ld      a,0x7f      ; last graphic character + 1
    cp      b
    jp      nz,spew_loop1

    ; start a new line
    ld      b,'\r'
    call    sioa_tx_char
    ld      b,'\n'
    call    sioa_tx_char

    jp      spew_loop


include 'sio.asm'
