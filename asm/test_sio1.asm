include 'io.asm'

stacktop:   equ 0   ; end of RAM + 1


    ;###################################################
    ; STARTING HERE, WE ARE RUNNING FROM RAM
    ;###################################################

    ld      sp,stacktop
    call    sioa_init
    call    helloa

loop:
    nop
    jp      loop

;##############################################################
; Print 'Hello' on SIO_A
;##############################################################
helloa:
    ld      b,'\r'
    call    sioa_tx_char
    ld      b,'\n'
    call    sioa_tx_char
    ld      b,'H'
    call    sioa_tx_char
    ld      b,'e'
    call    sioa_tx_char
    ld      b,'l'
    call    sioa_tx_char
    call    sioa_tx_char
    ld      b,'o'
    call    sioa_tx_char
    ld      b,'\r'
    call    sioa_tx_char
    ld      b,'\n'
    call    sioa_tx_char
    ret

include 'sio.asm'
