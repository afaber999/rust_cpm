include 'io.asm'

stacktop:   equ 0   ; end of RAM + 1


    ;###################################################
    ; STARTING HERE, WE ARE RUNNING FROM RAM
    ;###################################################

    ld      sp,stacktop
    call    sioa_init


    call    echo_loop

loop:
    nop
    jp      loop



;##############################################################
; Echo characters from SIO back after adding one.
;##############################################################
echo_loop:
    call    sioa_rx_char    ; get a character from the SIO
    ld      b,a
;   inc     b               ; add 1 (A becomes B, ...)
    call    sioa_tx_char    ; print the character
    jp      echo_loop

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
