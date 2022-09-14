    org     0x0000

stacktop:   equ 0xBABE   ; end of RAM + 1
    ld      sp,stacktop

    call    test_otir
    ld      b,'a'
    call    tx_char
    ld      b,'b'
    call    tx_char
    nop

    ld      b,0
loop:
    djnz loop


tx_char:
    ld      c,0x30
    out     (c),b
    ret

test_otir:
    ld  c, 0x30             ; serial port out
    ld  hl,sio_msg          ; start of byte sequence
    ld  b,sio_msg_len       ; number of bytes
    otir                    ; write B bytes from (HL) into port in the C reg
    ret

sio_msg:
    db      'B'
    db      'C'
    db      '5'
    db      '4'
    db      '7'
sio_msg_len:   equ $-sio_msg


    end