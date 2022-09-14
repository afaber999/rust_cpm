;****************************************************************************
;
;    Copyright (C) 2021 John Winans
;
;    This library is free software; you can redistribute it and/or
;    modify it under the terms of the GNU Lesser General Public
;    License as published by the Free Software Foundation; either
;    version 2.1 of the License, or (at your option) any later version.
;
;    This library is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public
;    License along with this library; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
;    USA
;
;****************************************************************************

; A proggie to test the SIO in polled mode.


do_hexdump: equ 1   ; set to 1 if want to see a hexdump of the code


include 'io.asm'

stacktop:   equ 0   ; end of RAM + 1


    ;###################################################
    ; STARTING HERE, WE ARE RUNNING FROM RAM
    ;###################################################

    ld      sp,stacktop

if do_hexdump
    ; wipe the stack region to make the subsequent dump easier to see
    ld      hl,0xff00
    ld      (hl),0xa5
    ld      de,0xff01
    ld      bc,0xff
    ldir
endif

    call    sioa_init
    call    siob_init

if do_hexdump
    ; dump the RAM copy of the text region 
    ld      hl,0
    ld      bc,_end
    ld      e,1
    call    hexdump

    ; dump the RAM stack region 
    ld      hl,0xff00
    ld      bc,0x100
    ld      e,1
    call    hexdump
endif

    call    helloa
;   call    spew_loop
    call    echo_loop
;   call    relay

halt_loop:
    halt
    jp      halt_loop


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
; Read from A and send to B and at the same time, 
; read from B and send to A.
;##############################################################
relay:
    call    sioa_rx_ready
    jp      z,relay_b       ; not ready? skip tx
    call    sioa_rx_char
    ld      b,a
    call    siob_tx_char

relay_b:
    call    siob_rx_ready
    jp      z,relay         ; not ready? skip tx
    call    siob_rx_char
    ld      b,a
    call    sioa_tx_char

    jp      relay


include 'sio.asm'

if do_hexdump
include 'hexdump.asm'
endif

_end:
