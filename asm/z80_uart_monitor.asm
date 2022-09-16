; code found on github
; https://raw.githubusercontent.com/fiskabollen/z80Monitor/master/z80UARTMonitor.asm
; Simple monitor on UART
; adapted where needed
;
;  Current address is in HL
;  Display [nnnn] bb (A)
;          nnnn is current address, bb is hex byte, A is ASCII char
;  Input:
; <space> displays current byte
; [0-9,A-F] enters current address
; <enter> increments current address (loops through FFFF)
; <backspace> decrements current address (loops through 0000)
; l lists 16 locations, update current
; d dumps a grid of memory from current until keypress
; c copies memory: requesting from, to and length
; S (capital) enters set mode: hex input fills memory until <enter> or <ESC>
; X (capital) executes from current
; h <enter> display this help
; any errors dislpays '?'",0x0A,0x0D
;
; Memory Map is
; 0000-3FFF    16K ROM (probably though only 4k or 8k chip)
; 4000-7FFF space for 16K of memory (ROM or RAM)
; 8000-FFFF 32K RAM

include 'io.asm'

A_CR:       equ 0x0D
A_LF:       equ 0x0A        ; Line Feed ASCII
A_BS:       equ 0x08        ; Backspace
A_FF:       equ 0x0C
A_ESC:      equ 0x1B
A_DEL:      equ 0x7F

RAMTOP:     equ 0xFFFF    ;    RAM ends at 0xFFFF
TEMP:       equ RAMTOP    ;     Temporary storage byte
KDATA1:     equ TEMP-1    ;    keyed input for addresses
KDATA2:     equ KDATA1-1
BUFFER:     equ KDATA2-256    ; for building strings - 256 bytes
STACK:      equ BUFFER-1    ; then we have the stack
    
    ORG     0x0000
    LD      SP,STACK
init:
    call    sioa_init

    ld      b,'*'
    call    sioa_tx_char
    ld      b,'S'
    call    sioa_tx_char
    ld      b,'T'
    call    sioa_tx_char
    ld      b,'A'
    call    sioa_tx_char


    LD      HL,0000h
    jp  start

start:
; Output the startup text
    LD DE, TEXT0
    CALL otext
    
; Output the current location [nnnn] bb (A)
display:
; Turn on LED1 to show display loop
    CALL on1        ; turn on LED1 to show busy
    CALL dispadd    ; Display [nnnn]
    LD A, ' '
    CALL outchar
    CALL outchar
    LD A, (HL)
    CALL hexout
    LD A, ' '
    CALL outchar
    LD A, '('
    CALL outchar
    LD A, (HL)
    CALL outchar
    LD A, ')'
    CALL outchar
    CALL OUTCRLF
    
inloop:
    CALL inchar         ; wait for input
    LD BC, 0            ; C is used

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECT BASED ON INPUT CHAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    CP 0x20             ; <space>: display
    JP Z, display

    CP 0x0D             ; <CR>: increment and display
    JP NZ, L1
    INC HL
    JP display
L1: CP 0x7F            ; backspace: decrement and display
    JP NZ, L2
    DEC HL
    JP display
L2: CP 'h'              ; h: show help then display
    JP Z, start
    CP 'c'              ; c: copy memory
    JP Z, copy
    CP 'd'              ; d: dump until keypress
    JP Z, dump
    CP 'l'              ; l: list 16 locations
    JP Z, list
    CP 'S'              ; S: enter write mode (set)
    JP Z, set
    CP 'k'              ; k: bulk set memory
    JP Z, bulkset
    CP 't'              ; t: type ascii to memory
    JP Z, typemem
    CP 'X'              ; X: execute from current
    JP Z, exec
    CP 30h              ; test for hex digit
    JP C, notdig        ; < 0x30
    CP 47h            
    JP NC, notdig       ; >= 0x47
    CP 3Ah
    JP NC, T1           ; >= 0x3A
    JP digit
T1: CP 41h              ; AND
    JR C, notdig        ; < 0x41
digit:
    CALL fourcar        ; <hexdigit>: address entry
    JP display
notdig:
    LD A, '?'           ; no other commands, output '?'
    CALL outchar
    CALL OUTCRLF
    JP display

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SET
;;   output SET [aaaa] [nn] where nn is current contents
;;   call two character input to set (HL)
;;   increment HL
;;   repeat until <esc>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
set:
    LD DE, SETTXT
    CALL otext
    CALL dispadd
    LD A, ' '
    CALL outchar
    
    CALL twocar         ; two character input and set (HL)
    CALL OUTCRLF        ; new line
    LD A, B             ; B contains 0xFF if we aborted
    ; CP 0xFF
    JP NZ, setend       ; abort - go to display
    JP display    
setend:
    INC HL              ; else next address and loops
    JP set
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXECUTE
;;    execute from HL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
exec:
    LD DE, EXTXT        ; confirmation text
    CALL otext
    CALL dispadd
    CALL OUTCRLF
    
    CALL inchar
    
    CP 0x0D    ; FIX A_CR         ; <ret> we continue, else abort
    JP NZ, xabort    
    PUSH HL
    RET
xabort:
    JP display
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIST - LIST 16 LOCATIONS, SETTING HL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
list:
    LD C, 0xFF        ; Use C=0xFF to do one cycle of dump

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DUMP - dump memory from current location until keypress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dump:
    LD A, H
    CALL hexout
    LD A, L
    CALL hexout
    
    LD A, ' '
    CALL outchar
    CALL outchar

    LD B, 16
    LD IX, BUFFER           ; Build string of ASCII values at TEMP
loop16:    
    LD A, (HL)
    CALL hexout
    LD (IX+0),'.'           ; set it to dot and we'll overwrite if it's displayable
    CP 0x20                 ; displayable is >0x19 and <0x7f
    JP M, skip
    CP 0x7F
    JP P, skip
    LD (IX+0), A              ; replace with the ASCII code otherwise
skip:
    LD A, ' '
    CALL outchar
    INC HL
    INC IX
    DEC B
    LD A, 0
    CP B
    JP NZ, loop16
    
    ; Output the 8 ASCII chars at BUFFER
    ; Add a 0x80 on the end and use otext routine
    LD A, 0x80
    LD (BUFFER+16), A
    LD DE, BUFFER
       
    CALL otext
    halt
    CALL OUTCRLF
    

    LD A, C                 ; check if we were only doing one line
    CP 0xFF
    JP Z, display           ; C was 0xFF so stop at one cycle
    
    CALL chkchar            ; check if a key was pressed
    CP 0xFF
    JP NZ, display          ; a keypress: abort
    
    JP dump
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COPY from, to, length (all in hex)
;;    use BUFFER to store 'to' and 'from'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
copy:
    PUSH HL
    PUSH DE
    PUSH BC
    LD DE, CPTXT1           ; Copy: From
    CALL otext
    
    LD A, 0x30              ; start fourcar with [0000]
    CALL fourcar
    LD (BUFFER),HL
    LD DE, CPTXT2           ; To:
    CALL otext
    LD A, 0x30              ; start fourcar with [0000]
    CALL fourcar
    LD (BUFFER+2),HL
    LD DE, CPTXT3           ; Length:
    CALL otext
LD A, 0x30                  ; start fourcar with [0000]
    CALL fourcar
    LD B,H                  ; set up for eLDIR
    LD C,L
    LD DE, (BUFFER+2)
    LD HL, (BUFFER)
    CALL eLDIR
    
    LD DE, DONETXT          ; Done
    CALL otext
    POP BC
    POP DE
    POP HL
    JP display

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Four hex digit rotating input starting with contents of A
;;   exits on <ret> or <esc>
;;   HL contains the address input on return
;;   or HL remains unchanged on abort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fourcar:
        PUSH AF
        PUSH BC
        LD B,H             ; save original HL
        LD C,L
        ; First set HL to [000(digit)] to display
        CALL ATOHEX
        LD L, A
        LD H, 00h
        LD (KDATA2), A      ; start with the digit we were given
        LD A, 0
        LD (KDATA1), A
        ; Output [nnnn] then one backspace
        CALL dispadd
        LD A, 0x08          ;   A_BS
        CALL outchar
fcloop:
        ; Output 4 backspaces
        LD A, 0x08          ; 0x08
        CALL outchar
        CALL outchar
        CALL outchar
        CALL outchar
        
        CALL inchar
        CP 0x0D             ; <return>: end
        JP Z, fcend
        CP 0x1B             ; <escape>: abort
        JP NZ, fccont
        LD H,B              ; Abort - restore old value
        LD L,C
        JP fcabort
fccont:    CALL ATOHEX
        LD HL, KDATA2
        RLD
        LD HL, KDATA1
        RLD
        LD A, (KDATA1)
        CALL hexout
        LD A, (KDATA2)
        CALL hexout
        JP fcloop
        
fcend:    LD HL, (KDATA2)        ;Loads L then H
fcabort:
        CALL OUTCRLF
        POP BC
        POP AF
        RET    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TWO CHARACTER ROLLING INPUT ROUTINE, exits on <esc> or <ret>
;;   sets (HL) to A and returns
;;   on <esc> set (HL) to original value, write FF to A and return
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
twocar:
        PUSH HL
        ; Output [00] then one backspace
        LD A, '['
        CALL outchar
        LD A, '0'
        CALL outchar
        CALL outchar
        LD A, ']'
        CALL outchar
        LD A, 0x08          ; A_BS
        CALL outchar
        LD B, (HL)          ; save the old contents for <esc>
        LD HL, KDATA1
        LD (HL), 0
tcloop:
        ; Output 2 backspaces
        LD A, 0x08          ; A_BS
        CALL outchar
        CALL outchar

        CALL inchar
        CP 0x0D
        JP Z, tcend
        CP 0x1B
        JP Z, tcabort
        
        CALL ATOHEX
        RLD
        LD A, (HL)
        CALL hexout
        JP tcloop
        
tcabort:
        LD A, B        ; <esc>: so restore A
        LD (KDATA1), A
        LD B, 0xFF    ; Use 0xFF in B to indicate an abort
tcend:    POP HL
        LD A, (KDATA1)
        LD (HL), A    ; set (HL) to KDATA1
        RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; Display '[aaaa]' - address of HL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
dispadd:
        LD A, '['
        CALL outchar
        LD A, H
        CALL hexout
        LD A, L
        CALL hexout
        LD A, ']'
        CALL outchar
        RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
; OUTPUT VALUE OF A IN HEX ONE NYBBLE AT A TIME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
hexout: PUSH BC
        PUSH AF
        LD B, A
        ; Upper nybble
        SRL A
        SRL A
        SRL A
        SRL A
        CALL TOHEX
        CALL outchar
        
        ; Lower nybble
        LD A, B
        AND 0x0F
        CALL TOHEX
        CALL outchar
        
        POP AF
        POP BC
        RET
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
; TRANSLATE value in lower A TO 2 HEX CHAR CODES FOR DISPLAY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
TOHEX:
        PUSH HL
        PUSH DE
        LD D, 0
        LD E, A
        LD HL, DATA
        ADD HL,DE
        nop
        nop
        nop
        LD A, (HL)
        nop
        nop
        nop
        POP DE
        POP HL
        RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ASCII char code for 0-9,A-F in A to single hex digit
;;    subtract 0x30, if result > 9 then subtract 0x0x7 more
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ATOHEX:
        SUB 0x30
        CP 10
        RET M        ; If result negative it was 0-9 so we're done
        SUB 0x07    ; otherwise, subtract 0x7 more to get to 0x0A-0x0F
        RET        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eLDIR - LDIR but with confirmed writes
;;   HL=from, DE=to, BC=length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
eLDIR:
        PUSH AF
ldlp:    LD A, B            ; test BC for zero first
        OR C            ; stupid z80 doesn't flag after DEC xy
        JP Z, ldend
        LD A, (HL)
        PUSH HL
        LD H,D
        LD L,E
        CALL CONFWR        ; uses HL
        POP HL
        INC HL
        INC DE
        DEC BC
        JP ldlp
ldend:    POP AF
        RET        
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFWR - Write to address with confirm, returns when complete
;;          used for writign to EEPROM
;;  This will hang the computer if write does not succeed
;; byte to write is in A
;; address to write is HL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CONFWR:
        PUSH BC
        LD B, A
        LD (HL), A        ; write the byte
eeloop:    LD A, (HL)        ; read the byte
        CP B            ; the EEPROM puts inverse of the value
        JP NZ, eeloop    ; while it is writing
        POP BC
        RET    
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wait until UART has a byte, store it in A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
inchar:
        JP    sioa_rx_char          ; WILL FALL THROUGH AND RETURN
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If UART has a byte, store it in A else return 0xFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
chkchar:
        call    sioa_rx_ready
        JP      NZ, sioa_rx_char     ; WILL FALL THROUGH AND RETURN
        LD      A,0xFF
        RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output the byte in A to UART, wait until transmitted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
outchar:
        PUSH    AF
        PUSH    BC
        LD      B,A
        call    sioa_tx_char
        POP     BC
        POP     AF
        RET
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
; Output text pointed to by DE
;   loop through calling outchar until 0x80 is encountered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
otext:
        PUSH AF
otloop:    LD A, (DE)
        CP 0x80             ; 0x80 means end of text
        JP Z, otend        
        CALL outchar        ; output the byte in A
        INC DE              ; point to next
        JP otloop
otend:    POP AF
        RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; OUTCRLF - output a CR and an LF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
OUTCRLF:
        PUSH AF
        LD A, 0x0D
        CALL outchar
        LD A, 0x0A
        CALL outchar
        POP AF
        RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
; Toggle LEDs on the UART
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
toggle1:
        ; NOT IMPLEMENTED
        RET
toggle2:
        ; NOT IMPLEMENTED
        RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; Turn on or off LED 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
on1:
        ; NOT IMPLEMENTED
        RET

DATA:
        DEFB    30h    ; 0
        DEFB    31h    ; 1
        DEFB    32h    ; 2
        DEFB    33h    ; 3
        DEFB    34h    ; 4
        DEFB    35h    ; 5
        DEFB    36h    ; 6
        DEFB    37h    ; 7
        DEFB    38h    ; 8
        DEFB    39h    ; 9
        DEFB    41h    ; A
        DEFB    42h    ; B
        DEFB    43h    ; C
        DEFB    44h    ; D
        DEFB    45h    ; E
        DEFB    46h    ; F
    
TEXT0:
    DEFM    "Mon 0xRevision: 1.17 0x",0x0A,0x0D
    DEFM    "<spc>: display address",0x0A,0x0D
    DEFM    "[0-9A-F]: enter address (<esc> abort)",0x0A,0x0D
    DEFM    "<ent>: inc address, <bs>:dec address",0x0A,0x0D
    DEFM    "l: list+inc 16",0x0A,0x0D
    DEFM    "d: dump at address (any key ends)",0x0A,0x0D
    DEFM    "S: set at address (<ent>:set+inc <esc>:end)",0x0A,0x0D
    DEFM    "X: exec address (caution!)",0x0A,0x0D
    DEFM    "c: copy... (length=0 to abort)",0x0A,0x0D
    DEFM    "k: bulk set...",0x0A,0x0D
    DEFM    "t: type ascii to mem...",0x0A,0x0D
    DEFM    "h: this help",0x0A,0x0D
    DEFB    0x80

SETTXT:
    DEFM    "SET ",0x80
    
EXTXT:
    DEFM    "exec ",0x80
    
CPTXT1:
    DEFM    "copy from:",0x80
CPTXT2:
    DEFM    "to:", 0x80
CPTXT3:
    DEFM    "length:",0x80

DONETXT:
    DEFM    "Done.",0x0A,0x0D,0x80
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional routines
;; April 2015
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call address in HL
;; Works by putting 'display' on the stack
;; destroys DE
callhl:
    LD DE, EXTXT    ; confirmation text
    CALL otext
    CALL dispadd
    CALL OUTCRLF
    CALL inchar
    CP 0x0D            ; <ret> we continue, else abort
    JP NZ, xabort    ; xabort jumps to display
    
    LD DE, display
    PUSH DE
    PUSH HL
    RET


;; Bulk memory set, continuous entry
;; designed to take paste from clipboard
;; of continual hex stream
;; starts from HL until <esc>
bulkset:
    PUSH DE
    LD DE, bstxt
    CALL otext
    
    ; ask for address -> HL
    XOR A
    CALL fourcar
    
    LD DE, bstxt1
    CALL otext
    
bkdigit:    
    ; Digit 1
    CALL inchar
    CP 0x1B
    JR Z, bsabort
    CALL outchar    ; echo the character
    CALL ATOHEX        ; convert to binary
    RLD                ; move into (HL) lower nybble

    ; Digit 2
    CALL inchar
    CALL outchar    ; echo the character
    CALL ATOHEX        ; convert to binary
    RLD                ; shift (HL) and move into lower nybble
    
    INC HL
    JR     bkdigit
    
bsabort:
    LD DE, DONETXT
    CALL otext
    POP DE
    JP    display
bstxt:
    DEFM "Bulk load to: ",0x80
bstxt1:
    DEFM "Ready (<esc> to end): ",0x80
    
    
;; Type ascii values to memory, <esc> exits
typemem:
    PUSH DE
    LD DE, tmtxt
    CALL otext

    ; ask for address -> HL
    XOR A            ; zero A as first digit of fourchar
    CALL fourcar    ; set HL as per user entry

    LD DE, bstxt1
    CALL otext

tmloop:
    CALL inchar
    LD (HL), A
    INC HL
    CALL outchar
    CP 0x1B        ; escape
    JR NZ, tmloop

    LD H,D
    LD L,E
    POP DE
    JP display
tmtxt:
    DEFM "Type ascii to: ",0x80
    

;; Set memory range to value in A
;; From HL, length in BC
SETMEM:
    PUSH DE
    LD D, A
smloop:
    LD A, B        ; Test BC for zero first
    OR C
    JR Z, smend        
    LD A, D
    CALL CONFWR
    INC HL
    DEC BC
    JR smloop
smend:    
    LD DE, DONETXT
    CALL otext
    POP DE
    JP display

txt:    DEFM "Fin.",0x0D,0x0A,0x80

include 'sio.asm'
