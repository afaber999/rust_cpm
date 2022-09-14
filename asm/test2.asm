	org 0000

    ld b,5
	ld b,0x00
	ld c,0x80

	ld A,'0'
    out (C),A

	ld b,0x00
loop:
	djnz loop
.text
	end

