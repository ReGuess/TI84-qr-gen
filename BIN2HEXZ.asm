;#SECTION "MAIN", CODE

	org	userMem - 2
	db	$BB, $6D
Start:
	ld	hl, Str8name
	rst	rMOV9TOOP1
	rst	rFINDSYM
	ret	c
	ld	a, b
	or	a
	ret	nz
; EX   DE,HL
	ld	h, d
	ld	l, e
	ld	b, (hl)
	inc	hl
LOOP1:
	inc	hl
	ld	a, (hl)
	sub	48
	jr	z, SKIPROT
	ld	c, b
	dec	b
	push	af
	ld	a, b
	and	$03
	ld	b, a
	pop	af
LAZYLOOP:
	rlca
	djnz	LAZYLOOP
	ld	b, c
SKIPROT:
;	ADD  A,48	; Not yet
	ld	(hl), a
	djnz	LOOP1
	ld	c, b
	ex	de, hl
	ld	b, (hl)
	push	hl
	inc	hl
LOOP2:
	inc	hl
	ld	a, b
	dec	a
	and	3
	cp	3
	ld	a, (hl)
	jr	nz, TODO 	; Actual ToDo: give these labels better names
	ld	c, a
	ld	d, h
	ld	e, l
TODO:
	call	nz, TODO2
	ld	(de), a
	djnz	LOOP2
	pop	hl
	ld	d, h
	ld	e, l
	ld	b, (hl)
	srl	b
	srl	b
	inc	hl
LOOP3:
	inc	hl
	ld	a, (hl)
	add	a, 48
	cp	58
	jr	c, NOPLUS7
	add	a, 7
NOPLUS7:
	ld	(hl), a
	inc	hl
; LD   (HL),A		; These allow for the result to have the same length, but like "00005555"
	inc	hl
; LD   (HL),A
	inc	hl
; LD   (HL),A
; ; DEC  B
; ; DEC  B
; ; DEC  B
	djnz	LOOP3
	ex	de, hl
	ld	b, (hl)
	srl	b
	srl	b
	ld	(hl), b
	inc	hl
	inc	hl
	ld	d, h
	ld	e, l
	ld	c, 2
LOOP4:
	inc	c
	ldi
	inc	hl
	inc	hl
	inc	hl
	djnz	LOOP4

	ret


TODO2:
	add	a, c
	ld	c, a
	ret


Str8name:
	db	StrngObj, $AA, 7, 0, 0
