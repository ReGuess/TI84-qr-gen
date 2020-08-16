;#SECTION "MAIN", CODE

.nolist
#include "ti83plus.inc"
.list

	.org	userMem-2
	.db	$BB, $6D
Start:
	xor	a
	ld	hl, appBackUpScreen
	ld	de, appBackUpScreen+1
	ld	(hl), a
	ld	bc, $0300
	ldir
; CALL LINE_COUNT
	call	test_rsencode
 ;CALL test_rsgp
; CALL test_pmxpa
; CALL test_gf_pow
;; CALL test_poly_add
; CALL test_poly_mult
; CALL test_gf_mult
; CALL test_poly_scale
	ret

;
test_rsencode:
	ld	a, 10 ;  17
; LD   HL,$08
; CALL disp_wrapper
; LD   BC,$08
	ld	b, 0
	ld	de, msg_in
	ld	hl,  HELLO_WORLD_1M ; DATA0
	ld	c, (hl)
; EX   DE,HL
	inc	bc
	ldir
	ld	hl, msg_in
; JP   DISP_POLY
	call	rs_encode_msg
	ld	hl, msg_out
	jp	nc, DISP_POLY
	ret

;
test_rsgp:
	ld	a, 10
	ld	de, PLYMLTANS
	call	rs_generator_poly
	ld	hl, PLYMLTANS
	jp	DISP_POLY

;
test_pmxpa:
	ld	a, 3
; LD   HL,init_g
	ld	hl, DATA6
	ld	de, PLYMLTANS
; CALL poly_mult_1x_plus_A
	call	pmult_x_plus_A_HI
; RET
	ld	hl, PLYMLTANS
	jp	DISP_POLY

;
;
test_gf_pow:
	ld	hl, $0245
	call	gf_pow_noLUT
	ld	h, l
	ld	l, d
	B_CALL(	_DispHL)
	B_CALL(	_NewLine)
	ld	b, $45
	call	gf_exp2_noLUT
	ld	h, b
	ld	l, a
	B_CALL(_DispHL)
	ret

;
test_poly_mult:
	ld	hl, DATA4
	ld	de, DATA5
	ld	bc, PLYMLTANS
	call	poly_mult
	ld	hl, PLYMLTANS
	jp	DISP_POLY

; CALL DISP_POLY
; RET
;
;
;test_poly_add:
;	ld	hl, DATA1
;	ld	de, DATA2
;	ld	bc, PLYMLTANS
;	call	poly_add
;	ld	hl, PLYMLTANS
;	jp	DISP_POLY

; CALL DISP_POLY
; RET
;
;
;
test_gf_mult:
	ld	de, $0280
	call	gf_mult_noLUT
	ld	h, 0
	ld	l, c
	B_CALL(_DispHL)
	ret

;
test_poly_scale:
	ld	hl, DATA1
	ld	de, PLYMLTANS
	ld	a, 4
	call	poly_scale
	B_CALL(_DispHL)
	B_CALL(_NewLine)
	ld	hl, PLYMLTANS
	B_CALL(_DispHL)
	B_CALL	(_NewLine)
	ld	hl, PLYMLTANS
; CALL DISP_POLY
; RET
;
DISP_POLY:
	ld	b, (hl)
	ld	a, 200
	inc	b
	cp	b
	jr	c, nope
	dec	hl
	ld c,0
LBL01:
	inc	hl
	push hl
	push bc
	ld	l, (hl)
	ld	h, 0
	bcall(_DispHL)
	call	wait
	pop bc
	bit 0,c
	call z,print_a_space
	call nz,print_newline
	pop	hl
	inc	c
	djnz	LBL01
	ret
print_a_space:
	ld a, ' '
	bcall(_PutC)	; destroys none
	ret
print_newline:
	push bc
	bcall(_NewLine)
	pop bc
	ret

nope:
; LD   L,B
; LD   H,0
	B_CALL(_DispHL)
	ret

;#SECTION "PLYSCALE", CODE

; INPUTS:
; A: scalar multiplicand
; HL: ptr to size byte of polynomial multiplicand
; DE: Ptr to where to put result
;
; DESTROYED: ALL
; REMARKS: ...
poly_scale:
	ld	b, 0
	ld	c, (hl)
	inc	c
	push	de
	ldir
	pop	hl
	ld	b, (hl)
	inc	b
poly_scale_lbl1:
	inc	hl
	ld	d, (hl)
	ld	e, a
	push	bc
	call	gf_mult_noLUT
	ld	a, e
	ld	(hl), c
	pop	bc
	djnz	poly_scale_lbl1
	ret

;#SECTION "QRDATA", DATA

;TEMPBC .equ	saveSScreen
TEMPBC .equ appBackUpScreen	;;;;;;
;
;
PLYMLTANS .equ	appBackUpScreen+2	;;;;;
; PMLTTMP = PLYMLTANS+256
; PMLTTMP2 = PMLTTMP+256
PMLTTMP .equ	PLYMLTANS + 64; 128
PMLTTMP2 .equ	PMLTTMP + 64;128
;
GENTMP .equ	PMLTTMP2 + 64;128
; GENTMP = saveSScreen+8
GENOUT .equ	GENTMP ; + 32 ; 16 ;64;128
; no longer need GENTMP, since rs_g_poly now uses in-place multiplication
;
;
HELLO_WORLD_1M:
.db 16, 32, 91, 11, 120, 209, 114, 220, 77, 67, 64, 236, 17, 236, 17, 236, 17, 0,0,0,0,0,0,0,0
;.db 16
;.db %00100000, %01011011, %00001011, %01111000, %11010001, %01110010, %11011100
;.db %01001101, %01000011, %01000000, %11101100, %00010001, %11101100, %00010001
;.db %11101100, %00010001

DATA:  ; uses the others as a single 30-byte string
	.db	30
DATA0:
	.db	7, 8, 6, 7, 5, 3, 0, 9
DATA1:
	.db	4, 1, 2, 3, 4
DATA2:
	.db	4, 1, 2, 3, 0
DATA3:
	.db	3, 3, 2, 1
DATA4:
	.db	1, 1
DATA5:
	.db	2, 1, 1
DATA6:
	.db	2, 1, 2
;#SECTION "GFMULT", CODE

; INPUTS: D,E: Multiplicands
; OUTPUT: C : Product
; DESTROYED:  A, B, D
; Note: B=0, and A=D=junk
; $1C == lsb($011D) ^ $01
gf_mult_noLUT:
	ld	bc, $0800
LBL1:
	xor	a
	rrc	e		; using rrc means we can preserve E
	sbc	a, a	; A = E.carry ? $ff : $00
	and	d		; A = E.carry ? D : $00
	xor	c		; A = E.carry ? D^C : C
	ld	c, a
	ld	a, d
	rlca
	jr	nc, LBL2
	xor	$1C
LBL2:
	ld	d, a
	djnz	LBL1
	ret

;
; Input: A
; Output: A = 2*A
; Destroyed: None
; This is for reference only,
; and should be inlined IRL
gf_mult_by_2:
	rlca
	ret	nc
	xor	$1C
	ret

;#SECTION "GFPOW", CODE

; INPUTS:
;     H: Base; L: Exponent
; OUTPUTS:
;   D=H**L
; DESTROYED: A,BC,E,L
; Remarks:
; E=H
; C=D if exponent>0
; L=0
; z flag set
gf_pow_noLUT:
	ld	a, l
	or	a
	ld	d, 1
	ret	z
	ld	e, h
gfpow_lbl1:
; Remember:
; D*E->C;  A,B,D destroyed
	call	gf_mult_noLUT
	ld	d, c
	dec	l
	jr	nz, gfpow_lbl1
	ret

;
;
; INPUTS:
;   B: Exponent
; OUTPUTS:
;   A=2**B
; DESTROYED: B
; Remarks: B=0
gf_exp2_noLUT:
	xor	a
	cp	b
	ld	a, 1
	ret	z
gf_exp2_lbl1:
; CALL gf_mult_by_2
; Inlined below:
	rlca
	jr	nc, $ + 4
	xor	$1C
	djnz	gf_exp2_lbl1
	ret

;#SECTION "POLYADD", CODE
;;; Not needed?
;; INPUTS:
;;  DE, HL: Ptrs to plyn addends
;;  BC:  Ptr to polynoial sum
;; OUTPUT:
;; DESTROYED: ALL
;poly_add:
;	ld	a, (de)
;	cp	(hl)
;	jr	nc, paddlbl1
;; if (DE)<(HL)
;	ex	de, hl
;	ld	a, (de)
;paddlbl1:
;; assert  A=(DE) >= (HL)
;	ld	(bc), a
;	push	hl
;; PUSH DE
;	push	bc
;; DE -> HL
;	ex	de, hl
;	ld	d, b
;	ld	e, c
;	ld	b, 0
;	ld	c, a
;	inc	de
;	inc	hl
;	ldir
;
;	pop	de
;	pop	hl
;	ld	b, (hl)
;paddlbl2:
;	inc	hl
;	inc	de
;	ld	a, (de)
;	xor	(hl)
;	ld	(de), a
;	djnz	paddlbl2
;	ret


;#SECTION "POLYMULT", CODE

; INPUTS:
;  DE, HL: Ptrs to plyn mltiplcnds
;  BC:  Ptr to polynoial product
poly_mult:
; LD   (TEMPBC+2),DE
; LD   (TEMPBC+4),HL
	ld	a, (de)
	add	a, (hl)
	dec	a
	ld	(bc), a
; (BC) = (DE)+(HL)-1
	push	de
	push	hl
	ld	(TEMPBC), bc
; Set Result Area to 0
	inc	bc
	ld	h, b
	ld	l, c
	ld	d, b
	ld	e, c
	inc	de
	dec	a
	ld	c, a
	xor	a
	ld	b, a
	ld	(hl), a
	ldir
;
	pop	hl
	pop	de
	ld	a, (de)
	ld	b, a
pmltLbl1:
; Loops over "C"
	ld	c, b
	ld	b, (hl)
pmltLbl2:
; More comfy w/ this than w/ scewing up SP
; LD   (TEMPBC),BC
	push	de
	push	hl
	push	bc
;
	ld	a, b
	ld	b, 0
	ex	de, hl
	add	hl, bc
	ld	c, a
	ex	de, hl
	add	hl, bc
;
	ld	a, (de)
	ld	e, (hl)
	ld	d, a
	call	gf_mult_noLUT
; C = Product=value to XOR
	pop	de
; the counters
	push	de
	ld	a, d
	add	a, e
	dec	a
	ld	de, (TEMPBC)
	add	a, e
	ld	e, a
	adc	a, d
	sub	e
	ld	d, a
;
	ld	a, c
	ex	de, hl
	xor	(hl)
	ld	(hl), a
; EX   DE,HL
; gets overwritten by POP anyway
	pop	bc
	pop	hl
	pop	de
	djnz	pmltLbl2
	ld	b, c
	djnz	pmltLbl1
	ret

;#SECTION "PMUL1XPA", CODE

; INPUTS:
; HL: Ptr to 1st multiplicand
; A: Const term of 2nd multiplicand
; DE: Ptr to Output
; DESTROYED: ALL
; NOTE: Lowest-degree
;   terms come 1st
poly_mult_1x_plus_A:
; Mult by A
	push	hl
; NEXT LINE IS FOR SWITCHIG COEFF ORDER
; INC  DE
;
	push	de
	call	poly_scale
; might need INC  HL here?
; B=0
	ld	(hl), b
;
; Mult by 1X
	pop	hl
; LD   (HL),B ; DEC  HL
; size byte of DEstination
	inc	(hl)
	ex	de, hl
	pop	hl
;
	ld	b, (hl)
; This would be the perfect place for a macro
	inc	de
pmxpa_lbl1:
	inc	de
	inc	hl
	ld	a, (de)
; CALL disp_wrapper
	xor	(hl)
	ld	(de), a
	djnz	pmxpa_lbl1
	ret

;#SECTION "XPLSAREV", CODE

; INPUTS:
; HL: Ptr to 1st multiplicand
; A: Const term of 2nd multiplicand
; DE: Ptr to Output
; DESTROYED: ALL
; NOTE: Highest-degree terms come 1st
pmult_x_plus_A_HI:
	push	hl
	inc	de
	push	de
	call	poly_scale
; LD   (HL),B
	pop	hl
; CALL disp_wrapper
	ld	c, (hl)
	ld	(hl), b
	dec	hl
	inc	c
	ld	(hl), c
; INC  (HL)
	ex	de, hl
	pop	hl
	ld	b, (hl)
xpa_rev_lbl1:
	inc	de
	inc	hl
	ld	a, (de)
	xor	(hl)
	ld	(de), a
	djnz	xpa_rev_lbl1
	ret

;#SECTION "RSGENPLY", CODE

; INPUTS:
; A: nsym
; DE: Ptr to output
; DESTROYED: all except DE ?
; Also, I think C=nsym
rs_generator_poly:	; OBSOLETE?
	JP rs_g_poly	; jump directly to new version
	;
	or	a
	jp	z, A_was_0	; honestly, this should be tested before the routine is called.
; PUSH DE
; LD   HL,init_g
	ld	hl, $0101	; first step only needs 2 bytes: 1 for count, 1 for data
	ld	(GENTMP), hl
	ld	hl, GENTMP
; LD   HL,PMLTTMP
	ld	c, 0
	ld	b, a
rsgp_lbl1:
	push	bc
	push	de
	push	hl
	ld	b, c
	call	gf_exp2_noLUT
; CALL poly_mult_1x_plus_A
	call	pmult_x_plus_A_HI
	; move result from output space to input space
	pop	hl
	pop	de
; EASIER BUT PROLLY SLOWER
	push	de
	push	hl
	ex	de, hl
; TODO: clean this up
	ld	c, (hl)
	ld	b, 0
	inc	bc
	ldir
	pop	hl
	pop	de
	pop	bc
	inc	c
	djnz	rsgp_lbl1
	ret

; This could prolly be done w/
; a parity check on C instead
; (Or just put the exchange inside the loop :p)
; POP  BC
; LD   A,D
; CP   B
; JR   NZ,rsgp_lbl2
; LD   A,E
; CP   C
; RET  Z
; rsgp_lbl2:
; We had an [even/odd] number of iterations,
; So we need to put the result where it's expected
; [BC=HL, not DE]
; PUSH BC
; POP  DE
; LD   C,(HL)
; LD   B,0
; INC  BC
; LDIR
;
; RET
;
A_was_0:
	inc	a
	ld	(de), a
	inc	de
	ld	(de), a
	xor	a
	ld	b, a
	ld	c, a
; In case the effect is wanted
	ret

;
init_g:
	.db	1, 1
; rs_gen_minibuffer:
; DB   2,1,1
;#SECTION "RSENCODE", CODE

; INPUTS:
;  A: nsym (number of symbols)
;  HL: Ptr to msg_in
; Actually, gonna use a name
; TODO:RENAME THESE TO MAKE MORE SENSE
msg_in .equ	PLYMLTANS
; gen_ptr = PMLTTMP2
msg_out .equ	PMLTTMP2
gen_ptr .equ	GENOUT
rs_encode_msg:
	or	a
	scf
	ret	z
;
	push	af
	ld	hl, msg_in
	ld	b, 0
	add	a, (hl)
	ld	c, a
	jp	c, msg_too_long
	
	ld	(msg_out), a
	ld	de, msg_out + 1
	inc	hl
	ldir
; LD   A,B
; PUSH HL
	pop	af
	ld	de, gen_ptr
	CALL rs_g_poly
	;call	rs_generator_poly
; B=0, C=nsym(?)

;
; LD   A,C
; ; PUSH BC ; POP  HL ; BCALL DispHL
; ; POP  HL
; ; PUSH HL
; LD   HL,msg_in
; LD   C,(HL)
; INC  C
; ; or INC BC? What if C=255?
; ;   i'd think if C=255, and A+255 didnt carry, then we've got a
; ; LD   DE,PMLTTMP
; LD   DE,msg_out
; LDIR
; ; Pad w/ 0s
; ; B=C=0
; LD   (HL),B
; LD   D,H
; LD   E,L
; INC  DE
; LD   C,A
; ; DEC  C   ;?
; LDIR
;
; ; EX   DE,HL
; ::...
	ld	a, (msg_in)
 	;DEC  A ;??

	ld	b, a
	ld	de, msg_out
rsenc_lbl1:
	inc	de
	ld	a, (de)
	or	a
	jr	z, rsenc_lbl3
	push	de
	ld	c, b
	ld	hl, gen_ptr
	ld	b, (hl)
; DJNZ LMAO
; ::TODO: Error catching ::
; Or at least display "Error"
; (nsym should nvr be small enough for this to matter)
	dec	b
; LMAO:
	inc	hl
rsenc_lbl2:
	push	bc
	inc	de
	inc	hl
	push	hl
	ld	h, (hl)
	ld	l, a
	ex	de, hl
	call	gf_mult_noLUT
	ld	a, c
	xor	(hl)
	ld	(hl), a
	ld	a, e
	ex	de, hl
	pop	hl
	pop	bc
	djnz	rsenc_lbl2
	ld	b, c
	pop	de
rsenc_lbl3:
	djnz	rsenc_lbl1
	ld	de, msg_out + 1
	ld	hl, msg_in
	ld	c, (hl)
	inc	hl
; EX   DE,HL
; INC  BC;;;;;?????
	;ldir	;;;;;;;!!!!!!!!! put this back  ; or take it out to see the quotient
; POP  AF
; ; DEC  A;;;;;?????
; LD   HL,msg_out
; ADD  A,(HL)
; LD   (HL),A
	or	a
	ret

;
;
msg_too_long:
; TODO
	pop	hl
	call	disp_wrapper
	scf
	ret

;WTF:
;	ld	h, 0
;	ld	l, b
;	pop	af
;	B_CALL(_DispHL)
;	scf
;	ret

;#SECTION "DEBUG", CODE

wait:
; RET
	ld	c, $45
	in	a, (c)
waitLoop:
	in	b, (c)
	cp	b
	jr	z, waitLoop
	ret

;
disp_wrapper:
	push	af
	push	bc
	push	de
	push	hl
	B_CALL(_DispHL)
	call	wait
	ld a, ' ' \ bcall(_PutC)
	;B_CALL(_NewLine)
	pop	hl
	pop	de
	pop	bc
	pop	af
	ret

;
;
;#SECTION "SCRATCH", DATA

; PUSH BC
; PUSH AF
; INC  SP
; POP  BC
; ; A0->C, C0->B
; DEC  SP
; POP  AF
; ; B0->A
;
; PUSH AF ; LD   A,C ; LD   C,B ; LD   B,A ; POP  AF
;;;;;;;;;;:::::::::::::

; INPUTS:
; A: nsym
; DE: Ptr to output
; DESTROYED: all except DE ?
; Also, I think C=nsym
rs_g_poly:
	ld b,a
	ld a,1
	ld (de),a
	inc de
	ld (de),a
	dec de
	ld a,b
	or	a
	ret z	;jp	z, A_was_0
	ex de,hl
	ld e,1
rsgp_lbl1a:
	push bc
; INPUTS:
;		E: constant term in the monic binomial multiplicand (1x + E)
;		HL: pointer to size byte of polynomial (call it g(x))
; OUTPUTS: At (HL), the product (x+E)*g(x), which is 1 degree greater than g(x).
; DESTROYED: A, BC, D. Additionally, since this algorithm is in-place, 
;					   it overwrites the input polynomial at (HL).
; Highest-degree terms are first. (otherwise interpret this as (E*x+1)*g(x)?)
inplace_pmult_x_plus_e:	; originally 1x + A
	inc (hl)		; add 1 to the size of the polynomial
	ld b,0
	ld c,(hl)
	add hl,bc	; HL now pts to 1 past the const term in the existing polynomial
	ld (hl),b	; b == 0
	dec c   	; don't use the size byte as a coefficient
	ld b,c
lbl_whatever:
	dec hl
	push bc
	ld d,(hl)
	call gf_mult_noLUT	; C holds product; E preserved, but A=D=junk and B=0
	ld a,c
	inc hl
	xor (hl)
	ld (hl),a
	dec hl
	pop bc
	djnz lbl_whatever
	dec hl		; it preserves hl... YES!
	;ret
	; E *= 2
	ld a,e
	rlca
	jr	nc, $ + 4
	xor	$1C
	ld e,a	
	pop bc
	djnz rsgp_lbl1a
	ex de,hl
	ret
;

