.nolist
#include "ti83plus.inc"
.list

;#SECTION "MAIN", CODE

	.org	userMem - 2
	.db	$BB, $6D
Start:
; TODO: CASE V>9
; TODO: Get V instead of Ans
	bcall(_RclAns)
	bcall(_ConvOP1)
	push	de

; EC LVL
	bcall(_RclX)
	bcall(_ConvOP1)
	pop	hl
; Since V < 12 < 256,Dont need DEC HL
	dec	l
	rlc	l
	rlc	l
	dec	a
	add	a, l
	ld	l, a
	ld	bc, ECCTBL
	add	hl, bc
	ld	e, (hl)

	ex	de, hl
	bcall(_SetXXXXOP2)
	bcall(_OP2ToOP1)
	bcall(_StoTheta)

	ret

;#SECTION "TABLE", DATA

ECCTBL:
	.db	19, 16, 13, 9
	.db	34, 28, 22, 16
	.db	55, 44, 34, 26
	.db	80, 64, 48, 36
	.db	108, 86, 62, 46
	.db	136, 108, 76, 60
	.db	156, 124, 88, 66
	.db	194, 154, 110, 86
	.db	232, 182, 132, 100
VERSION_10:
	.dw	274, 216, 154, 122
	.dw	324, 254, 180, 140
