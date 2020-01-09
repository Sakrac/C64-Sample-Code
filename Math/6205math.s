; These are some 6502 math coding tips that is also a gist but it fits well in this repo

; subtract Accurmlator from byte
	sec
	eor #$ff
	adc Address
	; a = (Address) - a

;----

; Add or subtract from a byte based on carry (set = subtract, clear = add)
	bcc .add
	eor #$ff
.add
	adc Address

;----

; multiply a * x
	temp Shift.w	; temp means temporary storage, .w means 16 bit value
	temp Result.w
	stx Shift
	ldx #0
	stx Shift+1
	stx Result
	stx Result+1
	ldx #8
.bit
		lsr
		bcc .clear
			pha
			clc
			lda Shift
			adc Result
			sta Result
			lda Shift+1
			adc Result+1
			sta Result+1
			pla
.clear	asl Shift
		rol Shift+1
		dex
		bne .bit
	lda Result
	ldx Result+1
; a = lo, x = hi of (a * x)

;----

; divide 16bit by 8bit
; x = lo, a = hi, y = divider
	temp num.w
	temp remainder
	temp div

    stx num
    sta num+1
    lda #0
    sta remainder
    sty div
    ldx #16
.bit
        asl num
        rol num+1
        rol remainder
		sec
		lda remainder
		sbc div
		bcc .less
            sta remainder
            inc num ; record this bit div
.less   dex
        bne !
    }
    lda num
    ldx remainder
; a = 16bit/8bit, x = remainder

;----

; compare a 16 bit number with a value
; x = lo, a = hi
	cpx #<compare ; C is set if x >= <compare
	sbc #>compare ; takes low byte compare into account if equal
; C is set if number is greater or equal to compare
