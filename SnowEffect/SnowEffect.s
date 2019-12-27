pool zpGlobal $40-$f8
zpGlobal pool zpLocal 16
zpGlobal pool zpUtility 8

const ColorRAM = $d800
const RawRect = $5000
const RectSize = 8*4*4 ; 16 chars/shift
const RectEnd = RawRect + 8 * RectSize

const SnowFont = $4000
const SnowScreen = $4800
const SnowChars = SnowScreen - 16*8	; use last chars for font

const NUM_FLAKES = 8
const ScrollYSpeed = 57
const ScrollXSpeed = 29

SECTION Code, code

org $801

dc.b $0b, $08, $01, $00, $9e, $32, $30, $36, $34, $00, $00, $00, $00, $00, $00

Snowie:
{	; some basic screen & interrupt setup
	sei
	lda #$35
	sta 1
	lda #$7f
	sta $dc0d
	sta $dd0d
	lda $dc0d
	lda $dd0d
	lda #14
	sta $d021
	sta $d020
	lda #1
	sta $d022
	lda #10
	sta $d023
	lda #0
	sta $d01d
   	lda #((SnowScreen>>6)&$f0) | ((SnowFont>>10)&$0e)
	sta $d018
	lda $dd00	 ; vic bank = $4000
	and #$fc
	ora #2
	sta $dd00
	lda #$1b
	sta $d011
    lda #$d8
    sta $d016
	lda #<Interrupt1
	sta $fffe
	lda #>Interrupt1
	sta $ffff
	lda #<justRTI
	sta $fffa
	lda #>justRTI
	sta $fffb


	lda #20
	sta $d012
	lda #1
	sta $d01a

	jsr CopyColors
	ldx #0
	ldy #WinterCharsSize>>8
	{
		lda WinterChars,x
		sta SnowFont,x
		inx
		bne !
		inc !+2
		inc !+5
		dey
		bpl !
	}
	ldx #0
	{
		rept 4 {
			 lda WinterScreen + rept * 256,x
			 sta SnowScreen + rept * 256,x
		}
		inx
		bne !
	}
	{
		zpLocal .zpScrn.w
		zpLocal .zpCol.w
		lda #<SnowScreen
		sta .zpScrn
		sta .zpCol
		lda #>SnowScreen
		sta .zpScrn+1
		lda #>ColorRAM
		sta .zpCol+1
		ldx #0
		{
			ldy #40-1
			{
				{
					zpLocal .zpChar
					; 0, 4, 8, 12, 2, 6, 10, 14
					lda (.zpScrn),y
					bne %
					tya
					asl
					asl
					and #$0c
					sta .zpChar
					txa
					and #3
					ora .zpChar
					sta .zpChar
					tya
					lsr
					and #2
					eor .zpChar
					and #15
					ora #(SnowChars>>3)
					sta (.zpScrn),y
					lda #1
					sta (.zpCol),y
				}
				dey
				bpl !
			}
			clc
			lda .zpScrn
			adc #40
			sta .zpScrn
			sta .zpCol
			{
				bcc %
				inc .zpScrn+1
				inc .zpCol+1
			}
			inx
			cpx #25
			bcc !
		}
	}
	jsr UpdateScrollPos	; do one without drawing to initialize previous values
	jsr CopyChars
	jsr MakeCodeGen

	cli

	{
		sta IntWait
		{
			cmp IntWait
			beq !
		}
;		inc $d020
		jsr UpdateScrollPos	; do one without drawing to initialize previous values
		jsr CopyChars
		jsr DrawFlakes
		jsr ScrollSnow
;		dec $d020
		jmp !
	}
}

Interrupt1:
{	; set sky color in border
	pha
	lda #14
	sta $d020
	lda #<Interrupt2
	sta $fffe
	lda #>Interrupt2
	sta $ffff
	lda #138+50
	sta $d012
	lda #$1b
	sta $d011
	rol $d019
	pla
justRTI:
	rti
}

Interrupt2:
{	; set snow color in border
	pha
	lda #<Interrupt1
	sta $fffe
	lda #>Interrupt1
	sta $ffff
	lda #20
	lda #1
	sta $d020
	sta $d012
	lda #$1b
	sta $d011
	rol $d019
	inc IntWait
	pla
	rti
}

CopyColors:
{	; copy color nibbles to color ram
    zpLocal .zpDst.w
    lda #<ColorRAM
    sta .zpDst
    lda #>ColorRAM
    sta .zpDst+1
    ldx #0
    ldy #0
    {
        lda WinterCol,x
        sta (.zpDst),y
        iny
        lsr
        lsr
        lsr
        lsr
        sta (.zpDst),y
        iny
        {
            bne %
            inc .zpDst+1
        }
        cpy #<(25*40)
        bne .nchk
        lda .zpDst+1
        cmp #$db
        beq %
.nchk	inx
        bne !
        inc !+2
        bne !
    }
    rts
}

UpdateScrollPos:
{
	sec
	lda ScrollY
	sbc #ScrollYSpeed
	sta ScrollY
	{
		bcs %
		dec ScrollY+1
	}
	clc
	lda ScrollX
	adc #ScrollXSpeed
	sta ScrollX
	{
		bcc %
		inc ScrollX+1
	}

	zpLocal .zpLo
	zpLocal .zpHi

	inc SinOffsX
	inc SinOffsY
	lda SinOffsY
	and #$7f
	tax
	lda #0
	sta .zpHi
	lda Sinus,x
	asl
	rol .zpHi
	asl
	rol .zpHi
	asl
	rol .zpHi
	adc ScrollY
	sta CurrPosY
	sta .zpLo
	lda .zpHi
	adc ScrollY+1
	sta ShiftYPos
	sta CurrPosY+1

	{	; check scroll x
		lda #0
		sta .zpHi
		lda SinOffsX
		and #$7f
		tax
		lda Sinus,x
		asl
		rol .zpHi
		asl
		rol .zpHi
		asl
		rol .zpHi
		asl
		rol .zpHi
		asl
		rol .zpHi
		clc
		adc ScrollX
		sta CurrPosX
		sta .zpLo
		lda .zpHi
		adc ScrollX+1
		tax
		sec
		sbc ShiftXPos
		stx ShiftXPos
		stx CurrPosX+1
		beq %
		{
			lda #1
			bcc %
			lda #2
		}
		sta ScreenEffectExtra ; 3: left, 4: right
		rts
	}
	; no scroll
	lda #0
	sta ScreenEffectExtra
	rts
}

; copy snow backdrop shifted up by ShiftYPos in two passes
CopyChars:
{
	lda ShiftYPos
	and #$1f
	tax
	ldy #$1f
	{
		rept 4 {
			 lda BGCharMem + rept * $20, x
			 ;lda #0
			 sta SnowChars + rept * $20,y
		}
		dey
		dex
		bpl !
	}
	{
		tya
		bpl %
		rts
	}
	ldx #$1f
	{
		rept 4 {
			 lda BGCharMem + rept * $20, x
			 ;lda #0
			 sta SnowChars + rept * $20,y
		}
		dex
		dey
		bpl !
	}
	rts
}

; scroll snow background left or right
ScrollSnow:
{
	{
		ldy ScreenEffectExtra
		bne %
		rts
	}
	ldx #15
	{
		dey
		bne %
		{	; left
			lda BGCharMem+96,x
			asl
			rol BGCharMem + 64,x
			rol BGCharMem + 32,x
			rol BGCharMem,x

			rol BGCharMem + 112,x
			rol BGCharMem + 80,x
			rol BGCharMem + 48,x
			rol BGCharMem + 16,x
			adc #0
			sta BGCharMem + 96,x
			dex
			bpl !
		}
		rts
	}
	{	; right
		lda BGCharMem,x
		lsr
		ror BGCharMem + 32,x
		ror BGCharMem + 64,x
		ror BGCharMem + 96,x

		ror BGCharMem + 16,x
		ror BGCharMem + 48,x
		ror BGCharMem + 80,x
		ror BGCharMem + 112,x
		{
			bcc %
			ora #$80
		}
		sta BGCharMem,x
		dex
		bpl !
	}
	rts
}

; ya = a * y, but y only up to 63
MulAccY:
{
	zpUtility .zpMulRes.w
	zpUtility .zpMulShift
	zpUtility .zpMulProd

	sty .zpMulProd
	ldy #0
	sty .zpMulShift
	sty .zpMulRes
	sty .zpMulRes+1
	ldy #4 ; only do 6 bits..
	{
		lsr .zpMulProd
		{
			bcc %
			pha
			clc
			adc .zpMulRes
			sta .zpMulRes
			lda .zpMulShift
			adc .zpMulRes+1
			sta .zpMulRes+1
			pla
		}
		asl
		rol .zpMulShift
		dey
		bpl !
	}
	lsr .zpMulProd ; last bit
	{
		bcc %
		clc
		adc .zpMulRes
		sta .zpMulRes
		lda .zpMulShift
		adc .zpMulRes+1
		sta .zpMulRes+1
	}
	lda .zpMulRes
	ldy .zpMulRes+1
	rts
}

; draw the individual snowflakes on top of the snow background
DrawFlakes:
{
	ldx #NUM_FLAKES-1
	{
		lda FlakeXSinSpd,x
		clc
		adc FlakeXSinLo,x
		sta FlakeXSinLo,x
		{
			bcc %
			inc FlakeXSinHi,x
		}

		lda FlakeYSinSpd,x
		clc
		adc FlakeYSinLo,x
		sta FlakeYSinLo,x
		{
			bcc %
			inc FlakeYSinHi,x
		}

		txa
		pha

		lda FlakeType,x
		pha
		lda FlakeXSinHi,x
		and #$7f
		tay
		lda Sinus,y
		ldy FlakeXSinScl,x
		jsr MulAccY
		clc
		adc CurrPosX
		tya
		adc CurrPosX+1
		pha

		lda FlakeYSinHi,x
		and #$7f
		tay
		lda Sinus,y
		ldy FlakeYSinScl,x
		jsr MulAccY
		sec
		sbc CurrPosY
		tya
		sbc CurrPosY+1
		tay

		pla
		tax
		pla

		jsr DrawFlake
		pla
		tax
		dex
		bmi %
		jmp !
	}
	rts
}

; call the codegen functions for drawing the left/right bytes of the individual flakes
; a = type (0-2)
; x = xpos
; y = ypos
DrawFlake:
{
	zpLocal .zpIndex
	zpLocal .zpTop
	asl ; 8 positions / type
	asl
	asl
	sta .zpIndex
	txa ; + position & 7
	and #7
	ora .zpIndex
	sta .zpIndex
	txa
	asl
	asl
	and #$60 ; (position >> 3) << 5
	sta .zpTop
	tya
	and #$1f ; + y & $1f
	ora .zpTop
	sta .zpTop
	txa
	{
		and #$20
		beq %
		lda .zpTop
		eor #$10
		sta .zpTop
	}
	ldy .zpIndex
	{
		lda CodeGenDrawLeftHi,y
		beq % ; skip if no left column
		sta .callLeft+2
		lda CodeGenDrawLeftLo,y
		sta .callLeft+1
		ldx .zpTop
.callLeft
		jsr CodeGen
		ldy .zpIndex
	}
	{
		lda CodeGenDrawRightHi,y
		beq % ; skip if no left column
		sta .callRight+2
		lda CodeGenDrawRightLo,y
		sta .callRight+1
		lda .zpTop
		{
			cmp #$60
			bcc .add
			and #$1f
			eor #$10
			bpl %
.add		adc #$20
		}
		tax
.callRight
		jmp CodeGen
	}
	rts
}

; copy this code for each byte to draw
CodeGenTemplate:
{
	lda SnowChars,x
const CodeGenTemplateByte = *+1-CodeGenTemplate
	ora #$aa
	sta SnowChars,x
	lda NextSnowRow,x	; 4 + 2 cycles for next index
	tax
}
const CodeGenTemplateSize = * - CodeGenTemplate

MakeCodeGen:
{
	zpLocal .zpGen.w
	zpLocal .zpSrc.w
	lda #<CodeGen
	sta .zpGen
	lda #>CodeGen
	sta .zpGen+1
	ldx #0
	{
		txa
		asl
		rol .zpSrc+1
		asl
		rol .zpSrc+1
		asl
		rol .zpSrc+1
		asl
		rol .zpSrc+1
		clc
		adc #<Flakes
		sta .zpSrc
		lda .zpSrc+1
		and #$0f
		adc #>Flakes
		sta .zpSrc+1

		jsr .getCodeGen
		sta CodeGenDrawLeftLo,x
		tya
		sta CodeGenDrawLeftHi,x
		{
			beq %
			jsr .generateCol
		}
		lda .zpSrc
		ora #8
		sta .zpSrc
		jsr .getCodeGen
		sta CodeGenDrawRightLo,x
		tya
		sta CodeGenDrawRightHi,x
		{
			beq %
			jsr .generateCol
		}
		inx
		cpx #3*8
		bcc !
	}
	rts

.getCodeGen
	txa
	pha
	{ ; check if this column is empty first
		ldy #0
		ldx #7
		{
			lda (.zpSrc),y
			bne %
			tya ; empty byte insert next byte
			asl
			asl
			tay
			lda #$bd ; lda abs,x
			sta (.zpGen),y
			iny
			lda #<NextSnowRow
			sta (.zpGen),y
			iny
			lda #>NextSnowRow
			sta (.zpGen),y
			iny
			lda #$aa ; tax
			sta (.zpGen),y
			iny
			tya
			lsr
			lsr
			tay
			dex
			bpl !
			pla
			tax
			lda #0
			tay
			rts
		}
		pla
		tax
		tya
		asl
		asl
		adc .zpGen
		sta .zpGen
		{
			bcc %
			inc .zpGen+1
		}
		ldy .zpGen+1
	}
	rts

.generateCol
	ldy #7
	{
		lda (.zpSrc),y
		bne %
		dey
		bpl !
		rts
	}
	txa
	pha
	tya
	tax
	ldy #0
	{
		zpLocal .zpY

		lda (.zpSrc),y
		sty .zpY
		pha
		ldy #CodeGenTemplateSize-1
		{
			lda CodeGenTemplate,y
			sta (.zpGen),y
			dey
			bpl !
		}
		ldy #CodeGenTemplateByte
		pla
		sta (.zpGen),y
		clc
		lda .zpGen
		adc #CodeGenTemplateSize
		sta .zpGen
		{
			bcc %
			inc .zpGen+1
		}
		ldy .zpY
		iny
		dex
		bne !
	}
	ldy #0 ; insert rts
	lda #$60 ; rts
	sta (.zpGen),y
	{	; increment by one
		inc .zpGen
		bne %
		inc .zpGen+1
	}
	pla
	tax
	rts
}

SECTION Data, code
align 256
BGCharMem:	; 128 bytes
	incbin "bin/snowback.bin"

Sinus: ; 128 bytes
	dc.b 127, 133, 139, 146, 152, 158, 164, 170, 176, 181, 187, 192, 198, 203, 208, 212
	dc.b 217, 221, 225, 229, 233, 236, 239, 242, 244, 247, 248, 250, 252, 253, 253, 254
	dc.b 254, 254, 253, 253, 252, 250, 248, 247, 244, 242, 239, 236, 233, 229, 225, 221
	dc.b 217, 212, 208, 203, 198, 192, 187, 181, 176, 170, 164, 158, 152, 146, 139, 133
	dc.b 127, 121, 115, 108, 102, 96, 90, 84, 78, 73, 67, 62, 56, 51, 46, 42
	dc.b 37, 33, 29, 25, 21, 18, 15, 12, 10, 7, 5, 4, 2, 1, 1, 0
	dc.b 0, 0, 1, 1, 2, 4, 5, 7, 10, 12, 15, 18, 21, 25, 29, 33
	dc.b 37, 42, 46, 51, 56, 62, 67, 73, 78, 84, 90, 96, 102, 108, 115, 121

NextSnowRow: ; 128 bytes
	rept 128 {
		if( (rept & $1f)==$1f )
			dc.b rept & $60
		else
			dc.b rept + 1
		endif
	}

; stuff not aligned to 256
WinterScreen:
	incbin "bin/somekindoftreeman.scr"
WinterCol:
	incbin "bin/somekindoftreeman.col"
WinterChars:
	incbin "bin/somekindoftreeman.chr"
const WinterCharsSize = * - WinterChars

Flakes:
	incbin "bin/snowflakes.bin"

; each of these arrays must be NUM_FLAKES long
FlakeType:
	dc.b 0, 1, 2, 1, 2, 0, 1, 2
FlakeXSinHi:
	dc.b 0, 16, 32, 48, 64, 80, 96, 112
FlakeYSinHi:
	dc.b 32, 96, 80, 64, 48, 32, 16, 0
FlakeXSinSpd:
	dc.b 144/2, 132/2, 112/2, 150/2, 138/2, 147/2, 96/2, 137/2
FlakeYSinSpd:
	dc.b 137/2, 144/2, 138/2, 147/2, 96/2, 132/2, 112/2, 150/2
FlakeXSinScl:
	dc.b 33, 31, 29, 27, 25, 23, 21, 35
FlakeYSinScl:
	dc.b 37, 17, 35, 25, 27, 29, 23, 19
SinOffsX:
	dc.b 64
SinOffsY
	dc.b 32

SECTION BSS, bss
org $f000

ScrollFrame:
	ds 1
ScreenEffectExtra:
	ds 1

// linear scrolling values, add sin curve on top
ScrollX:
	ds 2
ScrollY:
	ds 2

CurrPosX:
	ds 2
CurrPosY:
	ds 2

ShiftXPos:
	ds 1 ; remember the previous sin value shifted to find delta
ShiftYPos:
	ds 1

FlakeXSinLo:
	ds NUM_FLAKES
FlakeYSinLo:
	ds NUM_FLAKES

IntWait
	ds 1

CodeGenDrawLeftLo:
	ds 3*8
CodeGenDrawLeftHi:
	ds 3*8

CodeGenDrawRightLo:
	ds 3*8
CodeGenDrawRightHi:
	ds 3*8

CodeGen: