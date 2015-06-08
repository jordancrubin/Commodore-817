;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      C64 interface for the FT8XX Yaesu Radio         ;;
;;      Using the CAT command protocol                  ;;
;;      Jordan Rubin 2015                               ;;
;;      technocoma.blogspot.com                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       help.asm                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*=$C000                 ; Store routine at 49152

	lda #$00         
	sta $d020       ; black border
	sta $d021       ; black background
	lda #$17 
	sta $D018 
	jmp START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        START                         ;;
;; The routines to generate the 4 page help screen      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

START:
	jsr WINDOWPREP
	jsr PAGE1T1
	jsr CONTINUEBANNERLOCATE
	jsr WAITFORKEY

	jsr WINDOWPREP
	jsr PAGE2T1
	jsr CONTINUEBANNERLOCATE
	jsr WAITFORKEY

	jsr WINDOWPREP
	jsr PAGE3T1
	jsr CONTINUEBANNERLOCATE
	jsr WAITFORKEY

	jsr WINDOWPREP
	jsr PAGE4T1
	jsr CONTINUEBANNERLOCATE
	jsr WAITFORKEY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   LOADSTARTUPCHK                     ;;
;; When pressing a key on the 4th page we jump here.    ;;
;; Load our filename for main into memory and run the   ;;
;; loaded code at $0C00                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOADSTARTUPCHK:
	lda #mainfname_end-mainfname
	ldx #<mainfname
	ldy #>mainfname
	jmp $0C00           ;LAUNCH LOADER FOR startupchk.prg


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    WAITFORKEY                        ;;
;;             Looping key wait routine                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WAITFORKEY:
	jsr $FFE4            ; GETKEYIN
	cmp  #$00        
	beq  WAITFORKEY      ; No key, go back to PROMPT
	cmp  #$20            ; STOP key, go to Exit
	bne  WAITFORKEY 
	rts

WINDOWPREP:        
	lda #$93               ; #$93 clear screen
	jsr $ffd2
	ldx #$03
	stx $0286              ; lt.blue color
	ldx #$00
	ldy #$00               ; Y coordunate for cursor
	clc                    ; carry
	jsr $fff0              ; move cursor        

WINDOWLINETOP:
	lda #$60               ; Give A an underline hex value   was 64
	jsr $ffd2              ;  Print one underline character
	inx                    ; X=X+1
	cpx #$28               ; does X = hex 28
	bne WINDOWLINETOP      ; branch to ULINE of not 28
	
LOCATEBANNER:	
	ldx #$00
	ldy #$0A                ; Y coordunate for cursor
	clc                     ; carry
	jsr $fff0               ; move cursor
	ldx #$00
	
PRINTBANNER:
	lda TOPBANNER,x
	jsr $ffd2
	inx
	cpx #$14
	bne PRINTBANNER
	ldx #$02
	ldy #$00                ; Y coordunate for cursor
	clc                     ; carry
	jsr $fff0               ; move cursor  
	ldx #$00
	rts

CONTINUEBANNERLOCATE:
	ldx #$18
	ldy #$08                ; Y coordunate for cursor
	clc                     ; carry
	jsr $fff0               ; move cursor
	ldx #$00

CONTINUEBANNER:
	lda CONTINUEBANNERTEXT,x
	jsr $ffd2
	inx
	cpx #$19
	bne CONTINUEBANNER
	rts

PAGE1T1:
	lda PAGE1TEXT1,x
	jsr $ffd2
	inx
	cpx #$F4
	bne PAGE1T1
	ldx #$00
	
PAGE1T2:		
	lda PAGE1TEXT2,x
	jsr $ffd2
	inx
	cpx #$FA
	bne PAGE1T2
	ldx #$00
	rts

PAGE2T1:
	lda PAGE2TEXT1,x
	jsr $ffd2
	inx
	cpx #$F2
	bne PAGE2T1
	ldx #$00

PAGE2T2:
	lda PAGE2TEXT2,x
	jsr $ffd2
	inx
	cpx #$BE
	bne PAGE2T2
	ldx #$00
	rts

PAGE3T1:
	lda PAGE3TEXT1,x
	jsr $ffd2
	inx
	cpx #$FF
	bne PAGE3T1
	ldx #$00

PAGE3T2:
	lda PAGE3TEXT2,x
	jsr $ffd2
	inx
	cpx #$F1
	bne PAGE3T2
	ldx #$00
	rts
	
PAGE4T1:
	lda PAGE4TEXT1,x
	jsr $ffd2
	inx
	cpx #$DF
	bne PAGE4T1
	ldx #$00	
	rts		

CONTINUEBANNERTEXT:
	.text "[PRESS SPACE TO CONTINE]"
	
TOPBANNER:
	.text " Commodore 817 Help "
	
PAGE1TEXT1:
	.text "  This program will allow you to controlthe most commmon functions of the FT-817with your Commodore 64. "
	.text "It has also beenfully tested under Vice, should you wantto go the emulator route.  Be sure, thatyour glink232,"
	.text " or similar device, is setfor "
	
PAGE1TEXT2:
	.text "$DE00, for the jumper configuration.  ",$0D,"  Also, if using a printer, make sure itis"
	.text " set for device number 4.  The programis MPS-801 MPS-803 compatible. I haven'ttested any others.",$0D, $0D
	.text "  The only other requirement is that theradio must be set to 9600 baud." 
	
PAGE2TEXT1:
	.text "   It is very important that a backup ofthe radio's calibration settings be madefirst. In the event something"
	.text " happens tothe configuration, it can easily be readback from file or printout. While havinga backup on floppy makes"
	.text " for an easy wayto"
	
PAGE2TEXT2:
	.text " restore, a hardcopy allows for a copythat is permanent.",$0D,$0D,"  This will be my last warning. You havebeen"
	.text " warned!!",$0D,$0D,$0D,"  Once the calibration routine ends, youwill be brought to the main program."
	
PAGE3TEXT1:
	.text "   The top window shows the main displayof your rig showing VFO, Frequency, ModePower, as well as your callsign" 
	.text ", Squelchand meter. In RX the meter is an Smeter,in TX the meter is the SWR.",$0D,$0D,"   The bottom window"
	.text " shows settings thatmay be changed by pressing"
	
PAGE3TEXT2:
	.text " any of the 12highlighted letters or the function keysshown below.",$0D,$0D,"   Note that F1 will require 8 digits"
	.text " to be entered. 14.070.00 is 01407000",$0D,$0D,"   F2 will transfer the settings of VFOAto VFOB and match the band."
	.text $0D,$0D,"   F6 is this help program."
	
PAGE4TEXT1:
	.text "  F8 will run a calibration check of theradio against the saved copy.",$0D,$0D,"Shout out to:",$0D,"KN4PKY",$0D,"KJ4VOK",$0D,"N0IMB",$0D
	.text "N4LGH",$0D,"KD4WOV",$0D,"The 147120 repeater",$0D,"and KA7OEI for his work with the FT817.",$0D,$0D,$0D
	.text "Jordan Rubin KJ4TLB",$0D,"TECHNOCOMA.BLOGSPOT.COM" 
		
	mainfname:  .text "STARTUPCHK"
mainfname_end:
