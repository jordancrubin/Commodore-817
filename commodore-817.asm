;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      C64 interface for the FT8XX Yaesu Radio         ;;
;; Works with Glink232 cartrige and compatible devices  ;;
;; Using the CAT command protocol and ACIA              ;;
;;      Jordan Rubin 2015                               ;;
;;      http://technocoma.blogspot.com                  ;;
;;      Written for compilation with 64TASS             ;;
;; All resulting PRG files shold be UPPERCASE except    ;;
;; for commodore-817.asm                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   commodore-817.asm                  ;;
;; This is the main startup file and autostarts from    ;;
;; basic when it finishes loading.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Because of how its written, this file would benefit  ;;
;; greatly from a cruncher to compress the wasted space ;;                                                     
;; No other files need crunching.                       ;;
;; Requires:                                            ;;          
;; 		main.asm                                ;;          
;;              help.asm                                ;;          
;;              startupchk.asm                          ;;          
;;              tcalchk.asm                             ;;                                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*=$0801              ; Autostart code does sys 49152 from basic

 .byte $0d,$08,$d9,$07,$9e,$20,$34,$39 
 .byte $31,$35,$32,$00,$00,$00 

*=$c000              ; Store routine at 49152   


DRAWSPLASHSCREEN:
	lda #$17 
	sta $D018 
	lda #$00         
	sta $d020       ; black border
	sta $d021       ; black background
	lda #$93        ; #$93 clear screen
	jsr $ffd2
	jsr FILLTEXTPREP
	jsr WINDOWPREP
	jsr WAITFORKEY
	rts

WINDOWPREP:        
	ldx #$03
	stx $0286          ; lt.blue color
	ldx #$00
	ldy #$00           ; Y coordunate for cursor
	clc                ; carry
	jsr $fff0          ; move cursor              
	lda #$f0           ; top left curved end    was d5
	jsr $ffd2
	jsr WINDOWLINETOP
	lda #$ee           ; top right curved end  was c9
	jsr $ffd2
	ldx #$01
	jsr SIDEBARS
	lda #$ed           ; bottom left was ca
	jsr $ffd2
	jsr WINDOWLINEBOTTOM
	lda #$fd           ; bottom right was cb
	jsr $ffd2

WAITFORKEY:
	jsr $FFE4            ; GETKEYIN
	cmp  #$00        
	beq  WAITFORKEY      ; No key, go back to PROMPT
	cmp  #$03            ; STOP key, go to Exit
	beq  EXIT      
	cmp  #$49            ; I	
	beq LOADHELP	
	cmp  #$20            ; SPACE
	beq LOADPROGRAM
	jmp WAITFORKEY

LOADPROGRAM:	
	lda #fname_end-fname
	ldx #<fname
	ldy #>fname
	jmp LOADFILE

LOADHELP:
	lda #hname_end-hname
	ldx #<hname
	ldy #>hname
	jmp LOADFILE

EXIT:
	rts

FILLTEXTPREP:
	lda #$01		; white
	sta $0286      		; store in color register 
	ldx #$01
	ldy #$05                ; Y coordunate for cursor
	clc                     ; carry
	jsr $fff0               ; move cursor  
	ldx #$00

FILLTEXT:
	lda LOGO,x       
	jsr $FFD2
	inx
	cpx #$64
	bne FILLTEXT
	ldx #$08
	ldy #$01                ; Y coordunate for cursor
	clc                     ; carry
	jsr $fff0               ; move cursor  
	ldx #$00

FILLTEXT2:
	lda LOGO2,x  
	jsr $FFD2
	inx
	cpx #$D9
	bne FILLTEXT2
	ldx #$00

FILLTEXT3:
    	lda LOGO4,x  
    	jsr $FFD2
    	inx
    	cpx #$64
    	bne FILLTEXT3

KEYTOCONTINUE:
	lda #$02		; white
	sta $0286      		; store in color register 
	ldx #$15
	ldy #$06                ; Y coordunate for cursor
	clc                     ; carry
	jsr $fff0               ; move cursor 
	ldx #$00

FILLTEXT4:
	lda LOGO5,x  
	jsr $FFD2
	inx
	cpx #$39
	bne FILLTEXT4
	rts

SIDEBARS:
	ldy #$00                 ; Y coordunate for cursor
	clc                      ; carry
	jsr $fff0                ; move cursor  
	lda #$dd                 ; vertical bar left side was c7
	jsr $ffd2
	ldy #$27                 
	clc
	jsr $fff0                ; move cursor  
	lda #$dd                 ; vertical bar right side   was c8
	jsr $ffd2
	inx
	cpx #$04
	bne SIDEBARS
	rts

WINDOWLINETOP:
	lda #$60               ; Give A an underline hex value   was 64
	jsr $ffd2              ; Print one underline character
	inx                    ; X=X+1
	cpx #$26               ; does X = hex 28
	bne WINDOWLINETOP      ; branch to ULINE of not 28
	rts

WINDOWLINEBOTTOM:
	lda #$60              ; Give A an underline hex value was 63
	jsr $ffd2             ;  Print one underline character
	inx                   ; X=X+1
	cpx #$2A              ; does X = hex 28
	bne WINDOWLINEBOTTOM  ; branch to ULINE of not 28
	rts

LOGO:
.text "***COMMODORE-817 CONTROLLER***",$0D,"       Jordan Rubin [KJ4TLB] 2015",$0D,"     http://technocoma.blogspot.com"
LOGO2:
.text "This program will allow communication  between the Commodore 64 and the Yaesu  FT-817 radio via the CAT cable." 
LOGO3:
.text " Ensure  that the radio is set to 9600 baud and  connected to a Glink232 or similar C64  RS232 cartridge.",$0D,$0D
LOGO4:
.text " Written with Relaunch64 using 64tass.",$0D,"Yaesu and the FT817 are a copyright of",$0D,"Vertex Standard Japan."
LOGO5:
.text "[Press SPACE to continue]",$0D,"       or 'I' for instructions "

fname:  .text "STARTUPCHK"
fname_end:

hname:  .text "HELP"
hname_end:


*=$0C00
LOADFILE:

        jsr $FFBD     ; call SETNAM
        lda #$01
        ldx $BA       ; last used device number
        bne skip
        ldx #$08      ; default to device 8
skip:    
        ldy #$01      ; not $01 means: load to address stored in file
        jsr $FFBA     ; call SETLFS

        lda #$00      ; $00 means: load to memory (not verify)
        jsr $FFD5     ; call LOAD
        bcs error     ; if carry set, a load error has happened
        jmp START
error:
        ; Accumulator contains BASIC error code
        ; most likely errors:
        ; A = $05 (DEVICE NOT PRESENT)
        ; A = $04 (FILE NOT FOUND)
        ; A = $1D (LOAD ERROR)
        ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
        ;... error handling ...
		brk        

START:
jmp $C000
