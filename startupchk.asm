;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      C64 interface for the FT8XX Yaesu Radio         ;;
;;      Using the CAT command protocol                  ;;
;;      Jordan Rubin 2015                               ;;
;;      technocoma.blogspot.com                         ;;
;;      Written for compilation with 64TASS             ;;
;; All resulting PRG files should be UPPERCASE except    ;;
;; for commodore-817.asm                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    startupchk.asm                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define GLINK232/SWIFTLINK/TURBO232 labels here       ;;
;; Swiftlink registers found from                       ;;
;; ar.c64.org/wiki/turbo232_swiftlink_registers.txt     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

glink232  	   =   $DE00    ;Glink232 cart, ACIA, this must match on the DIP switch of the GLINK232
dataregister  	   =   glink232
statusregister     =   glink232+1
commandregister    =   glink232+2
controlregister    =   glink232+3
recvhead           =   $A7      ;pointer to next byte to be removed from receive buffer
recvtail           =   $A8      ;pointer to location to store next byte received
recvbuffer         =   $F7      ;receive-buffer vector   $c160
xmithead   	   =   $A9      ;pointer to next byte to be removed from transmit buffer
xmittail           =   $AA      ;pointer to location to store next byte in transmit buffer
xmitbuffer         =   $F9      ;transmit buffer         $c156
xmitbuffersize 	   =   $AB      ;number of bytes currently in xmitbuffer
recvbuffersize     =   $B4      ;number of bytes currently in recvbuffer
xmiton   	   =   $B6      ;storage location for model of command register for transmit on
xmitoff  	   =   $BD      ;storage location for model of command register for transmit off
nmivector          =   $0318    ;Commodore Non-Maskable Interrupt vector
oldnmivector       =   $03FE    ;location to store our old NMI vector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define SPRITE LABELS HERE                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SPRITEDATA         =   $2000    ;Data for sprites 0 through 7
SPENA              =   $D015    ;Sprite enable register
SSDP0              =   $07F8    ;Sprite Data pointers
SP0X               =   $D000    ;Sprite 0 Horizontal
SP0Y               =   $D001    ;Sprite 0 Vertical
SP0COL             =   $D027    ;Sprite 0 Color register
OFFSET             =   $FE      ;Initial Sprite offset Y 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define FT817 LABELS HERE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

radiobyte          =   $FB
isoutput           =   $FE
updateline         =   #$0D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program startpoint                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*=  $C000              ;Start of program at 49152

	lda #$17 
	sta $D018      ;changes to upperlower
	lda #$93       ; #$93 clear screen
	jsr $ffd2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize values to 0, clear everything             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda   #$00
	sta   recvhead
	sta   recvtail
	sta   xmithead
	sta   xmittail
	sta   xmitbuffersize
	sta   recvbuffersize
    	sta   isoutput



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the locations of the transmit and receive     ;;
;; Buffers from the memory areas just at the bottom of  ;;
;; the program. Stores it in 2 bytes  for the 16bit     ;;
;; memory address location                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda   #<TRANSMIT_BUFFER
	sta   xmitbuffer
	lda   #>TRANSMIT_BUFFER
	sta   xmitbuffer + 1
	lda   #<RECEIVE_BUFFER
	sta   recvbuffer
	lda   #>RECEIVE_BUFFER
	sta   recvbuffer + 1



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the values of the control register              ;;
;; and store it into control                            ;;
;; FT817 runs at 9600 or 4800 baud                      ;;
;; BIT [7]  : (1)    2 stop bits                        ;;
;; BIT [6-5]: (00)   8 bit                              ;;
;; BIT [4]  : (1)    Internal baudrate generator        ;;
;; BIT [3-0]: (1100) 9600 baud    or.....               ;;
;; BIT [3-0]: (1010) 4800 baud                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda   #%10011100 ; 9600 baud
	sta   controlregister



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the values of the command register              ;;
;; and store it into command and xmitoff                ;;
;; BIT [7-5]: (000)  Parity - NONE                      ;;
;; BIT [4]  : (0)    Echo   - OFF                       ;;
;; BIT [3-2]: (10)   TXIRQ  - OFF, RTS - LOW , XMIT ON  ;;
;; BIT [1]  : (0)    RCV INTERRUPT ENABLED              ;;
;; BIT [0]  : (1)    MASTER IRQ CONTROL ON              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
	lda   #%00001001
	sta   commandregister
    	sta   xmitoff 
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leave the parity and echo bits alone. BITS [7-4]     ;;
;; make the last four bits 0101 and store it to         ;;
;; xmiton to provide                                    ;;
;; BIT [3-2]: (01)   TXIRQ  - ON , RTS - LOW , XMIT ON  ;;
;; BIT [1]  : (1)    RCV INTERRUPT DISABLED             ;;
;; BIT [0]  : (0)    MASTER IRQ CONTROL OFF             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
          
	and   #%11110000     
	ora   #%00000101
	sta   xmiton     
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is where we replace the old NMI vector with our ;;     
;; own.  The old vector will be stored in oldnmivector  ;;      
;; this is 16bit and requires 2 bytes                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	sei                   ;Disable interrupts just in case
	lda   nmivector	      ;get low byte of the current NMI vector
	sta   oldnmivector    ;store it in oldnmivector
	lda   nmivector+1     ;get the high byte of the current NMI vector
	sta   oldnmivector+1  ;store it in oldnmivector + 1
	lda   #<NEWNMI        ;get low byte of our new NMI code
	sta   nmivector       ;store it in the nmivector
	lda   #>NEWNMI        ;get the high byte of our new NMI code
	sta   nmivector+1     ;store it in the nmivector + 1
	cli                   ;re-enable IRQ     
      
	
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We jump to CHECKCONNECT, where the program begins    ;;     
;; at the user end                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
            
	jmp   SPRITELOADER 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is our NEWNMI routine what happens when an NMI  ;;                
;; occurs, will jump to here to execute NMI code that   ;;               
;; we replaced thre original NMI code with.  This is    ;;          
;; not IRQ, we must store the processor state to the    ;;          
;; stack first, and restore it later                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
NEWNMI:
	sei     ; the usual processor status storage routine         
	pha              
	txa
	pha              
	tya
	pha     ; processor state now stored to stack                
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the statusregister into A                       ;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
      
	lda   statusregister



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify command register shutoff NMI from GLINK232    ;;
;;                                                      ;;
;; BIT [7-5]: (000)  Parity - NONE                      ;;
;; BIT [4]  : (0)    Echo   - OFF                       ;;
;; BIT [3-2]: (00)   TXIRQ  - OFF, RTS - HIGH, XMIT OFF ;;
;; BIT [1]  : (1)    RCV INTERRUPT DISABLED             ;;
;; BIT [0]  : (1)    MASTER IRQ CONTROL ON              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx   #%00000011   
	stx   commandregister



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           RECEIVE AND TRANSMIT HIGH???               ;;
;; Mask out the statusregister just check bits 3 and 4  ;;
;; Are both Bits HIGH???? or JSR to LEAVE_NMI           ;;
;; BIT [7]  : (1)  Interrupt caused by glink232         ;;
;; BIT [6]  : (1)  Carrier present                      ;;
;; BIT [5]  : (1)  DSR - HIGH  (0) DSR - LOW            ;;
;; BIT [4]  : (1)  Ready to get next byte in data reg   ;;
;; BIT [4]  : (0)  Chip currently sending byte          ;;
;; BIT [3]  : (1)  Byte recieved in data register       ;;
;; BIT [3]  : (0)  Nothing received in data register    ;;
;; BIT [2]  : (1)  Overrun occured!!!                   ;;
;; BIT [1]  : (1)  Parity error!!!                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	and   #%00011000  
	beq   LEAVE_NMI



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   IS THERE A RECEIVE BYTE IN THE DATA REGISTER???    ;;
;; Mask out the statusregister just check bits 3        ;;
;; Is it high???                                        ;;
;; If the value from and = 0, then it is not a receive  ;;
;; branch off to transmit [SEND_TO_DATAREGISTER]        ;;
;; otherwise proceed onto RECEIVE code                  ;;
;; find out where the end of the receive buffer is,     ;;
;; store the byte at the end,                           ;;
;; incriment recvbuffer to next address,                ;;
;; and incriment our receive buffer current size so we  ;;
;; know how many bytes are in the buffer                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IS_RECEIVE_BYTE:                  
	and   #%00001000            ; Mask all but bit #3
	beq   SEND_TO_DATAREGISTER  ; If 0 branch to SEND_TO_DATAREGISTER
                                    ; If you are still here, it is not zero
                                    ; start receive action
	lda   dataregister          ; get received byte from dataregister
	ldy   recvtail              ; load index memory value of end of receive buffer to Y
	sta   (recvbuffer),y        ; and store it
	inc   recvtail              ; move index to next slot
	inc   recvbuffersize        ; increment count of bytes in receive buffer (if used by your program)  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SEE IF THE TRANSMIT BUFFER IS EMPTY OR NOT         ;;
;; If empty branch to LEAVE_NMI                         ;;
;; If not empty continue onto XMITBYTE                  ;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
IS_XMIT_NEEDED:
	lda   xmitbuffersize    
	beq   LEAVE_NMI  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SEE IF THE TRANSMIT BUFFER IS EMPTY OR NOT         ;;
;; If empty branch to LEAVE_NMI                         ;;
;; If not empty continue onto XMITBYTE                  ;;  
;; BIT [4]  : (1)  Ready to get next byte in data reg   ;;
;; BIT [4]  : (0)  Chip currently sending byte          ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IS_XMITBYTE_READY:
	lda   statusregister    ; We have to reload the status register
	and   #%00010000        ; Mask all but bit #4
                                ; if 0 its busy
                                ; if 1 its available continue on
	beq   LEAVE_NMI 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             STORE BYTE TO  DATAREGISTER              ;;
;; Get the location of xmithead and store it in Y,      ;;
;; load a with the transmit buffer offset by Y,         ;;
;; Store that byte in the dataregister,                 ;;
;; Incriment xmithead to the next memory location,      ;;
;; decriment buffer by 1 since we pushed a byte out,    ;;
;; load the new xmitbuffersize into A,                  ;;
;; if empty branch to LEAVE_NMI,                        ;;
;; if not empty load A with xmiton                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SEND_TO_DATAREGISTER:              
	ldy   xmithead
	lda   (xmitbuffer),y     ;get character at head of buffer
	sta   dataregister       ;place in ACIA for transmit
                                 ;point to next character in buffer
	inc   xmithead           ;and store new index
	dec   xmitbuffersize     ;subtract one from count of bytes in xmit buffer
	lda   xmitbuffersize
	beq   LEAVE_NMI 
	lda   xmiton             ;model to leave both interrupts enabled                             
	bne   NMICOMMAND         ;branch always to store model in command register   
                            


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              LEAVE NMI with xmitoff                  ;;
;; This loads xmitoff into a before going onto          ;;
;; NMICOMMAND and exiting the interrupt                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LEAVE_NMI:
	lda   xmitoff


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               STORE COMMANDREGISTER                  ;;
;; store A into commandregister, with either xmiton or  ;;
;; xmitoff depending on the condition that loaded       ;;
;; either/or into A                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NMICOMMAND:
	sta   commandregister 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   RELOAD PROCESSOR STATUS FROM BEFORE                ;;
;;Restore the processor status and contine onto the     ;;
;;NMI code from the KERNEL                              ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
RESTORE_FROM_NMI:                  
	pla                 
	tay
	pla                 
	tax
	pla                       
	rti
;	jmp (oldnmivector)  ; keep this for other NMI Code  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Radio specific code starts here.......                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ACIACODEFINISH: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     SPRITELOADER                     ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


SPRITELOADER:
	lda #$30
	sta OFFSET
	lda LEDDATA,x
	sta SPRITEDATA,x
	inx
	cpx #$40
	bne SPRITELOADER
	ldx #$00

SPRITESETUP:   ;$F2 red ;$F5 green $F1 white $F8 orange $F9 brown 

	lda SPRITECOLORS,x
	sta SP0COL,x
	lda #$80
	sta SSDP0,x
	inx
	cpx #$08
	bne SPRITESETUP
	ldx #$00
	ldy #$00
	clc

SPRITELOCATE:
	lda #$3F         ; updown
	sta SP0Y,x
	lda OFFSET       ; leftright    
	sta SP0X,x
	adc #$06
	sta OFFSET
	inx
	inx
	iny
	cpy #$08
	bne SPRITELOCATE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Load disk directory into  0800           ;;
;; Then we will check to see if "cal" is in the list    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOADDIR:
	loadaddr = $0800
	lda #dirfname_end-dirfname
	ldx #<dirfname
	ldy #>dirfname
	jsr $FFBD     				; call SETNAM
	lda #$01
	ldx $BA 			        ; last used device number
	bne dirskip
	ldx #$08      				; default to device 8
dirskip:   
	ldy #$00      				; not $01 means: load to address stored in file
	jsr $FFBA     				; call SETLFS
	ldx #<loadaddr
	ldy #>loadaddr
	lda #$00      				; $00 means: load to memory (not verify)
	jsr $FFd5     				; call LOAD ffd5
	bcs direrror    			; if carry set, a load error has happened
	jmp CHECKCONNECT
direrror:
        ; Accumulator contains BASIC error code
        ; most likely errors:
        ; A = $05 (DEVICE NOT PRESENT)
        ; A = $04 (FILE NOT FOUND)
        ; A = $1D (LOAD ERROR)
        ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
        ;... error handling ...
brk


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      CHECKCONNECT                    ;;
;; Check to see if the radio is properly connected by   ;;
;; sending data to it and expecting something back.     ;;
;; wait in loop for a response                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHECKCONNECT:
	lda #$93         ; #$93 clear screen
	jsr $ffd2
	ldx #$00
CHECKCONNECT2:
	lda CHKCONNECT,x
	jsr $FFD2
	inx
	cpx #$1F
	bne CHECKCONNECT2
	ldx #$00
    	jsr GETVERSION
	beq CHECKCONNECT 
    	cmp #$FF
    	beq CHECKCONNECT
	ldx #$00
    	jsr PRINTOK

   
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      CALCHECK                        ;;
;; check to see if calibration file exists in 0800 area ;;
;; try to read 'cal' after each quote, die when 'blo'   ;;
;; has been reached.                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx #$00
CALCHECK:
	lda CHKCAL,x
	jsr $FFD2
	inx
	cpx #$28
	bne CALCHECK
	ldx #$00
	
READDIR:
	lda $0800,x
	inx
	cmp #$42      ;B
	beq ISITEND
	cmp #$22      ;"
	bne READDIR 
	lda $0800,x
	inx
	cmp #$43      ;C 
	bne READDIR
	lda $0800,x
	inx
	cmp #$41      ;A
	bne READDIR
	lda $0800,x
	inx
	cmp #$4c      ;L     
	bne READDIR     
	lda #$31      ; number 1
	jmp CALSEARCHEND

ISITEND:              ; do we have BL
	lda $0800,x
	inx
	cmp #$4c      ;L
	bne READDIR
	lda $0800,x     
	inx     
	cmp #$4f     ; O     
	bne READDIR     
	lda #$30

CALSEARCHEND:
	cmp #$31
	beq CALFOUND
    	jmp CALNOTFOUND


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      CALFOUND                        ;;
;; If cal is found we jump here, say it is found and    ;;
;; continue on....                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CALFOUND: 
    	ldx #$00

CALFOUND1:    
	lda CALFOUNDTEXT,x
	jsr $FFD2
	inx
	cpx #$26
	bne CALFOUND1
	ldx #$00    
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
    	jmp START



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      CALNOTFOUND                     ;;
;; If cal is not found we jump here, say it is found    ;;
;; continue here                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CALNOTFOUND:
    	ldx #$00
    
CALNOTFOUND1:    
	lda CALNOTFOUNDTEXT,x
	jsr $FFD2
	inx
	cpx #$AF
	bne CALNOTFOUND1
	ldx #$00  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      YESORNO                         ;;
;; If no CAL found, should we make one, Y/N. If no      ;;
;; continue on, if yes, proceed to SAVECAL              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

YESORNO:
	jsr $FFE4         ; GETKEYIN      
	beq  YESORNO      ; No key, go back to PROMPT     
    	jsr $ffd2
    	cmp #$4E          ;N
    	beq NO
    	cmp #$59          ;Y
    	beq SAVECAL
    	jmp YESORNO

NO:
	jmp START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      SAVECAL                         ;;
;; Dump radio 00x07 to 00x52 into receive buffer        ;;
;; does two bytes at a time                             ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
SAVECAL:
	ldx #$00

DUMPDISPLAY:
	lda DUMPTEXT,x
	jsr $FFD2
	inx
	cpx #$1C
	bne DUMPDISPLAY
	lda #$00        ; reinitialize buffer to empty
	sta recvhead
	sta recvtail
	ldx #$07        ; 07
	stx $FB
    	ldx #$17
    	stx $FC

GETCALDATA:
	ldy #$00
	jsr TESTACIA
	ldy $FB
	jsr TESTACIA
	ldy #$00
	jsr TESTACIA
	ldy #$00
	jsr TESTACIA
	ldy #$BB
	jsr TESTACIA
	jsr PAUSE
	inc $FB
	inc $FB      ; does 2 bytes at a time, incriment twice
	jsr UPDATE
	ldx $FB
	cpx #$53     ; 53
	bne GETCALDATA
    	ldx #$00
	ldx updateline
	ldy #$18
	clc
	jsr $fff0
	ldx #$00
    	jsr PRINTOK
	ldx #$00

SAVECALDISP:
	lda SAVECALTEXT,x
	jsr $FFD2
	inx
	cpx #$1C
	bne SAVECALDISP

WRITECAL2DISK:
	lda #fname_end-fname
	ldx #<fname
	ldy #>fname
	jsr $FFBD     ; call SETNAM
	lda #$00      ;    was 00
	ldx $BA       ; last used device number
	bne writeskip
	ldx #$08      ; default to device 8
writeskip:   
	ldy #$00
	jsr $FFBA     ; call SETLFS
	lda #<RECEIVE_BUFFER
	sta $C1
	lda #>RECEIVE_BUFFER
	sta $C2
	ldx #<RECEIVE_BUFFER+76
	ldy #>RECEIVE_BUFFER+76
	lda #$C1          ; start address located in $C1/$C2
	jsr $FFD8         ; call SAVE
	bcs writeerror    ; if carry set, a load error has happened
	ldx updateline+1
	ldy #$18
	clc
	jsr $fff0
	ldx #$00
	jsr PRINTOK
	ldx #$00
	jmp HARDCOPYDISPLAY
writeerror:
        ; Akkumulator contains BASIC error code
        ;... error handling ...
        brk

fname:  
.text "calibration"
fname_end:

HARDCOPYDISPLAY:
	lda HARDCOPYTEXT,x
	jsr $FFD2
	inx
	cpx #$8B
	bne HARDCOPYDISPLAY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      PRINTYESORNO                    ;;
;; Print a hardcopy, Y/N. If no                         ;;
;; continue on, if yes, proceed to PRINTIT              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRINTYESORNO:
	jsr $FFE4              ; GETKEYIN      
	beq  PRINTYESORNO      ; No key, go back to PROMPT     
    	jsr $ffd2
    	cmp #$4E               ;N
    	beq PRINTNO
    	cmp #$59               ;Y
    	beq PRINTIT
    	jmp PRINTYESORNO

PRINTNO:
	jmp START

PRINTIT:
	lda $BA        ; Store the last used drive number to before the printer ruins it
	sta $FC
	lda #$04       ; LOGICAL FILE NUM
	ldx #$04       ; DEVICE NUM
	ldy #$00       ; SECONDARY ADDRESS
	jsr $FFBA      ; SETLFS    fe00
	jsr $FFC0      ; OPEN       f34a
	ldx #$04
	jsr $FFC9      ; opens 4 for output CHKOUT   f250

prierror:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           DRAW THE BANNER AND COLUMN NAMES           ;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx #$00
DRAWBANNER:
	lda TEXTBANNER,x
	jsr $FFD2
	inx
	cpx #$4D
	bne DRAWBANNER	
	ldx #$01
	stx $FB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       WRITEDATA                      ;;   
;;  FB STORES THE CURRENT MENU ITEM TO WORK WITH        ;;   
;; FB STARTS AT 1-->39 -->2--40 ETC ETC                 ;;   
;; 38 is half of total and is hex $26                   ;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WRITEDATA:
    	lda #$00    ; HIGH BYTE , ALWAYS 00
    	ldx $FB     ; LOW BYTE STORED IN FB
	jsr $BDCD   ; CONVERT TO DECIMAL

;;;;;;;;;;;;;;;;;TAB AND VALUE CODE HERE FOR LEFT ROW - 29

	lda #$10
	jsr $ffd2       ; CALL PRINTER TAB 
	lda #$32        ; 2
	jsr $ffd2
	lda #$32        ; 2
	jsr $ffd2       ; MOVE 22 POSITIONS FROM CR
	ldy $FB
	dey
	ldx RECEIVE_BUFFER,y
	lda #$00
	jsr $BDCD       ; print value in decimal
	lda #$10
	jsr $ffd2       ; CALL PRINTER TAB 
	lda #$34        ; 4
	jsr $ffd2
	lda #$38        ; 8
	jsr $ffd2       ; MOVE 48 POSITIONS FROM CR

	lda $FB     ; LOAD A WITH FB
	adc #$26    ; ADD HEX 26
	sta $FB     ; STORE BACK TO FB
	lda #$00    ; HIGH BYTE , ALWAYS 00
	ldx $FB     ; LOW BYTE STORED IN FB
	jsr $BDCD   ; CONVERT TO DECIMAL

;;;;;;;;;;;;;;;;;TAB AND VALUE CODE HERE FOR RIGHT ROW - 69

	lda #$10
	jsr $ffd2       ; CALL PRINTER TAB 
	lda #$36        ; 6
	jsr $ffd2
	lda #$39        ; 9
	jsr $ffd2       ; MOVE 69 POSITIONS FROM CR
	ldy $FB
	dey
	ldx RECEIVE_BUFFER,y
	lda #$00
	jsr $BDCD       ; print value in decimal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #$0D        ; CARRAGE RETURN
	jsr $ffd2       ; WHEN THE LINE IS DONE
	lda $FB         ; LOAD FB BACK INTO A
	sbc #$26        ; SUBTRACT 26
	sta $FB         ; STORE BACK TO FB
	inc $FB       
	inc $FB         ; INCRIMENT FB TWICE
	cmp #$25        ; IS IT EQUAL TO HEX 25
	bne WRITEDATA   ; NO, DO THE NEXT ROW

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

close:
	jsr $ffcc ;CLRCHN f333
	lda #$04
	jsr $ffc3 ;close  f291  
	lda $FC
	sta $BA   ; restore last drive number used before printer
	jmp START ; for now


UPDATE:
	ldx updateline
	ldy $FC                 ; Y coordunate for cursor   start is $18
	clc                     ; carry
	jsr $fff0               ; move cursor              
	lda #$20
	jsr $ffd2
	lda #$2a                ; top left curved end    was d5
	jsr $ffd2
	inc $FC
	lda $FC
	cmp #$1B
	beq REDO
	rts

REDO:
	lda #$20
	sta $0623
	lda #$17
	sta $FC
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here the main window of the program is pre-loaded for main.asm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

START:     ;;;;;;;;;;;;;;;;;;reload all memory after ACIACODEFINISH with file main

TOPWINDOWDRAW:
    	lda #$93
    	jsr $ffd2           ; clear screen   
	lda #$f0            ; top left curved end    was d5
	jsr $ffd2
	ldx #$00
	jsr WINDOWLINETOP
	lda #$ee            ; top right curved end  was c9
	jsr $ffd2
	ldx #$01
	jsr SIDEBARS
	lda #$ed            ; bottom left was ca
	jsr $ffd2
	jsr WINDOWLINEBOTTOM
	lda #$fd            ; bottom right was cb
	jsr $ffd2
    	ldx #$00
    	ldy #$00
	jmp SECONDWINDOW

SIDEBARS:
	ldy #$00                 ; Y coordunate for cursor
	clc                      ; carry
	jsr $fff0                ; move cursor  
	lda #$dd                 ; vertical bar left side was c7
	jsr $ffd2
	ldy #$27                 ;
	clc
	jsr $fff0                ; move cursor  
	lda #$dd                 ; vertical bar right side   was c8
	jsr $ffd2
	inx
	cpx #$04
	bne SIDEBARS
    	lda #$53                ;S
    	sta $047A
    	lda #$3A                ;:
    	sta $047B
	rts

WINDOWLINETOP:
	lda #$60               ; Give A an underline hex value   was 64
	jsr $ffd2              ;  Print one underline character
	inx                    ; X=X+1
	cpx #$26               ; does X = hex 28
	bne WINDOWLINETOP      ; branch to ULINE of not 28
	rts

WINDOWLINEBOTTOM:
	lda #$60               ; Give A an underline hex value was 63
	jsr $ffd2              ;  Print one underline character
	inx                    ; X=X+1
	cpx #$2A               ; does X = hex 28
	bne WINDOWLINEBOTTOM   ; branch to ULINE of not 28
	rts

SECONDWINDOW:
    	ldx #$05
	ldy #$00                 ; Y coordunate for cursor
	clc                      ; carry
	jsr $fff0                ; move cursor  
	lda #$f0                 ; top left curved end  
	jsr $ffd2
	ldx #$00
	jsr WINDOWLINETOP
	lda #$ee                 ; top right curved end
	jsr $ffd2
	ldx #$06
	jsr SECONDSIDEBARS
	lda #$ed             ; bottom left
	jsr $ffd2
	ldx #$04
	jsr WINDOWLINEBOTTOM
	lda #$fd             ; bottom right
	jsr $ffd2
	jmp LOADMAINFILE

SECONDSIDEBARS:
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
	cpx #$16
	bne SECONDSIDEBARS
    	lda #$53                ;S
    	sta $047A
    	lda #$3A                ;:
    	sta $047B
    	lda #$D0                ;P
    	sta $0490
	lda #$D7                ;W
	sta $0491
	lda #$D2                ;R
	sta $0492
	lda #$3A                ;:
	sta $0493
	ldx #$04	

	stx $D8F2               ; Color for N
	lda #$4E                ;N

	sta $04F2
	lda #$4F                ;O
	sta $04F3
	lda #$49                ;I
	sta $04F4
	lda #$53                ;S
	sta $04F5
	lda #$45                ;E
	sta $04F6
;	lda #$20                ;
;	sta $04F7               
	lda #$42                ;B
	sta $04F8
	lda #$4C                ;L
	sta $04F9               
	lda #$4B                ;K
	sta $04FA
	lda #$3A                ;:
	sta $04FB 

	ldx #$04	
	stx $D908               ; Color for R 

	lda #$52                ;R
	sta $0508
	lda #$46                ;F
	sta $0509
	lda #$2D                ;-
	sta $050A
	lda #$47                ;G
	sta $050B
	lda #$2F                ;/
	sta $050C
	lda #$53                ;S
	sta $050D
	lda #$51                ;Q
	sta $050E
	lda #$4C                ;L
	sta $051F
	lda #$3A                ;:
	sta $0511

	ldx #$04	
	stx $D91A               ; Color for A

	lda #$41                ;A
	sta $051A
	lda #$47                ;G
	sta $051B
	lda #$43                ;C
	sta $051C
	lda #$4D                ;M
	sta $051E
	lda #$4F                ;O
	sta $051F
	lda #$44                ;D
	sta $0520  
	lda #$45                ;E
	sta $0521
	lda #$3A                ;:
	sta $0523

	ldx #$04	
	stx $D930               ; Color for D

	lda #$44                ;D
	sta $0530               
	lda #$55                ;U
	sta $0531
	lda #$41                ;A
	sta $0532
	lda #$4C                ;L
	sta $0533
	lda #$57                ;W
	sta $0535
	lda #$43                ;C
	sta $0536
	lda #$48                ;H
	sta $0537
	lda #$3A                ;:
	sta $0539

	ldx #$04	
	stx $D942               ; Color for V

	lda #$56                ;V
	sta $0542
	lda #$4F                ;O
	sta $0543
	lda #$58                ;X
	sta $0544
	lda #$3A                ;:
	sta $054B

	ldx #$04	
	stx $D958               ; Color for K

	lda #$4B                ;K
	sta $0558
	lda #$59                ;Y
	sta $0559
	lda #$52                ;R
	sta $055A
	lda #$3A                ;:
	sta $0561          

	ldx #$04	
	stx $D96A               ; Color for B

	lda #$42                ;B
	sta $056A
	lda #$52                ;R
	sta $056B
	lda #$4B                ;K
	sta $056C
	lda #$49                ;I
	sta $056E
	lda #$4E                ;N
	sta $056F
	lda #$3A                ;:
	sta $0573

	ldx #$04	
	stx $D980              ; Color for C

	lda #$43               ;C
	sta $0580
	lda #$57               ;W
	sta $0581
	lda #$50               ;P
	sta $0583
	lda #$41               ;A
	sta $0584
	lda #$44               ;D D
	sta $0585                  
	sta $0586
	lda #$4C               ;L
	sta $0587
	lda #$45               ;E
	sta $0588
	lda #$3A               ;:
	sta $0589

	ldx #$04	
	stx $D992               ; Color for F

	lda #$46                ;F
	sta $0592
	lda #$41                ;A
	sta $0593
	lda #$53                ;S
	sta $0594
	lda #$54                ;T T
	sta $0595               
	sta $0597
	lda #$4E                ;N
	sta $0598
	lda #$3A                ;:
	sta $059B

	ldx #$04	
	stx $D9A8               ; Color for I

	lda #$49                ;I
	sta $05A8
	lda #$44                ;D
	sta $05A9               
	lda #$43                ;C
	sta $05AB               
	lda #$57                ;W
	sta $05AC
	lda #$3A                ;:
	sta $05B1


	ldx #$04	
	stx $D9BA               ; Color for F


	lda #$54                ;T
	sta $05BA
	lda #$2F                ;/
    	sta $05BB
	lda #$52                ;R
	sta $05BC
	lda #$53                ;S
	sta $05BE
	lda #$50                ;P
	sta $05BF
	lda #$4C                ;L
	sta $05C0
	lda #$49                ;I
	sta $05C1
	lda #$54                ;T
	sta $05C2
	lda #$3A                ;:
	sta $05C3


	lda #$49                ;I
	sta $05A8


	ldx #$04	
	stx $D9D0               ; Color for S

	lda #$53                ;S
	sta $05D0
	lda #$43                ;C
	sta $05D1
	lda #$41                ;A
	sta $05D2
	lda #$4E                ;N
	sta $05D3
	lda #$4D                ;M
	sta $05D5
	lda #$4F                ;O
	sta $05D6
	lda #$44                ;D
	sta $05D7
	lda #$45                ;E
	sta $05D8
	lda #$3A                ;:
	sta $05D9

	ldx #$02	
	stx $DA48               ; Color for F
    	stx $DA49

	lda #$C6                ;F   inv
	sta $0648
	lda #$B2
	sta $0649               ;2   inv
	lda #$41                ;A
	sta $064B
	lda #$3D                ;=
	sta $064C
	lda #$42                ;B
	sta $064D


	ldx #$02	
	stx $DA98               ; Color for F
    	stx $DA99


	lda #$C6                ;F   inv
	sta $0698
	lda #$B4
	sta $0699               ;4   inv
	lda #$43                ;C
	sta $069B
	lda #$48                ;H
	sta $069C
	lda #$41                ;A
	sta $069D
	lda #$52                ;R
	sta $069E
	lda #$47                ;G
	sta $069F
	lda #$45                ;E
	sta $06A0


	ldx #$02	
	stx $DAE8               ; Color for F
    	stx $DAE9


	lda #$C6                ;F   inv
	sta $06E8
	lda #$B6
	sta $06E9               ;6   inv
	lda #$48                ;H
	sta $06EB
	lda #$45                ;E
	sta $06EC
	lda #$4C                ;L
	sta $06ED
	lda #$50                ;P
	sta $06EE


	ldx #$02	
	stx $DB38               ; Color for F
    	stx $DB39


	lda #$C6                ;F   inv
	sta $0738
	lda #$B8
	sta $0739               ;8   inv

	lda #$43                ;C
	sta $073B
	lda #$48                ;H
	sta $073C
	lda #$45                ;E
	sta $073D
	lda #$43                ;C
	sta $073E
	lda #$4B                ;K
	sta $073F
	lda #$43                ;C
	sta $0741
	lda #$41                ;A
	sta $0742
	lda #$4C                ;L
	sta $0743



	ldx #$02	
	stx $DA32               ; Color for F
    	stx $DA33


	lda #$C6                ;F   inv
	sta $0632
	lda #$B1
	sta $0633               ;1   inv
	lda #$43                ;C
	sta $0635
	lda #$48                ;H
	sta $0636
	lda #$41                ;A
	sta $0637
	lda #$4E                ;N
	sta $0638
	lda #$47                ;G
	sta $0639
	lda #$45                ;E
	sta $063A
	lda #$46                ;F
	sta $063C
	lda #$52                ;R
	sta $063D
	lda #$45                ;E
	sta $063E
	lda #$51                ;Q
	sta $063F
	

	ldx #$02	
	stx $DA82               ; Color for F
    	stx $DA83

	lda #$C6                ;F   inv
	sta $0682
	lda #$B3
	sta $0683               ;3   inv
	lda #$54                ;T
	sta $0685
	lda #$4F                ;O
	sta $0686
	lda #$47                ;G G
	sta $0687
	sta $0688
	lda #$4C                ;L
	sta $0689
	lda #$45                ;E
	sta $068A
	lda #$4D                ;M
	sta $068C
	lda #$4F                ;O
	sta $068D
	lda #$44                ;D
	sta $068E
	lda #$45                ;E
	sta $068F

	ldx #$02	
	stx $DAD2               ; Color for F
    	stx $DAD3

	lda #$C6                ;F   inv
	sta $06D2	
	lda #$B5
	sta $06D3               ;5   inv

	lda #$54                ;T
	sta $06D5
	lda #$4F                ;O
	sta $06D6
	lda #$47                ;G G
	sta $06D7
	sta $06D8
	lda #$4C                ;L
	sta $06D9
	lda #$45                ;E
	sta $06DA
	lda #$56                ;V
	sta $06DC
	lda #$46                ;F
	sta $06DD
	lda #$4F                ;O
	sta $06DE


	ldx #$02	
	stx $DB22               ; Color for F
    	stx $DB23

	lda #$C6                ;F   inv
	sta $0722
	lda #$B7
	sta $0723               ;7   inv

	lda #$54                ;T
	sta $0725
	lda #$4F                ;O
	sta $0726
	lda #$47                ;G G
	sta $0727
	sta $0728
	lda #$4C                ;L
	sta $0729
	lda #$45                ;E
	sta $072A
	lda #$50                ;P
	sta $072C
	lda #$57                ;W
	sta $072D
	lda #$52                ;R
	sta $072E


	lda #$CC                ;L
	sta $0442
	lda #$CF                ;O
	sta $0443 
	lda #$C1                ;A
	sta $0444
	lda #$C4                ;D
	sta $0445
	lda #$C9                ;I
	sta $0446
	lda #$CE                ;N
	sta $0447
	lda #$C7                ;G
	sta $0448
	
	ldx #$05	
	stx $D838	
	stx $D839	
	stx $D83A	
	stx $D83B	
	stx $D83C	
	stx $D83D	
	stx $D83E	
	rts


LOADMAINFILE:

	lda #mainfname_end-mainfname
	ldx #<mainfname
	ldy #>mainfname
	jmp $0C00           ; LAUNCH LOADER FOR MAIN.prg


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 GETVERSION/READEEPROM                ;;
;;  READ EEPROM AT 00x04 AND 00x05                      ;;
;; Drops into radiobyte and radiobyte+1                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GETVERSION:     ;
	lda GET_VERSION,x  ; load the bytes in GET_VERSION  
	tay
	jsr TESTACIA       ; pass it to the acia
	inx
	cpx #$05
	bne GETVERSION
	ldy #$01
    	jsr PAUSE          ;need this becuase process it too fast
    	ldy #$00
    	lda (recvbuffer),y ; load in 1st byte from receive buffer
    	sta radiobyte
	iny
	lda (recvbuffer),y
	sta radiobyte,y
    	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       PRINTOK                        ;;
;; Prints the word [OK] and returns                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRINTOK:
	lda OK,x
	jsr $ffd2
	inx
	cpx #$04
	bne PRINTOK
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   WAIT/PAUSE LOOP                    ;;
;; The 6502 is too damn fast.  Need a delay between     ;;
;; requests to the radio                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WAIT:
	ldx #$00
	ldy #$00

PAUSE:
	nop
	inx
	cpx #$FF             ; was FF
	bne PAUSE
	iny
	cpy #$AA             ; was AA
	bne PAUSE
	ldx #$00
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      MAKE SURE STATUS REGISTER IS READY FOR TX       ;;
;; If so, send byte to dataregister                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TESTACIA:
     lda   statusregister
     and   #%00010000
     beq   TESTACIA       ;is bit #4 to be set, no? wait.
     sty   dataregister   ;give byte to GLINK232
     rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 BUFFERS, BYTES AND STUFF             ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

GET_VERSION:
.byte $00,$04,$00,$00,$BB        ; returns the two byte radio version

CHKCONNECT: 
.text "Checking for radio connection: "
CHKCAL:
.text $0D,"Checking for radio calibration file...",$0D

CALFOUNDTEXT:
.text $0D,"Calibration File Found.... [STARTING]",$0D

SAVECALTEXT:
.text $0D,"Saving Calibration---->   "

DUMPTEXT:
.text $0D,$0D,$0D,$0D,"Dumping Addresses-----> "

CALNOTFOUNDTEXT:
.text "Calibration File NOT Found.",$0D,$0D,$0D, "  It is important that you have a backupof the 817 calibration "
.text "data in the eventthat the calibration becomes corrupt!",$0D,$0D
.text "Backup Calibration? [Y/N] "

HARDCOPYTEXT:
.text $0D,$0D,$0D,"  Now would be a good time to print out a hardcopy for your records.  Make sure your printer is on and ready."
.text $0D,$0D,"Print Calibration? [Y/N] "

TEXTBANNER:
.text $0E,"   calibration settings for the ft817",$0F,$0D,$0D
.text "menuitem",$10,"21value",$10,"48menuitem",$10,"68value",$0D

OK:
.text "[OK]"

calibrationfname:  
 .text "calibration"
calibrationfname_end:

dirfname:  
.text $24,"calib*"  ;   was ,calib*        
dirfname_end:

mainfname:  .text "MAIN"
mainfname_end:

SPRITECOLORS: ;$F2 red ;$F5 green $F1 white $F8 orange $F9 brown 
.byte $F5,$F5,$F8,$F8,$F9,$F9,$F2,$F2

LEDDATA:
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$70,$00,$00,$70,$00,$00,$70,$00,$00,$70,$00
.byte $00,$70,$00,$00,$70,$00,$00,$70,$00,$00,$70,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

TRANSMIT_BUFFER = *+0
RECEIVE_BUFFER  = *+12
;* = * + (2*16) ; move program counter behind RECIEVE_BUFFER
