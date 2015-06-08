;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      C64 interface for the FT8XX Yaesu Radio         ;;
;;      Using the CAT command protocol                  ;;
;;      Jordan Rubin 2015                               ;;
;;      technocoma.blogspot.com                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     tcalchk.asm                      ;;
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
updateline         =   #$06
calloadaddr        =   $CE00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program startpoint                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*=  $C000      ;Start of program at 49152

	lda #$17 
	sta $D018        ;changes to upperlower
	lda #$93         ; #$93 clear screen
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
      
	lda   #%00001001    ; was 00001001
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
	sta   oldnmivector	  ;store it in oldnmivector
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
            
	jmp   ACIACODEFINISH 


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
                             ;;;;;;;;;;;;;;;;;;;;;;FIXAREA, CANT WE JUST STA CMMANDREGISTER 
                             ;;;;;;;;;;;;;;;;;;;;;; AND BRANCH TO RESTORE_FROM_NMI


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

LOADDIR:
	loadaddr = $0800
	lda #dirfname_end-dirfname
	ldx #<dirfname
	ldy #>dirfname
	jsr $FFBD     				; call SETNAM
	lda #$01
	ldx $BA 			        ; last used device number
	bne dirskip
	ldx #$08       				; default to device 8
dirskip:   
	ldy #$00      				; not $01 means: load to address stored in file
	jsr $FFBA     				; call SETLFS
	ldx #<loadaddr
	ldy #>loadaddr
	lda #$00      				; $00 means: load to memory (not verify)
	jsr $FFd5     				; call LOAD ffd5
	bcs direrror    			; if carry set, a load error has happened
	jmp CALCHECKSTART
direrror:
        ; Accumulator contains BASIC error code
        ; most likely errors:
        ; A = $05 (DEVICE NOT PRESENT)
        ; A = $04 (FILE NOT FOUND)
        ; A = $1D (LOAD ERROR)
        ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
        ;... error handling ...
brk

CALCHECKSTART:
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

ISITEND:         ; do we have BL
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

CALFOUND:
	ldx #$00

CALFOUNDLOOP:
	lda LOADINGLABEL,x
	jsr $FFD2
	inx
	cpx #$1D
	bne CALFOUNDLOOP

CALLOAD:
	lda #calfilename_end-calfilename
	ldx #<calfilename
	ldy #>calfilename
	
        jsr $FFBD     ; call SETNAM
        lda #$01
        ldx $BA       ; last used device number
        bne skip
        ldx #$08      ; default to device 8
skip:   
	    ldy #$00      ; $00 means: load to new address
        jsr $FFBA     ; call SETLFS

        ldx #<calloadaddr
        ldy #>calloadaddr
        lda #$00      ; $00 means: load to memory (not verify)
        jsr $FFD5     ; call LOAD
        bcs error    ; if carry set, a load error has happened	
        jmp READDATAPREP
error:
        ; Accumulator contains BASIC error code

        ; most likely errors:
        ; A = $05 (DEVICE NOT PRESENT)
        ; A = $04 (FILE NOT FOUND)
        ; A = $1D (LOAD ERROR)
        ; A = $00 (BREAK, RUN/STOP has been pressed during loading)

        ;... error handling ...
        brk	
	

READDATAPREP:
	lda #$07
	sta $FB
	ldx #$00
	stx $FC
	ldx #$00

DOWNLOADLABEL:
	lda DLLABEL,x
	jsr $FFD2
	inx
	cpx #$20
	bne DOWNLOADLABEL
	ldx updateline
	ldy #$00                 ; Y coordunate for cursor   start is $18
	clc                      ; carry
	jsr $fff0   
	jsr STATUSLINE
	ldx #$00

READDATALOOP:
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
	inc $FB                   ; does 2 bytes at a time, incriment twice
	jsr UPDATE
	ldx $FB
	cpx #$53     ; 53
	bne READDATALOOP	
	lda #$20
	sta $0516
	ldx #$04	
	ldy #$00                 ; Y coordunate for cursor   start is $18
	clc                      ; carry
	jsr $fff0                ; move cursor  
	ldx #$00


VERIFYLABELLOOP:                     
	lda VERIFYLABEL,x
	jsr $FFD2
	inx
	cpx #$1E
	bne VERIFYLABELLOOP
	ldx #$00                     

VERIFY:
	lda $CE00,x         
	sta $FB
	lda RECEIVE_BUFFER,x	
	cmp $FB	
	bne FAIL	
	inx
	cpx #$4C
	bne VERIFY
	jmp VERIFYGOOD    

FAIL: 
	ldx #$00

FAILLOOP:
	lda FAILLABEL,x         
	jsr $FFD2
	inx
	cpx #$84
	bne FAILLOOP

FAILWAIT:
	jsr $FFE4              ; GETKEYIN      
	cmp #$00
	beq  FAILWAIT          ; No key, go back to PROMPT 
	jsr LOADSTARTUPCHK


VERIFYGOOD:
	ldx #$00
	
VERIFYGOODLOOP:
	lda GOODLABEL,x
	jsr $FFD2
	inx
	cpx #$22
	bne VERIFYGOODLOOP
	ldx #$00

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
	jmp LOADSTARTUPCHK

PRINTIT:

	lda $BA      ; Store the last used drive number to before the printer ruins it
	sta $FC
	lda #$04     ; LOGICAL FILE NUM
	ldx #$04     ; DEVICE NUM
	ldy #$00     ; SECONDARY ADDRESS
	jsr $FFBA    ; SETLFS    fe00
	jsr $FFC0    ; OPEN       f34a
	ldx #$04
	jsr $FFC9    ; opens 4 for output CHKOUT   f250

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
	jmp LOADSTARTUPCHK 


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
	cmp #$CB
	beq REDO
	rts

REDO:
	lda #$20
	sta $0623
	lda #$00
	sta $FC
	rts

CALNOTFOUND:
	ldx #$00

CALNOTFOUNDLOOP:
	lda CALNOTFOUNDLABEL,x
	jsr $FFD2
	inx
	cpx #$3B
	bne CALNOTFOUNDLOOP

LOADSTARTUPCHK:
	lda #mainfname_end-mainfname
	ldx #<mainfname
	ldy #>mainfname
	jmp $0C00           ;;;;;;;;;; LAUNCH LOADER FOR MAIN

STATUSLINE:
	lda #$60               ; Give A an underline hex value   was 64
	jsr $ffd2              ;  Print one underline character
	inx                    ; X=X+1
	cpx #$2D               ; does X = hex 28
	bne STATUSLINE         ; branch to ULINE of not 28
	rts


CHKCAL:
.text $0D,"Checking for radio calibration file...",$0D

CALNOTFOUNDLABEL:
.text "Calibration file doesn't exist!!!",$0D,$0D,"Program will restart...."

LOADINGLABEL:
.text "Loading calibration file....",$0D

DLLABEL:
.text "Downloading calibration data...."

VERIFYLABEL:
.text "Verifying Calibration Data...."

FAILLABEL:
.text $0D,$0D,"Calibration verification Failed!!!",$0D,$0D,"Refer to your printed copy and check",$0D 
.text "your settings turning on the radio whileholding A B and C" 

GOODLABEL:
.text $0D,$0D,"Calibration verification Passed!"

TEXTBANNER:
.text $0E,"   calibration settings for the ft817",$0F,$0D,$0D
.text "menuitem",$10,"21value",$10,"48menuitem",$10,"68value",$0D

HARDCOPYTEXT:
.text $0D,$0D,$0D,"  Now would be a good time to print out a hardcopy for your records.  Make sure your printer is on and ready."
.text $0D,$0D,"Print Calibration? [Y/N] "

	mainfname:  .text "STARTUPCHK"
mainfname_end:

dirfname:  
.text $24,"calib*"    ; was ,calib*
dirfname_end:

calfilename:
.text "calibration"
calfilename_end:

TRANSMIT_BUFFER = *+0
RECEIVE_BUFFER  = *+12
;* = * + (2*16) ; move program counter behind RECIEVE_BUFFER
