;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      C64 interface for the FT8XX Yaesu Radio         ;;
;;      Using the CAT command protocol                  ;;
;;      Jordan Rubin 2015                               ;;
;;      technocoma.blogspot.com                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       main.asm                       ;;
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
ENABLECURSOR       =   $CC      ;00 enabled not 00 disabled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define FT817 LABELS HERE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

isoutput           =   $FD
radiobyte          =   $FB
currentmode        =   $3F
updateline         =   #$0D
eepromoffset       =   #$FD
vfoAstarthigh      =   #$00   
vfoAstartlow       =   #$7D
vfoBstarthigh      =   #$02
vfoBstartlow       =   #$03
memorystarthigh    =   #$04
memorystartlow     =   #$84
memjumplow         =   $2C    ; seems safe to use
memjumphigh        =   $2B    ; seems safe to use
tempjumplow        =   $2D     
tempjumphigh       =   $2E   
offsetfrombase     =   $2A

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program startpoint                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*=  $C000      		;Start of program at 49152

	lda #$17        ; changes to upperlower
	sta $D018 
	nop
	nop
	nop
	nop
	nop

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
            
	jmp ACIACODEFINISH


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

START:
	jmp GETID

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        UPDATE                        ;;
;; What gets done when the commandset is idle           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UPDATE:
	jsr GETMEMORVFO
	jsr GETCATFREQANDMODE
	jsr GETCATTXKEY
	jsr GETFURTHERINFO
	jsr CHECKINPUT
	jmp UPDATE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     GET CATTXKEY                     ;;
;; Call cat commmand for TXKEY                          ;;
;; CAT 00 00 00 00 F7 - xmit status                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GETCATTXKEY:
	jsr CLEARBUFFER
GETCATTXKEY2:    
	lda GET_TX,x   
	tay
	jsr TESTACIA      
	inx
	cpx #$05
	bne GETCATTXKEY2
    	jsr WAIT        
    	lda (recvbuffer),y
    	sta radiobyte+1
	and #%10000000
    	sta radiobyte
    	lda radiobyte+1
	and #%00001111
	sta radiobyte+1
	ldx #$00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  TXRXSEARCH/PRINT                    ;;
;; Search for and print TX or RX based on value in      ;;
;; radiobyte.                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TXRXSEARCH:
    	ldy TXRXLABEL,x
    	inx
	cpx #$FF            ; if X hits this thr receive data was garbage
	beq GETCATTXKEY     ; try the module over again
    	cpy radiobyte
    	bne TXRXSEARCH
    	ldy #$00

TXRXPRINT:    
	lda TXRXLABEL,x
	sta $049B,y
	inx
	iny
	cpy #02    
	bne TXRXPRINT



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ISITRXORTX                       ;;
;; Fork in the code, is the 817 in TX or RX mode        ;;
;; if TX continue on with TXINFO, if RX jump to RXINFO  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ISITRXORTX:
    	lda radiobyte
    	cmp #$80
    	beq RXINFOJUMP



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       TXINFO                         ;;
;; Get info from 00,00,00,00,BD                         ;;
;; Returns two bytes                                    ;;
;; 1st byte PWR and VSWR      is radiobyte+1            ;;
;; 2nd byte is ALC and MOD    is radiobyte              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TXINFO:
	jsr CLEARBUFFER
TXINFO2: 
	lda GET_TXMETER,x   
	tay
	jsr TESTACIA      
	inx
	cpx #$05
	bne TXINFO2
    	jsr WAIT  
	ldx #$00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 PWRSEARCH/PWRPRINT                   ;;
;; Compare radiobyte+1 data against PWR index to find   ;;
;; its value and print it.                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PWRSEARCH:
    	ldy PWR,x
    	inx
	cpx #$FF            ; if X hits this thr receive data was garbage
	beq TXINFO          ; try the module over again
    	cpy radiobyte+1
    	bne PWRSEARCH
    	ldy #$00

PWRPRINT:
	lda PWR,x
	sta $0494,y
	inx
	iny
	cpy #02
    	bne PWRPRINT	
	ldy #$00
    	lda (recvbuffer),y
	and #%00001111  
	cmp #$00  ; is 00, load SM0 
	beq SM0
	cmp #$02  ; is 0001  first bar  
	bcc SM1
    	beq SM1
    	cmp #$04  ; is 0010 second bar 
	bcc SM2
	beq SM2
	cmp #$06  ; is 0011 third bar
	bcc SM3
	beq SM3
	cmp #$08  ; is 0100 fourth bar
	bcc SM4
    	beq SM4
    	cmp #$0A   
	bcc SM5
	beq SM5
	cmp #$0C
	bcc SM6
	beq SM6
	cmp #$0D
	bcc SM7
	beq SM7
    	cmp #$0F
	bcc SM8
	beq SM8
    	rts

RXINFOJUMP:
	jmp RXINFO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 SPRITEMEMER CODE                     ;;
;; Updates enable sprite values for Smeter in RX or     ;;
;; SWR meter in TX                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SM0:
	lda #%00000000
	jmp METERUPDATE

SM1:
	lda #%00000001
	jmp METERUPDATE

SM2:
	lda #%00000011
	jmp METERUPDATE

SM3:
	lda #%00000111
	jmp METERUPDATE

SM4:
	lda #%00001111
	jmp METERUPDATE

SM5:
	lda #%00011111
	jmp METERUPDATE

SM6:
	lda #%00111111
	jmp METERUPDATE

SM7:
	lda #%01111111
	jmp METERUPDATE

SM8:
	lda #%11111111
	jmp METERUPDATE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       RXINFO                         ;;
;; Where we folked from ISITRXORTX continue here for    ;;
;; RX.  From 00,00,00,00,E7                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RXINFO:
	jsr CLEARBUFFER
RXINFO2:	
	lda GET_RX,x   
	tay
	jsr TESTACIA      
	inx
	cpx #$05
	bne RXINFO2
    	jsr WAIT  
    	lda (recvbuffer),y
	jsr CLEARPOWER  ; SQUELCH tag en/dis and clear power tag
	and #%00001111  ; check only 1st 4, S-Meter reading
	cmp #$00   
	beq SM0
	cmp #$02
	bcc SM1
    	beq SM1
    	cmp #$04
	bcc SM2
	beq SM2
	cmp #$06
	bcc SM3
	beq SM3
	cmp #$08
	bcc SM4
    	beq SM4
    	cmp #$0A
	bcc SM5
	beq SM5
	cmp #$0C
	bcc SM6
	beq SM6
	cmp #$0D
	bcc SM7
	beq SM7
    	cmp #$0F
	bcc SM8
	beq SM8



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     METERUPDATE                      ;;
;; Sends the value left in A to SPENA to turn on or     ;;
;; off the sprites for the meter                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

METERUPDATE:
	sta SPENA          
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         CLEARPOWER/SQUELCHSEARCH/SQUELCHPRINT        ;;
;; Blanks the power label, Enables or disables the      ;;
;; Squelch label.                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CLEARPOWER:
	sta radiobyte
	and #%10000000	
	sta $FD	
    	ldx #$00

SQUELCHSEARCH:
	ldy SQUELCHLABEL,x
	inx
	cpy $FD
	bne SQUELCHSEARCH
	ldy #$00		
				
SQUELCHPRINT:
	lda SQUELCHLABEL,x
	sta $0483,y
	inx
	iny
	cpy #07
    	bne SQUELCHPRINT		
	lda radiobyte
	rts
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   GETFREQANDMODE                     ;;
;;  RETURNS CAT COMMAND FOR FREQ AND MODE, ADDS DECIMAL ;;
;; Drops into FIXED POSITION AT  $043E,X                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GETCATFREQANDMODE:  
	jsr CLEARBUFFER
GETCATFREQANDMODE2:               ;  c139
	lda GET_CAT_FREQANDMODE,x ; load the bytes in GET_VERSION  
	tay
	jsr TESTACIA              ; pass it to the acia
	inx
	cpx #$05
	bne GETCATFREQANDMODE2
    	jsr WAIT                  ;need this becuase process it too fast

freqout:              
	lda (recvbuffer),y
	and #%11110000 
	lsr
	lsr
	lsr
	lsr
	adc #$30
	sta $0440,x
	inx
	cpx #$03      ; first decimal
	beq decimal
	clc

continue:
	lda (recvbuffer),y
	and #%00001111
	adc #$30
	sta $0440,x
	inx
	cpx #$07     ; second decimal
	beq decimal2
	clc

continue2:
	iny
	cpy #$04     ; all 5 numbers processed
	bne freqout	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    MODE  PORTION                     ;;
;;  Parse lookup table and add 3chr mode name at 044B,X ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda (recvbuffer),y
	sta $FB
	sta currentmode
    	ldx #$00

modesearch:
	ldy MODES,x
	inx
	cpx #$FF                  ; if X hits this thr receive data was garbage
	beq GETCATFREQANDMODE     ; try the module over again
	cpy $FB
	bne modesearch
	ldy #$00

modeprint:
	lda MODES,x
	sta $044B,y
	inx
	iny
	cpy #03
    	bne modeprint
	rts

decimal:
	clc
	lda #$2E
	sta $0440,x
	inx
	jmp continue

	decimal2:
	clc
	lda #$2E
	sta $0440,x
	inx
	jmp continue2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       GETMEMORVFO                    ;;
;; Get the setting for MEM/VFO bit 7 00x55 and          ;; 
;; Get VFO A or B bit 0 00x55                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GETMEMORVFO:
	jsr CLEARBUFFER
GETMEMORVFO2:
	lda GET_VFO_STATUS,x  
	tay
	jsr TESTACIA
	inx
	cpx #$05
	bne GETMEMORVFO2
	jsr WAIT
    	lda (recvbuffer),y ; load in ONLY 1st byte from receive buffer
    	sta radiobyte
	and #%10000000     ; is it Mem or VFO ; mem 0 vfo 1
	sta $FC
	ldx #$00

hometest:
	lda radiobyte   ; check to see if HOME is active, if so skip to it
	and #%00010000  ; home is 4th bit
	cmp #$10
	beq homeprint

memvfosearch:
    	ldy MEMVFO,x
    	inx
	cpx #$FF                  ; if X hits this thr receive data was garbage
	beq GETMEMORVFO           ; try the module over again
    	cpy $FC
    	bne memvfosearch
    	ldy #$00

memvfoprint:    
	lda MEMVFO,x
	sta $042A,y
	inx
	iny
	cpy #04    
	bne memvfoprint
	jmp VFOletter

homeprint:
    	lda HOME,x
	sta $042A,x
	inx
	cpx #$05
	bne homeprint
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              PRINTVFOLETTER or MEMCHAN #             ;;
;; If its VFO goto letter print routine if its MEM go   ;;
;; to code to get and print current memory channel      ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VFOletter:
	lda $FC            ; is it VFO A or B CURRENTMEMnumber is at 04x4F
	cmp #$80           ;is it vfo
    	beq PRINTVFOLETTER
    	lda #$20
    	sta $042E          ; blank out vfo letter
    	rts

PRINTVFOLETTER:
	lda radiobyte 
	and #$00000001
	cmp #$00
	beq printA
	lda #$42         ; PRINT B
	sta $042E
	jmp GETVFOBAND
printA:
	lda #$41         ; PRINT A
	sta $042E
	jmp GETVFOBAND

GETVFOBAND:
	jsr CLEARBUFFER
GETVFOBAND2:
	lda GET_VFO_BAND,x  
	tay
	jsr TESTACIA
	inx
	cpx #$05
	bne GETVFOBAND2
	jsr WAIT
    	lda (recvbuffer),y ; load in ONLY 1st byte from receive buffer
    	sta radiobyte
	ldx $042E
	cpx #$42
	beq GETVFOBANDb

GETVFOBANDa:
	lda vfoAstarthigh
	sta memjumphigh     ; adds inital nemory 00x7D to memjump
	lda vfoAstartlow
	sta memjumplow
	lda radiobyte
	and #%00001111
	sta $FC
	ldx #$00
	jmp GETVFOBANDSEARCH

GETVFOBANDb:
	lda vfoBstarthigh
	sta memjumphigh    ; adds inital nemory 02x73 to memjump
	lda vfoBstartlow
	sta memjumplow
	lda radiobyte
	and #%11110000     ; needs further conversion asr x 4?
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jmp GETVFOBANDSEARCH

GETVFOBANDSEARCH:
    	ldy VFOBANDS,x
    	inx
	cpx #$FF                  ; if X hits this the receive data was garbage
	beq GETVFOBAND            ; try the module over again
    	cpy $FC
    	bne GETVFOBANDSEARCH
	stx $FB                   ; X must be preserved during the JSR
	jsr UPDATEJUMPTABLEVFO    ; update memjumplow and memjumphigh for the band given the VF0
	ldx $FB                   ; Restore X from before
    	ldy #$00

VFOBANDPRINT:
	lda VFOBANDS,x
	sta $0431,y
	inx
	iny
	cpy #04    
	bne VFOBANDPRINT
	rts

UPDATEJUMPTABLEVFO:
;;;;;;;;;;;;;;;;;;;;;;;;load inital vfo offset add 26 [1A]for each value in FC
	ldx #$00
	cpy #$00
	bne JUMPTABLEVFOLOOP
	rts              ; if its 160m , $00 no further conversion required

JUMPTABLEVFOLOOP:        ;if not $00 we need to add 1A for each number greater than 0
	clc
	lda memjumplow
	adc #$1A 
	sta memjumplow 
	lda memjumphigh 
	adc #$00 
	sta memjumphigh 
	inx
	cpx $FC 
	bne JUMPTABLEVFOLOOP  
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    EZREADPREPARE                     ;;
;; Same as CLEARBUFFER, takes offsetfrombase and adds   ;;
;; it to  memjumplow and memjumphigh and stores new     ;;
;; value in tempjumplow tempjumphigh                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EZREADPREPARE:
	clc
	lda memjumplow
	adc offsetfrombase 
	sta tempjumplow 
	lda memjumphigh 
	adc #$00 
	sta tempjumphigh 
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
    	ldy #$00
	rts   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       EZREAD                         ;;
;; After EZREADPREPARE dumps new offsets from           ;;     
;; offsetfrombase, add offset to A templomplow/high     ;;
;; tempjumplow,tempjumphigh,00,00,BB to the acia        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EZREAD:
	ldy tempjumphigh
	jsr TESTACIA
	ldy tempjumplow
	jsr TESTACIA
	ldy #$00	
	jsr TESTACIA	
	jsr TESTACIA	
	ldy #$BB		
	jsr TESTACIA					
	jsr WAIT
	ldy #$01
    	lda (recvbuffer),y ; load in 2nd byte from receive buffer
	sta radiobyte,y
	dey
	lda (recvbuffer),y ; load in 1st byte from receive buffer
	sta radiobyte,y
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;; 2 bytes come back, may be second!!!!!!!!!!!!!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   GETFURTHERINFO                     ;;
;; This is where we get more stuff that needs offset    ;;
;; memory locations from the memjumplow and memjumphigh ;;
;; vectors, works with VFO and Memory                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GETFURTHERINFO:
	jsr GET57
	jsr GET58
	jsr GET5D
	jsr GET5F
	jsr GET79
	jsr GET7A
	jsr GET7B
	rts


	
;---------------------------------------------------------------------------------

GET79:
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$79
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte
	and #%00000011    ; GET tx power all bands
	sta $FC
	ldx #$00
	
CONFIGPOWERSEARCH:
    	ldy CFGPOWER,x
    	inx
	cpx #$FF                  ; if X hits this the receive data was garbage
	beq GET79                 ; try the module over again
    	cpy $FC
    	bne CONFIGPOWERSEARCH
	ldy #$00	
	
CONFIGPOWERPRINT:
	lda CFGPOWER,x
	sta $0494,y
	inx
	iny
	cpy #02    
	bne CONFIGPOWERPRINT		

GETDW:
	lda radiobyte
	and #%00010000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETDWPRINT:
	lda ONOFFLABEL,x
	sta $053A,y
	inx
	iny
	cpy #03    
	bne GETDWPRINT

GETSCANMODE:
	lda radiobyte
	and #%01100000
	sta $FC
	ldx #$00

GETSCANSEARCH:	
    	ldy SCANMODELABEL,x
    	inx
    	cpy $FC
    	bne GETSCANSEARCH
	ldy #$00		

SCANMODEPRINT:
	lda SCANMODELABEL,x
	sta $05DA,y
	inx
	iny
	cpy #03    
	bne SCANMODEPRINT
	rts

;---------------------------------------------------------------------------------
	
GET57:	
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$57
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte		
	
GETAGC:
	and #%00000011
	sta $FC
	ldx #$00
	
AGCLABELSEARCH:
    	ldy AGCLABEL,x
    	inx
	cpx #$FF                  ; if X hits this the receive data was garbage
	beq GET57                 ; try the module over again
    	cpy $FC
    	bne AGCLABELSEARCH
	ldy #$00
	
AGCLABELPRINT:
	lda AGCLABEL,x
	sta $0524,y
	inx
	iny
	cpy #04    
	bne AGCLABELPRINT

GETNB:
	lda radiobyte
	and #%00100000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETNBPRINT:
	lda ONOFFLABEL,x
	sta $04FC,y
	inx
	iny
	cpy #03    
	bne GETNBPRINT

GETFAST:
	lda radiobyte
	and #%10000000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	eor #%00000001    ; Bit is backwards 00 is on, so flip
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETFASTPRINT:
	lda ONOFFLABEL,x
	sta $059C,y
	inx
	iny
	cpy #03    
	bne GETFASTPRINT
	rts

;---------------------------------------------------------------------------------
	
GET58:	
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$58
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte	
	
GETKYR:
	and #%00010000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETKYRPRINT:
	lda ONOFFLABEL,x
	sta $0562,y
	inx
	iny
	cpy #03    
	bne GETKYRPRINT

GETBK:
	lda radiobyte
	and #%00100000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH

GETBKPRINT:
	lda ONOFFLABEL,x
	sta $0574,y
	inx
	iny
	cpy #03    
	bne GETBKPRINT

GETVOX:
	lda radiobyte
	and #%10000000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH

GETVOXPRINT:
	lda ONOFFLABEL,x
	sta $054C,y
	inx
	iny
	cpy #03    
	bne GETVOXPRINT

GETCWPADDLE:
	lda radiobyte
	and #%00000100    ; GET tx power all bands
	sta $FC
	ldx #$00

GETCWPADDLESEARCH:
    	ldy CWPADDLELABEL,x
    	inx
	cpx #$FF                  ; if X hits this the receive data was garbage
	beq GET58                 ; try the module over again
    	cpy $FC
    	bne GETCWPADDLESEARCH
	ldy #$00

GETCWPADDLEPRINT:
	lda CWPADDLELABEL,x
	sta $058A,y
	inx
	iny
	cpy #04    
	bne GETCWPADDLEPRINT
	rts

;---------------------------------------------------------------------------------

GET5D:
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$5D
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte

GETIDCW:
	and #%00010000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETIDCWPRINT:	
	lda ONOFFLABEL,x
	sta $05B2,y
	inx
	iny
	cpy #03    
	bne GETIDCWPRINT
	rts

;---------------------------------------------------------------------------------

GET5F:
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$5F
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte

GETRFSQL:
	and #%10000000    ; GET tx power all bands
	sta $FC
	ldx #$00

GETRFSQLSEARCH:
    	ldy RFSQLLABEL,x
    	inx
    	cpy $FC
    	bne GETRFSQLSEARCH
	ldy #$00

GETRFSQLPRINT:
	lda RFSQLLABEL,x
	sta $0512,y
	inx
	iny
	cpy #03    
	bne GETRFSQLPRINT
	rts

;---------------------------------------------------------------------------------

GET7A:
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$7A
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte

GETSPLIT:
	lda radiobyte
	and #%10000000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETSPLITPRINT:
	lda ONOFFLABEL,x
	sta $05C4,y
	inx
	iny
	cpy #03    
	bne GETSPLITPRINT	
	rts

;---------------------------------------------------------------------------------

GET7B:
	ldy #$00      ; reinitialize buffer after send, we can find a better place
	sty recvhead
	sty recvtail
	sty tempjumphigh
	ldy #$7B
	sty tempjumplow
	jsr EZREAD
    	sta radiobyte

GETCHARGER:
	and #%00010000    ; GET tx power all bands
	lsr
	lsr
	lsr
	lsr
	sta $FC
	ldx #$00
	jsr ONOFFSEARCH
	
GETCHARGERPRINT:	
	lda ONOFFLABEL,x
	sta $06A2,y
	inx
	iny
	cpy #03    
	bne GETCHARGERPRINT
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       CHECKINPUT                     ;;
;; Was a key pressed, if no contine if yes go to        ;;                    
;; respondtoinput.                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHECKINPUT:
	jsr $FFE4        ; GETKEYIN is F13E
	cmp  #$00      
	bne RESPONDTOINPUT
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    RESPONDTOINPUT                    ;;
;; Where we branch based on keystroke using jumppoints  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RESPONDTOINPUT:
	cmp #$03        ; STOP key, go to TERMEXIT
	beq TERMEXIT 
	cmp #$85        ; F1
	beq CHANGEFREQUENCYJUMP
	cmp #$89        ; F2
    	beq AEQUALSBJUMP
	cmp #$86        ; F3
	beq TOGGLEMODEJUMP
	cmp #$8A        ; F4
	beq TOGGLECHGJUMP
	cmp #$87        ; F5
	beq TOGGLEVFOJUMP
	cmp #$8B        ; F6
	beq HELPJUMP
	cmp #$88        ; F7
	beq TOGGLEPWRJUMP
	cmp #$8C        ; F8
	beq CALCHKJUMP
	cmp #$4E        ; N
	beq NBTOGGLEJUMP
	cmp #$56        ; V
	beq VOXTOGGLEJUMP
	cmp #$42        ; B
	beq BKTOGGLEJUMP
	cmp #$52        ; R
	beq RFSQLTOGGLEJUMP
	cmp #$4B        ; K
	beq KYRTOGGLEJUMP
	cmp #$46        ; F
	beq FASTTOGGLEJUMP
	cmp #$54        ; T
	beq SPLITTOGGLEJUMP
	cmp #$44        ; D
	beq DWTOGGLEJUMP
	cmp #$43        ; C
	beq CWPADDLETOGGLEJUMP
	cmp #$49        ; I
	beq IDCWTOGGLEJUMP
	cmp #$41        ; A
	beq AGCTOGGLEJUMP
	cmp #$53        ; S
	beq SCANTOGGLEJUMP
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        IF THE STOP KEY IS PRESSED, THEN EXIT         ;;         
;; Turn off GLINK interrupts                            ;;
;; Restore old vectors                                  ;;
;; Exit                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TERMEXIT:      
	lda #$00
	sta SPENA
	lda commandregister
	ora #%00000010
	sei
	sta   commandregister
	cli
	rts  


;-----------------JUMPPOINTS-----------------------------


CHANGEFREQUENCYJUMP:
	jmp CHANGEFREQUENCY

AEQUALSBJUMP:
	jmp AEQUALSB

TOGGLEMODEJUMP:
	jmp TOGGLEMODE

TOGGLECHGJUMP:
	jmp TOGGLECHG

TOGGLEVFOJUMP:
	jmp TOGGLEVFO

HELPJUMP:
	jmp HELP
	
CALCHKJUMP:	
	jmp CALCHK	

TOGGLEPWRJUMP:
	jmp TOGGLEPWR

NBTOGGLEJUMP:
	jmp NBTOGGLE

VOXTOGGLEJUMP:
	jmp VOXTOGGLE
	
BKTOGGLEJUMP:	
	jmp BKTOGGLE	
	
RFSQLTOGGLEJUMP:	
	jmp RFSQLTOGGLE	

KYRTOGGLEJUMP:
	jmp KYRTOGGLE

FASTTOGGLEJUMP:
	jmp FASTTOGGLE

SPLITTOGGLEJUMP:
	jmp SPLITTOGGLE
	
DWTOGGLEJUMP:	
	jmp DWTOGGLE	

CWPADDLETOGGLEJUMP:
	jmp CWPADDLETOGGLE

IDCWTOGGLEJUMP:
	jmp IDCWTOGGLE

AGCTOGGLEJUMP:
	jmp AGCTOGGLE
	
SCANTOGGLEJUMP:	
	jmp SCANTOGGLE	

FORCEUPDATE:
	jmp GETFURTHERINFO

;---------------END JUMPPOINTS---------------------------




	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ONOFFSEARCH                    ;;
;; Generic ON OFF search 00 OFF 01 ON                   ;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
ONOFFSEARCH:	
    	ldy ONOFFLABEL,x
    	inx
	cpx #$FF                  ; if X hits this the receive data was garbage
	beq FORCEUPDATE           ; try the module over again
    	cpy $FC
    	bne ONOFFSEARCH
	ldy #$00	
	rts
	
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       CLEARBUFFER                    ;;
;; Clears receive buffer, and clears X,Y,A to $00       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CLEARBUFFER:
	ldx #$00
	lda #$00      ; reinitialize buffer after send, we can find a better place
	sta recvhead
	sta recvtail
    	ldy #$00
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
	cpx #$CF      ; was FF
	bne PAUSE
	iny
	cpy #$14      ; was AA    good at 19
	bne PAUSE
	ldx #$00
    	ldy #$00
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      TOGGLEVFO                       ;;
;; Switches between VFO A and B                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TOGGLEVFO:
	jsr CLEARBUFFER
TOGGLEVFO2:
	lda SET_TOGGLE_VFO,x  
	tay	
	jsr TESTACIA
	inx
	cpx #$05
	bne TOGGLEVFO2
	jsr WAIT
	jsr WAIT
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      TOGGLEMODE                      ;;
;; Switches between Modes in ascending order            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TOGGLEMODE:
	lda currentmode
	cmp #$06         ; is WFM?
	beq PUSHWFM
	cmp #$0C         ; is PKT?     was 0C
	beq MODERESET
	cmp #$FC
	beq MODERESET
	ldx #$00

TOGGLESEARCH:
	lda OPMODES,x	
	inx
	cmp currentmode
	bne TOGGLESEARCH
	inx
	lda OPMODES,x

DOMODETOGGLE:
	tay
	jsr TESTACIA
	ldy #$00
	jsr TESTACIA
	jsr TESTACIA
	jsr TESTACIA
	ldy #$07
	jsr TESTACIA
	jsr WAIT
	jsr WAIT
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      MODERESET                       ;;
;; Roll over back to 00, LSB if you pass 0C, PKT.       ;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

MODERESET:
	lda #$00              ; If at PKT 0C push to 00, LSB
	jmp DOMODETOGGLE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       PUSHWFM                        ;;
;; Push past mode 06, since it an invalid opmode and    ;;
;; change the next one in the list to 08, FM            ;;                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                

PUSHWFM:                  ; if current mode is WFM push to FM
	lda #$08
	sta currentmode
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  SETCURSORSTATUSLINE                 ;;
;; Has kernel move cursor into statusline position on   ;;
;; the bottom of the program.                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SETCURSORTOSTATUSLINE:
    	ldx #$17
	ldy #$02                 ; Y coordunate for cursor
	clc                      ; carry
	jsr $FFF0                ; move cursor  
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     CLEARSTATUSLINE                  ;;
;; Clears the bottom status line                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CLEARSTATUSLINE:
	lda #$20
	ldx #$00

CLEARSTATUSLINELOOP:
	sta $079A,x
	inx
	cpx #$26
	bne CLEARSTATUSLINELOOP
	rts



;--------------BEGINNING OF CHANGE FREQUENCY--------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     CHANGEFREQUENCY                  ;;
;; CAT 12 34 56 78 WITH $01 TO CHANGE                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHANGEFREQUENCY:
	jsr SETCURSORTOSTATUSLINE
	ldx #$00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   DRAWFREQUENCYLABEL                 ;;
;; Draws input frequency dialogue in status location    ;;
;; INITS X and Y and stores them in $2C and $2B         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
DRAWFREQUENCYLABEL:
	lda FREQUENCYLABEL,x	
	jsr $FFD2
	inx
	cpx #$17
	bne DRAWFREQUENCYLABEL
	ldy #$00
	ldx #$00
	sty $2B
	stx $2C
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    FREQINPUTWAIT                     ;;
;; This processes the 10's position, or the odd inputs. ;;
;; Validates the input, 30-39, subtracts $30, puts it   ;;
;; in $FB,X for further adding to 1's position.         ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 		
	
FREQINPUTWAIT:            ; using FB FC FD FE
	jsr $FFE4
	cmp #$00
	beq FREQINPUTWAIT
	cmp #$30
	bcc FREQINPUTWAIT     ;if less than 30
	cmp #$40
	bcs FREQINPUTWAIT     ;if greater than or equal to 40
	sec
	ldy $2B
	ldx $2C
	sta $07B1,y
	sbc #$30            ; convert to digit value, 30 = 00 31 = 01 etc
	sta $FB,x
	asl
	asl
	asl                 ; multiply by $10
	asl
	sta $FB,x           ; store value in FB
	iny
	sty $2B
	cpy #$03
	beq FREQDECIMAL     ; add first decimal?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   FREQINPUTWAIT2                     ;;
;; This processes the 1's position, or the even inputs. ;;
;; Validates the input, 30-39, subtracts $30, adds it   ;;
;; to $FB holding the 10's stores it in $FB,X           ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

FREQINPUTWAIT2:
	jsr $FFE4
	cmp #$00
	beq FREQINPUTWAIT2
	cmp #$30
	bcc FREQINPUTWAIT2     ;if less than 30
	cmp #$40
	bcs FREQINPUTWAIT2     ;if greater than or equal to 40
	sec
	ldy $2B
	ldx $2C
	sta $07B1,y
	sbc #$30
	clc
	adc $FB,x
	sta $FB,x
	inx
	iny
	sty $2B
	stx $2C
	cpy #$07              ; add second decimal?
	beq FREQDECIMAL2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      FREQRETURN                      ;;
;; Checks for 4 complete itterations, if not go back to ;;
;; FREQINPUTWAIT for next 10's position and init X.     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FREQRETURN:
	cpx #$04
	bne FREQINPUTWAIT
    	ldx #$00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       FREQLOAD                       ;;
;; When all vaid numeral are in place from input, will  ;;
;; send FB,FC,FD,FE,$01 to acia for freq change, then   ;;
;; jump to CLEARSTATUSLINE                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FREQLOAD:
	ldy $FB,x
	inx
	jsr TESTACIA
	cpx #$04
	bne FREQLOAD
	ldy #$01
	jsr TESTACIA
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr CLEARSTATUSLINE
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      FREQDECIMAL                     ;;
;; Prints 1st decimal in input frequency in 3rd place   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FREQDECIMAL:
	lda #$2E
	sta $07B1,y
	iny
	sty $2B
	jmp FREQINPUTWAIT2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      FREQDECIMAL2                    ;;
;; Prints 2nd decimal in input frequency in 7th place   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FREQDECIMAL2:
	lda #$2E
	sta $07B1,y
	iny
	sty $2B
	jmp FREQRETURN



;------------------END OF CHANGE FREQUENCY----------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       NBTOGGLE                       ;;
;; Toggles Noiseblock on or off from 00x57 bit 5        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NBTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$57
	sty tempjumplow
	jsr EZREAD
	eor #%00100000     ;Flip bit 5 only to toggle NB
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       VOXTOGGLE                      ;;
;; Toggles VOX on or off from 00x58 bit 7               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VOXTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$58
	sty tempjumplow
	jsr EZREAD
	eor #%10000000     ;Flip bit 7 only to toggle VOX
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       BKTOGGLE                       ;;
;; Toggles BK on or off from 00x58 bit 5                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BKTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$58
	sty tempjumplow
	jsr EZREAD
	eor #%00100000     ;Flip bit 5 only to toggle BK
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    RFSQLTOGGLE                       ;;
;; Toggles RFSQL on or off from 00x5F bit 7             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RFSQLTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$5F
	sty tempjumplow
	jsr EZREAD
	eor #%10000000     ;Flip bit 7 only to toggle RFSQL
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     KYRTOGGLE                        ;;
;; Toggles CW KEYER on or off from 00x58 bit 4          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

KYRTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$58
	sty tempjumplow
	jsr EZREAD
	eor #%00010000     ;Flip bit 4 only to toggle KYR
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     FASTTOGGLE                       ;;
;; Toggles FAST TUNER on or off from 00x57 bit 7        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FASTTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$57
	sty tempjumplow
	jsr EZREAD
	eor #%10000000     ;Flip bit 7 only to toggle FAST TUNE
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     SPLITTOGGLE                      ;;
;; Toggles SPLIT MODE on or off from 00x7A bit 7        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SPLITTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$7A
	sty tempjumplow
	jsr EZREAD
	eor #%10000000     ;Flip bit 7 only to toggle SPLIT
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      CHGTOGGLE                       ;;
;; Toggles SPLIT MODE on or off from 00x7B bit 4        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TOGGLECHG:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$7B
	sty tempjumplow
	jsr EZREAD
	eor #%00010000     ;Flip bit 4 only to toggle CHARGER
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts		
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       DWTOGGLE                       ;;
;; Toggles DUAL WATCH on or off from 00x79 bit 4        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DWTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$79
	sty tempjumplow
	jsr EZREAD
	eor #%00010000     ;Flip bit 4 only to toggle DW
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts	
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    CWPADDLETOGGLE                    ;;
;; Toggles CWPADDLE NORM/REV from 00x58 bit 2           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CWPADDLETOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$58
	sty tempjumplow
	jsr EZREAD
	eor #%00000100     ;Flip bit 2 only to toggle CWPADDLE
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts		
	

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      IDCWTOGGLE                      ;;
;; Toggles CW ID on or off from 00x5D bit 4             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IDCWTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$5D
	sty tempjumplow
	jsr EZREAD
	eor #%00010000     ;Flip bit 4 only to toggle IDCW
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts		
	
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      AGCTOGGLE                       ;;
;; Toggles AGC modes from 00x57 bits 1-0                ;;       
;; mask off bits 0 and 1 to get agc value, if 3 jump    ;;
;; to agczero to turn next value back to zero, if not   ;;
;; 3 increment by 1.  load original 0057 value from     ;;
;; radiobyte and shut off 1st 2 bits.  ORA our bit in   ;;
;; and resave to radiobyte.  push to EZWRITE            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

AGCTOGGLE:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$57
	sty tempjumplow
	jsr EZREAD       ; value of 57 in A
	and #%00000011   ; mask off all but 0 and 1
	sta $FD          ; store AGC values in FD
	cmp #%00000011   ; is it 3, jumpto agczero
	beq agczero 
	inc $FD          ; not 3, increment value in FD
                         ; need to mask $FD value onto radiobyte
                     
continueagc:                     
	lda radiobyte
	and #%11111100
	ora $FD
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts			
	
agczero:	
	lda #$00	
	sta $FD	
	jmp continueagc	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      SCANTOGGLE                      ;;	
;; Toggles SCAN off,up,down from 00x75 bits 6 and 5     ;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
SCANTOGGLE:	   
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$79
	sty tempjumplow
	jsr EZREAD       ; value of 79 in A
	and #%01100000   ; mask off all but 6 and 5
	sta $FD          ; store SCAN values in FD
	cmp #%01100000   ; is it $60 jumpto scanzero
	beq scanzero
	cmp #%00000000   ; is it $00 jumpto scan2
	beq scantwo
	cmp #%01000000   ; is it $40 jumpto scan3
	beq scanthree

continuescan:
	sta $FD
	lda radiobyte
	and #%10011111
	ora $FD
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts	
	
scanzero:	
	lda #$00		
	jmp continuescan	
	
scantwo:
	lda #$40
	jmp continuescan

scanthree:
	lda #$60
	jmp continuescan



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        GETID                         ;; 
;; Retrieve the CWID in OPT19 from 19x22 - 19x28        ;; 
;; Convert based on number,letter, or blank. store in   ;; 
;; top menubar                                          ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GETID:
	ldx #$22
	stx $FE
	ldx #$00
	stx $FD

getidloop:		
	jsr CLEARBUFFER
	ldy #$19
	sty tempjumphigh
	ldy $FE
	sty tempjumplow
	jsr EZREAD
	lda radiobyte

idconversion:
	cmp #$24                ; is it less than $24 (not blank)
	bcc notblank

isblank:	
	lda #$20                ; use $20 PETSCII blank code
	jmp idcontinue
	
notblank:
	cmp #$0A                ; less than 0A its a number
    	bcc isnumber
    	clc
    	adc #$37                ; adc for PETSCII letter
    	jmp idcontinue
	   
isnumber:    
	clc
	adc #$30                ; adc for PETSCII number
	jmp idcontinue

idcontinue:
	ldx $FD
	sta $0438,x             ;store final value on menubar
	inc $FE
	inx
	stx $FD
	cpx #$07                ; happens only when getlastbit is done
	beq idfinished
	cpx #$06                ; general loop for 1st 6 letters
	bne getidloop
	jmp getlastidbit        ; break out here after 6th letter

idfinished:
	jmp UPDATE

getlastidbit:
	jsr CLEARBUFFER         ; retreive only 2nd byte from 19x27
	ldy #$19
	sty tempjumphigh
	ldy #$27
	sty tempjumplow
	jsr EZREAD
	clc
	lda radiobyte+1         ; grab that second byte (1928) do conv.
	jmp idconversion



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       A EQUALS B                     ;;
;; Copy all values from VFOA and save them to VFOB      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ABABORT:
	jsr CLEARSTATUSLINE
	jmp UPDATE

AEQUALSB:
	jsr SETCURSORTOSTATUSLINE
	ldx #$00

DRAWAEQUALSBLABEL:
	lda AEQUALSBLABEL1,x	
	jsr $FFD2
	inx
	cpx #$23
	bne DRAWAEQUALSBLABEL

ABWAITINPUT:
	jsr $FFE4
	cmp #$00
	beq ABWAITINPUT
	cmp #$4E            ;N
	beq ABABORT
	cmp #$59            ;Y
	beq ABPREPARE
    	jmp ABWAITINPUT

ABPREPARE:
	jsr CLEARSTATUSLINE


ABisitB:
	lda $042E         ; A and B letter for vfo
	cmp #$42
	bne ABSTARTACTION
	jsr TOGGLEVFO
	jsr WAIT


ABSTARTACTION:                     
	jsr CLEARBUFFER
	ldy #$00
	sty tempjumphigh
	ldy #$59
	sty tempjumplow
	jsr EZREAD
	lda radiobyte
	lda vfoAstarthigh
	sta memjumphigh     ; adds inital nemory 00x7D to memjump
	lda vfoAstartlow
	sta memjumplow
	lda radiobyte
	and #%00001111
	sta $FC
	ldx #$00

ABBANDSEARCH:             
    	ldy VFOBANDS,x
    	inx
    	cpy $FC    
    	bne ABBANDSEARCH
	jsr UPDATEJUMPTABLEVFO ; update memjumplow and memjumphigh for the band given the VF0
    	ldy #$00

ADUMP:    
	jsr SETCURSORTOSTATUSLINE
	ldx #$00

DUMPLABEL:	
	lda AEQUALSBLABEL2,x	
	jsr $FFD2
	inx
	cpx #$26
	bne DUMPLABEL
	jsr TOGGLEVFO
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr TOGGLEVFO
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	jsr WAIT
	lda #$00        ; reinitialize buffer to empty
	sta recvhead
	sta recvtail
	ldx #$00



DUMPCOPYLOOP:	; memjumplow/memjumphigh is the starting address 26 bytes 2b 2c
	stx $FB
	ldy memjumphigh
	jsr TESTACIA
	ldy memjumplow
	jsr TESTACIA
	ldy #$00
	jsr TESTACIA
	jsr TESTACIA
	ldy #$BB
	jsr TESTACIA
    	jsr WAIT
    	jsr WAIT
	jsr WAIT
	clc
	lda memjumplow
	adc #$02          ; add 2 to memjump low
	sta memjumplow	
	lda memjumphigh
	adc #$00          ; carry to memjumphigh as needed
	sta memjumphigh
	ldx $FB
	inx
	cpx #$0D
	bne DUMPCOPYLOOP   ; at this point vfoA is in the RCV buffer



DUMPPASTE:	
	lda vfoBstarthigh
	sta memjumphigh
	lda vfoBstartlow
	sta memjumplow
	ldy $FC	
	jsr UPDATEJUMPTABLEVFO ; update memjumplow and memjumphigh for the band given the VF0	
	ldx #$00


DUMPPASTELOOP:
	stx $FB
	ldy memjumphigh
	jsr TESTACIA
	ldy memjumplow
	jsr TESTACIA
	ldy RECEIVE_BUFFER,x
	jsr TESTACIA
	ldy RECEIVE_BUFFER+1,x
	jsr TESTACIA 
	ldy #$BC
	jsr TESTACIA
	jsr WAIT
	jsr WAIT
	jsr WAIT
	clc
	lda memjumplow
	adc #$02          ; add 2 to memjump low
	sta memjumplow	
	lda memjumphigh
	adc #$00          ; carry to memjumphigh as needed
	sta memjumphigh
	ldx $FB
	inx
	inx
	cpx $1A
	bne DUMPPASTELOOP
	jsr CLEARSTATUSLINE

MATCHBBAND:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$59
	sty tempjumplow
	jsr EZREAD       ; value of 59 in A
	and #%00001111
	sta $2B
	asl
	asl
	asl
	asl
	sta $2C
	lda $2B
	ora $2C	
	sta $2B	    ; this is our new value to write to 00x59 with radiobyte+1
	ldy #$00
	jsr TESTACIA
	ldy #$59
	jsr TESTACIA	
	ldy $2B
	jsr TESTACIA
	ldy radiobyte+1
	jsr TESTACIA
	ldy #$BC
	jsr TESTACIA
	jsr WAIT
	jmp UPDATE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         HELP                         ;;
;; Jump to helpfile program, shutdown nicely            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HELP:
	jsr TERMEXIT
	jsr WAIT
	jsr WAIT
	lda #helpfname_end-helpfname
	ldx #<helpfname
	ldy #>helpfname
	jmp $0C00           ;;;;;;;;;; LAUNCH LOADER FOR MAIN



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         CALCHK                       ;;
;; Jump to calcheck program, shutdown nicely            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CALCHK:
	jsr TERMEXIT
	jsr WAIT
	jsr WAIT
	lda #calchkname_end-calchkname
	ldx #<calchkname
	ldy #>calchkname
	jmp $0C00           ;;;;;;;;;; LAUNCH LOADER FOR MAIN


	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       EZWRITE                        ;;
;; Writes to EEPROM 2 bytes radiobyte and radiobyte+1   ;;
;; uses address in tempjumphigh and tempjumplow         ;;
;; TJH,TJL,RB,RB+1,$BC                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EZWRITE:
	ldy tempjumphigh
	jsr TESTACIA
	ldy tempjumplow
	jsr TESTACIA
	ldy radiobyte
	jsr TESTACIA
	ldy radiobyte+1
	jsr TESTACIA
	ldy #$BC
	jsr TESTACIA
	jsr WAIT
	jsr WAIT
	jsr WAIT
	rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      TOGGLEPWR                       ;;
;; Toggles POWER levels from 00x79 bits 1-0             ;;       
;; mask off bits 0 and 1 to get power value, if 3 jump  ;;
;; to powerzero to turn next value back to zero, if not ;;
;; 3 increment by 1.  load original 0079 value from     ;;
;; radiobyte and shut off 1st 2 bits.  ORA our bit in   ;;
;; and resave to radiobyte.  push to EZWRITE            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TOGGLEPWR:
	jsr CLEARBUFFER
	sty tempjumphigh
	ldy #$79
	sty tempjumplow
	jsr EZREAD       ; value of 79 in A
	and #%00000011   ; mask off all but 0 and 1
	sta $FD          ; store POWER values in FD
	cmp #%00000011   ; is it 3, jumpto agczero
	beq powerzero 
	inc $FD          ; not 3, increment value in FD
                     ; need to mask $FD value onto radiobyte
                     
continuepower:                     
	lda radiobyte
	and #%11111100
	ora $FD
	sta radiobyte      ; Store back to radiobye 
	jsr EZWRITE
	rts			
	
powerzero:	
	lda #$00	
	sta $FD	
	jmp continuepower	



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

GET_CAT_FREQANDMODE:
.byte $00,$00,$00,$00,$03        ; returns the two byte radio version

GET_TXMETER:
.byte $00,$00,$00,$00,$BD

GET_RX:
.byte $00,$00,$00,$00,$E7

GET_TX:
.byte $00,$00,$00,$00,$F7

SET_TOGGLE_VFO:
.byte $00,$00,$00,$00,$81

GET_VFO_STATUS:      
.byte $00,$55,$00,$00,$BB        

GET_VFO_BAND:
.byte $00,$59,$00,$00,$BB

VFOBANDS:
.text $00,"160m",$01," 75m",$02," 40m",$03," 30m",$04," 20m",$05," 17m",$06," 15m"
.text $07," 12m",$08," 10m",$09,"  6m",$0A,"fmbc",$0B,"air ",$0C,"  2m",$0D," uhf",$0E,"phan" 

MODES:
.text $00,"LSB",$01,"USB",$02,"CW ",$03,"CWR",$04,"AM ",$06,"WFM",$08,"FM ",$0A,"DIG",$0C,"PKT",$FC,"PKT"; was 0C

OPMODES:
.byte $00,$01,$02,$03,$04,$08,$0A,$0C ; was 0c

PWR:
.text $01,"l1",$03,"l2",$05,"l3",$08,"hi"

CFGPOWER:
.text $00,"hi",$01,"l3",$02,"l2",$03,"l1"

MEMVFO:
.text $00,"MEM ",$80,"VFO "

FREQUENCYLABEL:
.text "Frequency? [8 digits]: "

AEQUALSBLABEL1:
.text "Copies A to B. Are you sure? [Y/N] "

AEQUALSBLABEL2:
.text "Copying A to B DON'T TOUCH THE RADIO!!"  

SQUELCHLABEL:
.text $00,"       ",$80,"SQUELCH"

TXRXLABEL:
.text $80,"RX",$00,"TX"

CWPADDLELABEL:
.text $00,"norm",$04,"rev "

AGCLABEL:
.text $00,"auto",$01,"fast",$02,"slow",$03,"off "

RFSQLLABEL:
.text $80,"sql",$00,"rfg"

ONOFFLABEL:
.text $00,"off",$01,"on "

SCANMODELABEL:
.text $00,"off",$20,"off",$40,"up ",$60,"dwn"

RPTOFFSETLABEL:
.text $00," ",$40,"-",$80,"+",$C0,"N"

TONEDCSLABEL:
.text $00,"    ",$01,"TONE",$02,"TSQ ",$03,"DCS "

TOGGLE_VFO_DATA:
.byte $00,$00,$00,$00,$81        ;this is the tail for togglevfo

HOME:
.text "home "

helpfname:  .text "HELP"
helpfname_end:

calchkname: .text "TCALCHK"
calchkname_end:


TRANSMIT_BUFFER = *+0
RECEIVE_BUFFER  = *+12
* = * + (2*16) ; move program counter behind RECIEVE_BUFFER
