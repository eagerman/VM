;
; vendingMachine.asm
;
; Created: 31/05/17 2:11:24 PM
; Author : williamweng
;

.include "m2560def.inc"

;========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ==========
.equ	ITEM_STRUCT_SIZE = 18
.equ	NO_PRESS = 1

.dseg
	item_struct:	.byte ITEM_STRUCT_SIZE
PotCounter:				;100ms
	.byte 2

.def temp = r16
.def key = r25 ;holds the latest key pressed
.def enablePot = r13
.def coinCount = r2
.def potValueL = r4
.def potValueH = r5
.def row = r17              ; current row number
.def col = r19              ; current column number
.def rmask = r20            ; mask for current row during scan
.def cmask = r21            ; mask for current column during scan
.def temp1 = r22 
.def temp2 = r23

.equ PORTLDIR = 0xF0        ; PH7-4: output, PH3-0, input
.equ INITCOLMASK = 0xEF     ; scan from the rightmost column,
.equ INITROWMASK = 0x01     ; scan from the top row
.equ ROWMASK = 0x0F         ; for obtaining input from Port L

; The macro clears a word (2 bytes) in a memory
; the parameter @0 is the memory address for that word
.macro clear
    ldi YL, low(@0)     ; load the memory address to Y
    ldi YH, high(@0)
    clr temp 
    st Y+, temp         ; clear the two bytes at @0 in SRAM
    st Y, temp
.endmacro

;========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ==========

;========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== 
.def lcd = r18

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro do_lcd_command	;send a command to the LCD
        ldi lcd, @0
        rcall lcd_command
        rcall lcd_wait
.endmacro

.macro do_lcd_data		; send Data to display
        ldi lcd, @0
        rcall lcd_data
        rcall lcd_wait
.endmacro

.macro do_lcd_data_reg		; send Data to display
        mov lcd, @0
        rcall lcd_data
        rcall lcd_wait
.endmacro

.macro do_lcd_rdata
	mov lcd, @0
	subi lcd, -'0' ;adding 48 to convert to digits 0-9
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro lcd_set
        sbi PORTA, @0
.endmacro

.macro lcd_clr
        cbi PORTA, @0
.endmacro

;========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== 
;========== SETUP MACROS ========== SETUP MACROS ========== SETUP MACROS ========== SETUP MACROS ========== SETUP MACROS ==========
.macro isOdd
	.if @1 & 1
		st @0, 1
	.else
		st @0, 2
	.endif
.endmacro

.macro fillElement
	
.endmacro
;========== SETUP MACROS ========== SETUP MACROS ========== SETUP MACROS ========== SETUP MACROS ========== SETUP MACROS ========== 

;========== TIMER VARIABLE ==========  TIMER VARIABLE ==========  TIMER VARIABLE ==========  TIMER VARIABLE ==========  TIMER VARIABLE ==========
Timer1Counter:
   .byte 2              ; Temporary counter. Used to determine 
                        ; if one second has passed

;========== TIMER VARIABLE ==========  TIMER VARIABLE ==========  TIMER VARIABLE ==========  TIMER VARIABLE ==========  TIMER VARIABLE ========== 

;========== INTERUPT INITIALISATION ==========  INTERUPT INITIALISATION ==========  INTERUPT INITIALISATION ==========  INTERUPT INITIALISATION ========== 

.cseg
.org 0x0000
   jmp RESET
   jmp DEFAULT          ; No handling for IRQ0.
   jmp DEFAULT          ; No handling for IRQ1.
.org OVF0addr
   jmp Timer0OVF        ; Jump to the interrupt handler for
.org 0x003A			;ADC ADDR
	jmp EXT_POT
.org OVF3addr
	jmp TIMER3OVF
jmp DEFAULT          ; default service for all other interrupts.
DEFAULT:  reti          ; no service

;========== RESET ========== RESET ==========  RESET ==========  RESET ==========  RESET ========== 
RESET:
	;stack pointer initialisation
	ldi temp, low(RAMEND)
	out SPL, temp
	ldi temp, high(RAMEND)
	out SPH, temp
	sei
	
	; LCD setup
	ser temp
	out DDRF, temp ; data input address
	out DDRA, temp ; inst inpit address
	clr temp
	out PORTF, temp; data port set
	out PORTA, temp; inst port set

	; LCD: init the settings
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001100 ; Cursor on, bar, no blink

	; Keypad setup
	ldi temp1, PORTLDIR
    sts DDRL, temp1	; enable input on lower 4 bits of port L
	
	clr enablePot
	clr potValueL
	clr potValueH
	clr coinCount

	;Timers
	ldi temp, 0b00000000
    out TCCR0A, temp
	sts TCCR3A, temp
    ldi temp, 0b00000010
    out TCCR0B, temp 
	sts TCCR3B, temp         ; Prescaling value = 8
    ldi temp, 1<<TOIE0      ; = 128 microseconds
	ldi temp, (1<<TOIE4)
    sts TIMSK0, temp        ; T/C0 interrupt enable
	sts TIMSK3, temp

	
;========== POTENTIOMETER INITIALISATION =========== POTENTIOMETER INITIALISATION =========
/*	ldi temp, (3<<REFS0 | 0<<ADLAR | 0<<MUX0)	;
	sts ADMUX, temp
	ldi temp, (1<<MUX5)	;
	sts ADCSRB, temp
	ldi temp, (1<<ADEN | 1<<ADSC | 1<<ADIE | 5<<ADPS0)	; Prescaling
	sts ADCSRA, temp*/
	
; To repeat the routine, set 1<<ADSC
	ldi temp, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
	sts ADMUX, temp
	ldi temp, (1<<MUX5)
	sts ADCSRB, temp
	ldi temp, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0)
	sts ADCSRA, temp

	ldi temp, 1
	rjmp start
;========== RESET ========== RESET ==========  RESET ==========  RESET ==========  RESET ========== 

;========== INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ========== 
Timer0OVF: ; interrupt subroutine to Timer0
	push r24
	push r25

counting:
	lds r24, Timer1Counter
    lds r25, Timer1Counter+1
	adiw r25:r24, 1 ; Increase the temporary counter by one.
	sts Timer1Counter, r24
    sts Timer1Counter+1, r25

endTimer:
	pop r25
	pop r24
	reti

;========== INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ========== 

;========== START ========== START ========== START ========== START ========== START ==========
start:
	ldi key, NO_PRESS ;'NUL'
	rcall fill_struct
	rcall initial_screen
    clear Timer1Counter       ; Initialize the temporary counter to 0
	clr temp

startLoop:
	rcall check3
	cpi temp, 1
	breq selectScreen	
	rcall checkKey
	cpi key, NO_PRESS
	brne selectScreen
	rjmp startLoop
	
;========== START ========== START ========== START ========== START ========== START ==========

;========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== 
selectScreen:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'S'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'e'
	do_lcd_data 'c'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	clr temp

selectLoop:
	rcall checkKey
	cpi key, NO_PRESS
	breq selectLoop
	cpi key, '*'
	breq jmpAdminMode
	rjmp checkStock

jmpAdminMode:
	jmp adminMode
;========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== 

;========== CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ========== 
checkStock:
	mov temp, key
	subi temp, '1'
	cpi temp, 9
	brge endStock

	ldi YH, high(item_struct)
	ldi YL, low(item_struct)

chooseLoop:
	cpi temp, 0
	breq chooseCon
	adiw Y, 2
	dec temp
	rjmp chooseLoop

chooseCon:
	ld temp, Y
	cpi temp, 0
	breq emptyScreen
	rjmp coinScreen

endStock:
	rjmp selectLoop
	;pop YL
	;pop YH
	;pop temp
	;ret
	
;========== CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ========== 

;========== EMPTY MODE ========== EMPTY MODE ========== EMPTY MODE ========== EMPTY MODE ========== EMPTY MODE ========== 
emptyScreen:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'O'
	do_lcd_data 'u'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'o'
	do_lcd_data 'f'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'c'
	do_lcd_data 'k'
	do_lcd_command 0b11000000
	do_lcd_data_reg key

	clear Timer1Counter       ; Initialize the temporary counter to 0
	clr temp

rcall flashLEDS

emptyLoop:
	rcall check3
	cpi temp, 1
	breq jmpSelectScreen
	rjmp emptyLoop

jmpSelectScreen:
	jmp selectScreen
	;TODO push buttons and led light

;========== EMPTY MODE ========== EMPTY MODE ========== EMPTY MODE ========== EMPTY MODE ========== EMPTY MODE ========== 

;========== COIN MODE ========== COIN MODE ========== COIN MODE ========== COIN MODE ========== COIN MODE ========== 
coinScreen:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data 'I'
	do_lcd_data 'n'
	do_lcd_data 's'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'c'
	do_lcd_data 'o'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 's'
	do_lcd_command 0b11000000
	
	adiw Y, 1 ;from quantity to price
	ld temp, Y
	/*subi temp, -'0'	;done in do_lcd_rdata macro
	do_lcd_data_reg temp*/ 
	do_lcd_rdata temp
	
	inc enablePot	;enables potentiometer
	
	clear Timer1Counter       ; Initialize the temporary counter to 0
	clr temp

coinLoop:
	rcall checkKey
	cpi key, '#'
	breq jmpselectScreen

	cpi temp, 1				;@WILLIAM WHAT IS THIS FOR?
	breq jmpselectScreen
	rjmp coinLoop
	
	;TODO push buttons and led light

;========== COIN MODE ========== COIN MODE ========== COIN MODE ========== COIN MODE ========== COIN MODE ========== 

;========== ADMIN MODE ========== ADMIN MODE ========== ADMIN MODE ========== ADMIN MODE ========== ADMIN MODE ==========
adminMode:
		do_lcd_command 0b10000000
		do_lcd_data 'A'
		do_lcd_data 'd'
		do_lcd_data 'm'
		do_lcd_data 'i'
		do_lcd_data 'n'
		do_lcd_data ' '
		do_lcd_data 'M'
		do_lcd_data 'o'
		do_lcd_data 'd'
		do_lcd_data 'e'
		do_lcd_data ' '

		do_lcd_command 0b11000000

;========== ADMIN MODE ========== ADMIN MODE ========== ADMIN MODE ========== ADMIN MODE ========== ADMIN MODE ==========

end:
	rjmp end

;========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ==========
;fill struct function
fill_struct: 
	push YH
	push YL
	push r16
	;push r17

	ldi YH, high(item_struct)
	ldi YL, low(item_struct)
	;ldi r16, 1
	;clr r17

/*
fill:
	cpi r16, 10
	breq end_fill
	st Y+, r16
	isOdd r17, r16
	st Y+, r17
	inc r16
	rjmp fill
*/

fill:
	ldi r16, 1
	st Y+, r16
	ldi r16, 1
	st Y+, r16
	ldi r16, 1
	st Y+, r16
	ldi r16, 2
	st Y+, r16
	ldi r16, 3
	st Y+, r16
	ldi r16, 1
	st Y+, r16
	ldi r16, 4
	st Y+, r16
	ldi r16, 2
	st Y+, r16
	ldi r16, 5
	st Y+, r16
	ldi r16, 1
	st Y+, r16
	ldi r16, 6
	st Y+, r16
	ldi r16, 2
	st Y+, r16
	ldi r16, 7
	st Y+, r16
	ldi r16, 1
	st Y+, r16
	ldi r16, 8
	st Y+, r16
	ldi r16, 2
	st Y+, r16
	ldi r16, 0; change back to 9 ;==============;==============;==============;==============;==============;==============
	st Y+, r16
	ldi r16, 1
	st Y, r16

end_fill:
;	pop r17
	pop r16
	pop YL
	pop YH
	ret
	
;Inital screen function
initial_screen:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data '7'
	do_lcd_data 's'
	do_lcd_data '1'
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data ' '
	do_lcd_data 'L'
	do_lcd_data '1'
	do_lcd_data '2'
	do_lcd_command 0b11000000
	do_lcd_data 'V'
	do_lcd_data 'e'
	do_lcd_data 'n'
	do_lcd_data 'd'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'M'
	do_lcd_data 'a'
	do_lcd_data 'c'
	do_lcd_data 'h'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'e'
ret

check3:
	push r24
	push r25

checking:
	lds r24, Timer1Counter
    lds r25, Timer1Counter+1

	cpi r24, low(23436)      ; 1953 is what we need Check if (r25:r24) = 7812 ; 7812 = 10^6/128
    ldi temp, high(23436)    ; 7812 interrupts = 1 second, 3906 interrupts = 0.5 seconds
    cpc r25, temp
	brge is3
	ldi temp, 0
	pop r25
	pop r24
	ret

is3:
	clear Timer1Counter
	ldi temp, 1
	pop r25
	pop r24
	ret

;========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ==========

;===========Potentiometer reading timer ==================================================
; TIMER 3 - POTENTIOMETER UPDATE EVERY 100MS
; SIMILAR TO TIMER1/2
; ENABLEPOT = 0 -> DISABLE TIMER3
; ENABLEPOT = 1 -> STAGE 1: GO TO 0x0000 (POTSTAGEMIN)
; ENABLEPOT = 2 -> STAGE 2: GO TO 0x03FF (POTSTAGEMAX)
; ENABLEPOT = 3 -> STAGE 3: GO TO 0x0000 (POTSTAGEMIN)
; ENABLEPOT = 4 -> STAGE 4: COIN INSERTION COMPLETE, GO BACK TO STAGE 1 (POTSTAGEFINAL)
TIMER3OVF:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	push YH
	push YL
	push r25
	push r24
checkPot:
	clr temp1
	cp enablePot, temp1
	breq TIMER3Epilogue
startTIMER3:
	; 2
	lds r24, PotCounter
	lds r25, PotCounter+1
	adiw r25:r24, 1
	; 3
	cpi r24, low(3)
	ldi temp1, high(3)
	cpc r25, temp1
	brne notTenthSecond
; WHEN 100MS PASS
; 1. RETRIVE POTENTIOMETER VALUE
; 2. CHECK STAGE, AND BRANCH ACCORDINGLY
tenthSeconds:
	clear PotCounter
	lds potValueL, ADCL
	lds potValueH, ADCH	
	ldi temp1, 1
	cp enablePot, temp1
	breq potStageMin
	inc temp1
	cp enablePot, temp1
	breq potStageMax
	inc temp1
	cp enablePot, temp1
	breq potStageMin
	inc temp1
	cp enablePot, temp1
	breq potStageFinal
; WHEN POTENTIOMETER = 0X0000 (FULLY ANTICLOCKWISE), INCREMENT
potStageMin:
	ldi temp1, low(0x0000)
	ldi temp2, high(0x0000)
	cp potValueL, temp1
	cpc potValueH, temp2
	breq incPot
	jmp TIMER3Epilogue
; WHEN POTENTIOMETER = 0X03FF (FULLY CLOCKWISE), INCREMENT
potStageMax:
	ldi temp1, low(0x03FF)
	ldi temp2, high(0x03FF)
	cp potValueL, temp1
	cpc potValueH, temp2
	breq incPot
	jmp TIMER3Epilogue
; WHEN PROCESS COMPLETE, INCREMENT COIN COUNT, DECREASE INVENTORY COST
potStageFinal:
	inc coinCount
	rcall turnOnLEDS;

;	dec inventoryCost
	ldi temp1, 1
	mov enablePot, temp1
	jmp TIMER3Epilogue
; NEXT STAGE
incPot:
	inc enablePot
	jmp TIMER3Epilogue
notTenthSecond:
	sts PotCounter, r24
	sts PotCounter+1, r25
	rjmp TIMER3Epilogue
; 4
TIMER3Epilogue:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti
; POTENTIOMETER INTERRUPT ADC
; USED IN CONJUNCTION WITH TIMER3
; 1. PUSH
; 2. STORE (1<<ADSC) INTO ADCSRA
; 3. POP
EXT_POT:
	; 1
	push temp1
	push temp2
	in temp1, SREG
	push temp1
	; 2
	lds temp1, ADCSRA
	ori temp1, (1<<ADSC)
	sts ADCSRA, temp1
	; 3
	pop temp1
	out SREG, temp1
	pop temp2
	pop temp1
	reti
;========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== 
checkKey:
		push r16; debouncer
		push r17;
		push r19;
		push r20;
		push r21;
		push r22;
		push r23; 

keyStart:
        ldi cmask, INITCOLMASK
        clr col
		clr r16

colloop:
        cpi col, 4
        breq noPress
        sts PORTL, cmask

debounce:				; if same button is pressed for 5 loops with 1ms delays
		;inc temp
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
        lds temp1, PINL	; Read Port L
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		lds temp2, PINL	; Read from same port
		cp temp2, temp1
		brne nopress	; if not same then no press
		;cpi temp, 5
		;brne debounce
        ;@khaled debouncing works well for row 1
        andi temp1, ROWMASK
        cpi temp1, 0xF
        breq nextcol
		
		ldi rmask, INITROWMASK
        clr row

rowloop:
        cpi row, 4
        breq nextcol
        mov temp2, temp1
        and temp2, rmask
        breq convert
        inc row
        lsl rmask
        rjmp rowloop

nextcol:
        lsl cmask
        inc col
        rjmp colloop

convert:
        cpi row, 3
		breq row3
		cpi col, 4
		breq col4
        mov temp1, row
        lsl temp1
        add temp1, row
        add temp1, col
        subi temp1, - '1'
        rjmp convert_end

nopress:
		ldi temp1, NO_PRESS
		rjmp convert_end
row3:
		cpi col, 0
		breq star
		cpi col, 1
		breq zero
		cpi col, 2
		breq hash
		rjmp nopress

col4:
		cpi row, 0
		breq letA
		cpi row, 1
		breq letA
		cpi row, 2
		breq letA
		rjmp nopress
		
hash:
		ldi temp1, '#'
		rjmp convert_end

star:
		ldi temp1, '*'
		rjmp convert_end

zero:
        ldi temp1, '0'
		rjmp convert_end

letA:
		ldi temp1, 'A'
		rjmp convert_end

letB:
		ldi temp1, 'B'
		rjmp convert_end

letC:
        ldi temp1, 'C'
		rjmp convert_end

convert_end:
        mov key, temp1
		pop r23;
		pop r22;
		pop r21;
		pop r20;
		pop r19;
		pop r17;
		pop r16;
        ret
;========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== 

; ============== LED FUNCTIONS================================================
turnOnLEDS:
	ser temp
	out DDRC, temp
	out DDRG, temp
	out PORTC, temp
	out PORTG, temp
	ret

turnOffLEDS:
	clr temp
	out PORTC, temp
	out PORTG, temp
	ret

flashLEDS:
	rcall turnOnLEDS
	rcall sleep_500ms
	rcall turnOffLEDS
	rcall sleep_500ms

	rcall turnOnLEDS
	rcall sleep_500ms
	rcall turnOffLEDS
	rcall sleep_500ms

	rcall turnOnLEDS
	rcall sleep_500ms
	rcall turnOffLEDS
	rcall sleep_500ms
	ret

jmpTurnOffLEDS:
	rjmp turnOffLEDS

;========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== 

;;
;;  Send a command to the LCD (r18)
;;

lcd_command:
        out PORTF, lcd
        rcall sleep_1ms
        lcd_set LCD_E	; turn on the enable pin
        rcall sleep_1ms
        lcd_clr LCD_E
        rcall sleep_1ms
        ret

lcd_data:
        out PORTF, lcd
        lcd_set LCD_RS
        rcall sleep_1ms
        lcd_set LCD_E
        rcall sleep_1ms
        lcd_clr LCD_E
        rcall sleep_1ms
        lcd_clr LCD_RS
        ret

lcd_wait:
        push lcd
        clr lcd
        out DDRF, lcd
        out PORTF, lcd
        lcd_set LCD_RW

lcd_wait_loop:
        rcall sleep_1ms
        lcd_set LCD_E
        rcall sleep_1ms
        in lcd, PINF
        lcd_clr LCD_E
        sbrc lcd, 7
        rjmp lcd_wait_loop
        lcd_clr LCD_RW
        ser lcd
        out DDRF, lcd
        pop lcd
        ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4

sleep_1ms:
        push r24
        push r25
        ldi r25, high(DELAY_1MS)
        ldi r24, low(DELAY_1MS)
delayloop_1ms:
        sbiw r25:r24, 1
        brne delayloop_1ms
        pop r25
        pop r24
        ret

sleep_5ms:
        rcall sleep_1ms
        rcall sleep_1ms
        rcall sleep_1ms
        rcall sleep_1ms
        rcall sleep_1ms
        ret
sleep_25ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret
sleep_100ms:
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	ret
sleep_500ms:
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	rcall sleep_100ms
	ret
;========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== 