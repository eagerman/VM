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

.def temp = r16
.def key = r25 ;holds the latest key pressed

;========== POTENTIOMETER VARIABLE ========== POTENTIOMETER VARIABLE ========== POTENTIOMETER VARIABLE ========== POTENTIOMETER VARIABLE ========== 
.def coinStage = r13
.def coinCount = r2
.def potValueL = r4
.def potValueH = r5
.def motor = r3
.def pushR = r6
.def pushL = r7

.equ COIN_START = 0
.equ COIN_LOW = 1
.equ COIN_HIGH = 2
.equ COIN_END = 3

;========== POTENTIOMETER VARIABLE ========== POTENTIOMETER VARIABLE ========== POTENTIOMETER VARIABLE ========== POTENTIOMETER VARIABLE ========== 
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

;========== LED MACROS ========== LED MACROS ========== LED MACROS ========== LED MACROS ========== LED MACROS ========== 
.macro ledLightUpBinary
	mov temp1, @0
	clr temp2
	out PORTG, temp2

	cpi temp1, 10
	brlo check8
	ldi temp2, 3
	out PORTG, temp2
	dec temp1
	rjmp binaryLoop

	check8:
	cpi temp1, 9
	brlo binaryLoop
	ldi temp2, 1
	out PORTG, temp2
	dec temp1

	binaryLoop:
		cpi temp1, 0
		breq binaryCont
		lsl temp2
		inc temp2
		dec temp1
		rjmp binaryLoop

	binaryCont:
	out PORTC, temp2
.endmacro
;========== LED MACROS ========== LED MACROS ========== LED MACROS ========== LED MACROS ========== LED MACROS ========== 

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
.org INT0addr
	jmp EXT_INT0
.org INT1addr
	jmp EXT_INT1
.org OVF0addr
   jmp Timer0OVF        ; Jump to the interrupt handler for
.org 0x003A			;ADC ADDR
	jmp EXT_POT
jmp DEFAULT          ; default service for all other interrupts.
DEFAULT:  reti          ; no service

;========== RESET ========== RESET ==========  RESET ==========  RESET ==========  RESET ========== 

RESET:
	clr motor
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
	
	clr coinStage
	clr potValueL
	clr potValueH
	clr coinCount

	;motor init
	ldi temp, 0b00010000; set PE2 to one
	out DDRE, temp

	;led init
	ldi temp, 0xff
	out DDRG, temp
	out DDRC, temp

	;Timers
	ldi temp, 0b00000000
    out TCCR0A, temp
    ldi temp, 0b00000010
    out TCCR0B, temp         ; Prescaling value = 8
    ldi temp, 1<<TOIE0      ; = 128 microseconds
    sts TIMSK0, temp        ; T/C0 interrupt enable
	
	
	;PB0 set up
	ldi temp, (2 << ISC00) ; set INT0 as fallingsts
	sts EICRA, temp ; edge triggered interrupt
	in temp, EIMSK ; enable INT0
	ori temp, (1<<INT0)
	out EIMSK, temp
	;PB1 set up
	ldi temp, (2 << ISC01) ; set INT0 as fallingsts
	sts EICRA, temp ; edge triggered interrupt
	in temp, EIMSK ; enable INT0
	ori temp, (1<<INT1)
	out EIMSK, temp

	; Speaker 
	ser temp
	out DDRB, temp
;========== POTENTIOMETER INITIALISATION =========== POTENTIOMETER INITIALISATION =========

; To repeat the routine, set 1<<ADSC
	ldi temp, (3<<REFS0) | (0<<ADLAR) | (0<<MUX0)
	sts ADMUX, temp
	ldi temp, (1<<MUX5)
	sts ADCSRB, temp
	ldi temp, (1<<ADEN | 1<<ADSC | 1<<ADIE | 5<<ADPS0)	; Prescaling
	;ldi temp, (1<<ADEN) | (1<<ADSC) | (1<<ADIE) | (1<<ADPS2) | (1<<ADPS1) | (1<<ADPS0)
	sts ADCSRA, temp

	ldi temp, 1
	rjmp start

;========== INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ========== 
Timer0OVF: ; interrupt subroutine to Timer0
	push r24
	push r25
	push temp
	push temp2

counting:
	lds r24, Timer1Counter
    lds r25, Timer1Counter+1
	adiw r25:r24, 1 ; Increase the temporary counter by one.
	mov temp2, motor
	cpi temp2, 1
	breq motorCode

motorCodeBack:
	sts Timer1Counter, r24
    sts Timer1Counter+1, r25

endTimer:
	pop temp2
	pop temp
	pop r25
	pop r24
	reti

/*beep_code:
	rcall beep
	rjmp beep_back*/

motorCode:
	mov temp1, coinCount
	cpi temp1, 0
	breq motorEnd
	cpi r24, low(1953)      ; 1953 is what we need Check if (r25:r24) = 7812 ; 7812 = 10^6/128
    ldi temp, high(1953)    ; 7812 interrupts = 1 second, 3906 interrupts = 0.5 seconds
    cpc r25, temp
	brge turnOff
	ser temp
	out PORTE, temp 
	rjmp motorCodeBack

turnOff:
	clr temp2
	ledLightUpBinary temp2
	clr temp ; turn motor off
	out PORTE, temp
	cpi r24, low(3906)      ; 1953 is what we need Check if (r25:r24) = 7812 ; 7812 = 10^6/128
    ldi temp, high(3906)    ; 7812 interrupts = 1 second, 3906 interrupts = 0.5 seconds
    cpc r25, temp
	brlo motorCodeBack
	clear Timer1Counter
	clr r24
	clr r25
	dec coinCount
	rjmp motorCodeBack

motorEnd:
	clr motor
	rjmp motorCodeBack

;========== INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ==========  INTERUPTS ========== 
EXT_INT0:
	push temp ; save register
	in temp, SREG ; save SREG
	push temp
	push temp1
	; debounce

	; WRITE YOU CODE HERE ==================
	mov temp1, pushL
	cpi temp1, 0
	brne END_INT0
	inc pushL
	; WRITE YOU CODE HERE ==================


	END_INT0:
	pop temp1
	pop temp ; restore SREG
	out SREG, temp
	pop temp ; restore register
	reti

EXT_INT1:
	push temp ; save register
	in temp, SREG ; save SREG
	push temp
	push temp1
	; debounce

	; WRITE YOU CODE HERE ==================
	mov temp1, pushR
	cpi temp1, 0
	brne END_INT1
	inc pushR
	; WRITE YOU CODE HERE ==================

	END_INT1:
	pop temp1
	pop temp ; restore SREG
	out SREG, temp
	pop temp ; restore register
	reti


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

	rcall sleep_25ms

	clr temp

selectLoop:
	rcall checkKey
	cpi key, NO_PRESS
	breq selectLoop
	cpi key, '*'
	breq adminCheck
	rjmp checkStock

adminCheck:
	clr temp
	clear Timer1Counter

adminCheckLoop:
	rcall checkKey
	cpi key, '*'
	brne selectLoop
	rcall check5
	cpi temp, 1
	brne adminCheckLoop
	
jmpAdminMode:
	jmp adminMode
;========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== 

;========== CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ==========  CHECKSTOCK FUNCTION ========== 
checkStock:
	mov temp, key
	cpi temp, '#'
	breq endStock
	cpi temp, '1'
	brlo endStock
	cpi temp, ':'
	brge endStock
	subi temp, '1'

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
	adiw Y, 1 ;from quantity to price
	clr coinStage
	clr coinCount
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
	rcall flashLEDS
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
	ld temp1, Y
	do_lcd_rdata temp1
	clr coinStage

coinLoop:
	rcall checkKey
	cpi key, '#'
	breq cancelCoin
	rcall checkPot
	mov temp, coinStage
	cpi temp, COIN_END
	breq payCheck
coinBack:
	rcall sleep_5ms
	rjmp coinLoop
	;TODO push buttons and led light

jmpdeliveryScreen:
	jmp deliveryScreen
;========== COIN MODE ========== COIN MODE ========== COIN MODE ========== COIN MODE ========== COIN MODE ========== 

;========== CANCEL COIN MODE ========== CANCEL COIN MODE ========== CANCEL COIN MODE ========== CANCEL COIN MODE ========== 
cancelCoin:
	clr coinStage
	inc motor
	clear Timer1Counter
	ledLightUpBinary coinStage
	jmp selectScreen
;========== CANCEL COIN MODE ========== CANCEL COIN MODE ========== CANCEL COIN MODE ========== CANCEL COIN MODE ========== 

;========== PAY MODE ========== PAY MODE ========== PAY MODE ========== PAY MODE ========== PAY MODE ========== 

payCheck:
	clr coinStage
	ld temp1, Y
	inc coinCount
	mov temp, coinCount
	cp temp, temp1
	breq jmpdeliveryScreen
	sub temp1, temp
	do_lcd_command 0b11000000
	do_lcd_rdata temp1
	ledLightUpBinary temp
	rjmp coinBack

;========== PAY MODE ========== PAY MODE ========== PAY MODE ========== PAY MODE ========== PAY MODE ========== 

;========== CHECK POT MODE ========== CHECK POT MODE ========== CHECK POT MODE ========== CHECK POT MODE ========== 
checkPot:
	push temp1
	push temp2
	push temp
	lds potValueL, ADCL
	lds potValueH, ADCH

checkLow:
	mov temp, coinStage	
	cpi temp, COIN_START
	breq lowCon
	mov temp, coinStage
	cpi temp, COIN_HIGH
	brne checkHigh

lowCon:
	ldi temp1, low(0x0000)
	ldi temp2, high(0x0000)
	cp potValueL, temp1
	cpc potValueH, temp2
	breq incCoinStage
	jmp checkPotExit

checkHigh:
	mov temp, coinStage
	cpi temp, COIN_LOW
	brne checkPotExit
	ldi temp1, low(0x03FF)
	ldi temp2, high(0x03FF)
	cp potValueL, temp1
	cpc potValueH, temp2
	breq incCoinStage
	jmp checkPotExit

incCoinStage:
	inc coinStage
	
checkPotExit:
	pop temp
	pop temp2
	pop temp1
	ret

;========== CHECK POT MODE ========== CHECK POT MODE ========== CHECK POT MODE ========== CHECK POT MODE ========== 

jmpselectScreen:
	jmp selectScreen
;========== Delivery MODE ===============================================================
deliveryScreen:
	clr coinStage
	do_lcd_command 0b00000001 ; Clear display
	do_lcd_data 'D'
	do_lcd_data 'e'
	do_lcd_data 'l'
	do_lcd_data 'i'
	do_lcd_data 'v'
	do_lcd_data 'e'
	do_lcd_data 'r'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data 'g'
	do_lcd_data ' '
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data 'e'
	do_lcd_data 'm'
	ser temp
	out PORTE, temp
	rcall flashLEDS
	clr temp
	out PORTE, temp

	ld temp, -Y
	cpi temp, 0
	breq jmpselectScreen
	dec temp
	st Y, temp
	jmp selectScreen
;========== Delivery MODE ===============================================================

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
	do_lcd_data '1'
	do_lcd_command 0b11000000
	ldi YH, high(item_struct)
	ldi YL, low(item_struct)
	ld temp, Y+
	do_lcd_rdata temp
	
	ledLightUpBinary temp
	do_lcd_command 0b11001110
	do_lcd_data '$'
	ld temp, Y
	do_lcd_rdata temp

adminLoop:
	rcall checkKey
	cpi key, 'A'
	breq jmpPriceUp
	cpi key, 'B'
	breq jmpPriceDown
    cpi key, 'C'
    breq clearItem
	cpi key, '#'
	breq jmpBackScreen
	cpi key, '*'
	breq adminLoop
	cpi key, NO_PRESS
	brne jmpItemChoose
    mov temp2, pushR
	cpi temp2, 1
    breq jmpItemDown
    mov temp2, pushL
	cpi temp2, 1
    breq jmpItemUp
	rjmp adminLoop

jmpItemChoose:
	jmp itemChoose
jmpPriceUp:
	jmp priceUp
jmpPriceDown:
	jmp priceDown
jmpItemUp:
	clr pushL
	jmp itemUp
jmpItemDown:
	clr pushR
	jmp itemDown

jmpBackScreen:
	clr temp
	ledLightUpBinary temp
	jmp selectScreen

clearItem:
    ld temp, -Y
    clr temp
    ledLightUpBinary temp
    st y, temp
    adiw y, 1
    ld temp, y
    do_lcd_command 0b11000000
    do_lcd_data '0'
    rjmp adminLoop
	
itemUp:
    ld temp, -Y
    inc temp
	cpi temp, 10
	brge itemUBack
    ledLightUpBinary temp
    do_lcd_command 0b11000000
    do_lcd_rdata temp
    st y, temp
    adiw y, 1
    ld temp, y
	rcall sleep_100ms
    rjmp adminLoop

itemUBack:
    ledLightUpBinary temp
    do_lcd_command 0b11000000
    do_lcd_data '1'
    do_lcd_command 0b11000001
    do_lcd_data '0'
    adiw y, 1
    ld temp, y
	jmp adminLoop

itemDown:
    do_lcd_command 0b11000001
    do_lcd_data ' '
    ld temp, -Y
	cpi temp, 1
	brlo itemDBack
	rcall sleep_100ms
    dec temp
    ledLightUpBinary temp
    do_lcd_command 0b11000000
    do_lcd_rdata temp
    st y, temp
    adiw y, 1
    ld temp, y
    rjmp adminLoop
	
itemDBack:
    adiw y, 1
    ld temp, y
	jmp adminLoop

jmpAdminLoop:
	jmp adminLoop
	
priceUp:
	cpi temp, 3
	breq jmpAdminLoop
	inc temp
	st Y, temp
	do_lcd_command 0b11001111
	do_lcd_rdata temp
	rcall sleep_100ms
	rjmp adminLoop
	
priceDown:
	cpi temp, 1
	breq jmpAdminLoop
	dec temp
	st Y, temp
	do_lcd_command 0b11001111
	do_lcd_rdata temp
	rcall sleep_100ms
	rjmp adminLoop

itemChoose:
	mov temp, key
	cpi temp, '#'
	breq endItem
	cpi temp, '1'
	brlo endItem
	cpi temp, ':'
	brge endItem
	subi temp, '1'

	ldi YH, high(item_struct)
	ldi YL, low(item_struct)

itemLoop:
	cpi temp, 0
	breq itemCon
	adiw Y, 2
	dec temp
	rjmp itemLoop

itemCon:
	do_lcd_command 0b10001011
	do_lcd_data_reg key
	ld temp, Y+
	ledLightUpBinary temp
	do_lcd_command 0b11000000
	do_lcd_rdata temp
	ld temp, Y
	do_lcd_command 0b11001111
	do_lcd_rdata temp
	rjmp adminLoop

endItem:
	jmp adminLoop
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
	ldi r16, 2
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
	ldi r16, 9; change back to 9 ;==============;==============;==============;==============;==============;==============
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

checking3:
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
	
check5:
	push r24
	push r25

checking5:
	lds r24, Timer1Counter
    lds r25, Timer1Counter+1

	cpi r24, low(31248)      ; 1953 is what we need Check if (r25:r24) = 7812 ; 7812 = 10^6/128
    ldi temp, high(31248)    ; 7812 interrupts = 1 second, 3906 interrupts = 0.5 seconds
    cpc r25, temp
	brge is5
	ldi temp, 0
	pop r25
	pop r24
	ret

is5:
	clear Timer1Counter
	ldi temp, 1
	pop r25
	pop r24
	ret

;========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ==========
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

debounce:
		/* start a counter at 10, if key is pressed, increment, if not pressed, decrement,
		if counter is 20, key is pressed, else key is not pressed */

		ldi temp, 20
		testDebounce:
			rcall sleep_1ms
			lds temp1, PINL
			cpi temp1, NO_PRESS				; compare key if it's pressed
			breq testDebounceNotPressed		; 
			inc temp
			cpi temp, 40
			brne testDebounce
			rjmp pressed
		testDebounceNotPressed:
			dec temp
			cpi temp, 0
			brne nopress
			rjmp testDebounce

pressed:

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
		cpi col, 3
		breq col3
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

col3:
		cpi row, 0
		breq letA
		cpi row, 1
		breq letB
		cpi row, 2
		breq letC
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
		cpi temp1, '*'
		breq backtoConvert
		cpi temp1, NO_PRESS
		brne jmptobeep
		backtoConvert:
        mov key, temp1
		pop r23;
		pop r22;
		pop r21;
		pop r20;
		pop r19;
		pop r17;
		pop r16;
        ret

jmptobeep:
	rcall beep
	rjmp backtoConvert
	
		; MAKE SPEAKER BEEP FOR 250MS
beep:
	push temp
	in temp, SREG
	push temp
	push temp1

	clr temp
	
	soundLoop:
		;125*2ms sleeps = 250ms beep
		cpi temp, 125		
		breq endSoundLoop

		sbi PORTB, 0	;set bit 0
		rcall sleep_1ms

		cbi PORTB, 0	; clear bit 0
		rcall sleep_1ms	;square wave

		inc temp

	rjmp soundLoop

	endSoundLoop:
		pop temp1
		pop temp
		out SREG, temp
		pop temp
		ret

; ============== LED FUNCTIONS================================================
turnOnLEDS:
	ser temp
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