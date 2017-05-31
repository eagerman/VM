;
; vendingMachine.asm
;
; Created: 31/05/17 2:11:24 PM
; Author : williamweng
;

.include "m2560def.inc"

;========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== 
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro do_lcd_command	;send a command to the LCD
        ldi r16, @0
        rcall lcd_command
        rcall lcd_wait
.endmacro

.macro do_lcd_data		; send Data to display
        ldi r16, @0
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

;========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ==========
.equ	ITEM_STRUCT_SIZE = 18

.dseg
	item_struct:	.byte ITEM_STRUCT_SIZE

;========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ==========

.cseg
.org 0x0000
	jmp RESET

;========== RESET ========== RESET ==========  RESET ==========  RESET ==========  RESET ========== 
RESET:
	;stack pointer initialisation
	ldi YL, low(RAMEND)
	ldi YH, high(RAMEND)
	out SPH, YH
	out SPl, YL

	;LCD screen port initialisation
    ser r16
    out DDRF, r16
    out DDRA, r16
    clr r16
    out PORTF, r16
    out PORTA, r16
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
	
;========== RESET ========== RESET ==========  RESET ==========  RESET ==========  RESET ========== 

;========== START ========== START ========== START ========== START ========== START ==========
start:
	rcall fill_struct
	rcall initial_screen
	
;========== START ========== START ========== START ========== START ========== START ==========
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
	ldi r16, 9
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

;========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ==========

;========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== 
        ;;
        ;;  Send a command to the LCD (r16)
        ;;

lcd_command:
        out PORTF, r16
        rcall sleep_1ms
        lcd_set LCD_E	; turn on the enable pin
        rcall sleep_1ms
        lcd_clr LCD_E
        rcall sleep_1ms
        ret

lcd_data:
        out PORTF, r16
        lcd_set LCD_RS
        rcall sleep_1ms
        lcd_set LCD_E
        rcall sleep_1ms
        lcd_clr LCD_E
        rcall sleep_1ms
        lcd_clr LCD_RS
        ret

lcd_wait:
        push r16
        clr r16
        out DDRF, r16
        out PORTF, r16
        lcd_set LCD_RW
lcd_wait_loop:
        rcall sleep_1ms
        lcd_set LCD_E
        rcall sleep_1ms
        in r16, PINF
        lcd_clr LCD_E
        sbrc r16, 7
        rjmp lcd_wait_loop
        lcd_clr LCD_RW
        ser r16
        out DDRF, r16
        pop r16
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
;========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== 