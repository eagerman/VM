;
; vendingMachine.asm
;
; Created: 31/05/17 2:11:24 PM
; Author : williamweng
;

.include "m2560def.inc"

;========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ========== GLOBAL VARIABLE ==========
.equ	ITEM_STRUCT_SIZE = 18

.dseg
	item_struct:	.byte ITEM_STRUCT_SIZE

.def temp = r16
.def key = r25 ;holds the latest key pressed

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

.macro lcd_set
        sbi PORTA, @0
.endmacro

.macro lcd_clr
        cbi PORTA, @0
.endmacro

;========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== LCD MACROS ========== 

;========== KEYPAD MACROS ========== KEYPAD MACROS ========== KEYPAD MACROS ========== KEYPAD MACROS ========== KEYPAD MACROS ==========

.def row = r17              ; current row number
.def col = r19              ; current column number
.def rmask = r20            ; mask for current row during scan
.def cmask = r21            ; mask for current column during scan
.def temp1 = r22 

.equ PORTLDIR = 0xF0        ; PH7-4: output, PH3-0, input
.equ INITCOLMASK = 0xEF     ; scan from the rightmost column,
.equ INITROWMASK = 0x01     ; scan from the top row
.equ ROWMASK = 0x0F         ; for obtaining input from Port L

;========== KEYPAD MACROS ========== KEYPAD MACROS ========== KEYPAD MACROS ========== KEYPAD MACROS ========== KEYPAD MACROS ==========

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
jmp DEFAULT          ; default service for all other interrupts.
DEFAULT:  reti          ; no service

;========== INTERUPT INITIALISATION ==========  INTERUPT INITIALISATION ==========  INTERUPT INITIALISATION ==========  INTERUPT INITIALISATION ========== 

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
	ldi temp, 0b00000000
    out TCCR0A, temp
    ldi temp, 0b00000010
    out TCCR0B, temp        ; Prescaling value = 8
    ldi temp, 1<<TOIE0      ; = 128 microseconds
    sts TIMSK0, temp        ; T/C0 interrupt enable

	ldi key, 0 ;'NUL'
	rcall fill_struct
	rcall initial_screen
    clear Timer1Counter       ; Initialize the temporary counter to 0
	clr temp

startLoop:
	rcall check3
	cpi temp, 1
	breq selectScreen
	rcall checkKey
	cpi key, 15
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

;	do_lcd_data_reg  key

	
;========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== SELECTIONSCREEN ========== 

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
	breq is3
	ldi temp, 0
	pop r25
	pop r24
	ret

is3:
	ldi temp, 1
	pop r25
	pop r24
	ret
;========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ========== HELPER FUNCTIONS ==========

;========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== 
checkKey:
		push r16;
		push r17;
		push r19;
		push r20;
		push r21;
		push r22;
keyStart:
        ldi cmask, INITCOLMASK

        clr col
colloop:
        cpi col, 4
        breq convert_end
        sts PORTL, cmask
        ldi temp1, 0xFF

delay:
        dec temp1
        brne delay
        lds temp1, PINL	; Read Port L
        andi temp1, ROWMASK
        cpi temp1, 0xF
        breq nextcol
        
        ldi rmask, INITROWMASK
        clr row

rowloop:
        cpi row, 4
        breq nextcol
        mov temp, temp1
        and temp, rmask
        breq convert
        inc row
        lsl rmask
        jmp rowloop
nextcol:        
        lsl cmask
        inc col
        jmp colloop
 
convert:
        cpi row, 3
		breq row3
        mov temp1, row
        lsl temp1
        add temp1, row
        add temp1, col
        subi temp1, -1
        jmp convert_end      
        
row3:
		cpi col, 1
		breq zero
zero:
        ldi temp1, '0'
        
convert_end:
        mov key, temp1
pop r22;
pop r21;
pop r20;
pop r19;
pop r17;
pop r16;
        ret

;========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== KEYPAD FUNCTIONS ========== 

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
;========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== LCD HELPER FUNCTION ========== 