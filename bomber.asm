	processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include "vcs.h"
	include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declare variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u variables
    org $80

JET_XPOS            byte    ; P0 X position
JET_YPOS            byte    ; P0 Y position

JET_XPOS_MIN        byte    ; P0 min X pos
JET_XPOS_MAX        byte    ; P0 max X pos
JET_YPOS_MIN        byte    ; P0 min Y pos
JET_YPOS_MAX        byte    ; P0 max Y pos

MISSILE_XPOS        byte    ; missile X position
MISSILE_YPOS        byte    ; missile Y position

BOMBER_XPOS         byte    ; P1 X position
BOMBER_YPOS         byte    ; P1 Y position

SCORE               byte    ; 2-digit score stored as BCD
TIMER               byte    ; 2-digit timer stored as BCD
TEMP                byte    ; auxiliary variable to store temp score values
ONES_DIGIT_OFFSET   word    ; lookup table offset for the score one's digit
TENS_DIGIT_OFFSET   word    ; lookup table offset for the score ten's digit

    ; memory address is 16 bits - hence we need a word for pointers to addresses
JET_SPRITE_PTR      word    ; pointer to P0 sprite lookup table
JET_COLOR_PTR       word    ; pointer to P0 color lookup table
BOMBER_SPRITE_PTR   word    ; pointer to P1 sprite lookup table
BOMBER_COLOR_PTR    word    ; pointer to P1 color lookup table

JET_ANIM_OFFSET     byte    ; P0 sprite frame offset

RANDOM              byte    ; random number generated to set enemy position

SCORE_SPRITE        byte    ; store the sprite bit pattern for the score
TIMER_SPRITE        byte    ; store the sprite bit pattern for the timer

TERRAIN_COLOR       byte    ; color of the terrain (grass patterns)
RIVER_COLOR         byte    ; color of the river

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT equ 9            ; P0 sprite height (# of rows in lookup table)
BOMBER_HEIGHT equ 9         ; P1 sprite height (# of rows in lookup table)
DIGITS_HEIGHT equ 5         ; scoreboard digit height (# of rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg CODE
	org $F000				; defines the origin of ROM at $F000

RESET:
	CLEAN_START				; macro to reset memory and addresses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta JET_YPOS            ; JET_YPOS = 10

    lda #60
    sta JET_XPOS            ; JET_XPOS = 60

    lda #26
    sta JET_XPOS_MIN
    lda #107
    sta JET_XPOS_MAX

    lda #0
    sta JET_YPOS_MIN
    lda #82
    sta JET_YPOS_MAX

    lda #83
    sta BOMBER_YPOS         ; BOMBER_YPOS = 83

    lda #54
    sta BOMBER_XPOS         ; BOMBER_XPOS = 54

    lda #%11010100
    sta RANDOM              ; RANDOM = $D4

    lda #0
    sta SCORE               ; SCORE = 0
    sta TIMER               ; TIMER = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declare a MACRO to check if we should display the missile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000      ; disable missile as default
        cpx MISSILE_YPOS    ; compare X (current scanline) with missile Y position
        bne .SKIP_MSL_DRAW  ; if value in X != MISSILE_YPOS, then skip draw
.DRAW_MSL:
        lda #%00000010      ; else enable M0 (missile) display
        inc MISSILE_YPOS    ; make missile go up with every scanline

.SKIP_MSL_DRAW:
        sta ENAM0           ; store the correct value in the TIA missile register
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize the pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JET_SPRITE        ; loads the LO-BYTE of the JET_SPRITE lookup table
    sta JET_SPRITE_PTR      ; store in JET_SPRITE_PTR
    lda #>JET_SPRITE        ; loads the HI-BYTE
    sta JET_SPRITE_PTR + 1  ; lo FIRST, THEN hi - little endian architecture

    lda #<JET_COLOR
    sta JET_COLOR_PTR
    lda #>JET_COLOR
    sta JET_COLOR_PTR + 1

    lda #<BOMBER_SPRITE
    sta BOMBER_SPRITE_PTR
    lda #>BOMBER_SPRITE
    sta BOMBER_SPRITE_PTR + 1

    lda #<BOMBER_COLOR
    sta BOMBER_COLOR_PTR
    lda #>BOMBER_COLOR
    sta BOMBER_COLOR_PTR + 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
START_FRAME:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display VSYNC and VBLANK before anything else
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK              ; turn on VBLANK
    sta VSYNC               ; turn on VSYNC

    REPEAT 3                ; note - NOT a loop
        sta WSYNC           ; 3 recommended lines of VSYNC
    REPEND

    lda #0
    sta VSYNC               ; turn off VSYNC

    REPEAT 32
        sta WSYNC           ; 37 recommended lines of VBLANK, 5 are used
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mute all audio on channel 1, i.e. missiles and P1 hit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta AUDV1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculations and tasks performed in the VBLANK - takes 4 lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JET_XPOS
    ldy #0
    jsr SET_OBJECT_XPOS     ; set P0 horizontal position

    lda BOMBER_XPOS
    ldy #1
    jsr SET_OBJECT_XPOS     ; set P1 horizontal position

    lda MISSILE_XPOS
    ldy #2
    jsr SET_OBJECT_XPOS     ; set P0 missile horizontal position

    jsr GET_DIGIT_OFFSET    ; calculate the scoreboard digit lookup table offset

    jsr GENERATE_JET_SOUND  ; configure and enable our jet engine audio

    sta WSYNC
    sta HMOVE               ; apply the horizontal offsets previously set

    lda #0
    sta VBLANK              ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1                ; reset TIA registers before displaying the score
    sta CTRLPF
    sta COLUBK              ; background color to black

    lda #$1E
    sta COLUPF              ; set the scoreboard playfield color to yellow

    ldx #DIGITS_HEIGHT      ; start X counter with 5 (height of digits)

.SCORE_DIGIT_LOOP:
    ldy TENS_DIGIT_OFFSET   ; get the tens digit offset for the SCORE
    lda DIGITS,Y            ; load the bit pattern from lookup table
    and #$F0                ; mask/remove the graphics for the ones digit
    sta SCORE_SPRITE        ; save the SCORE tens digit pattern in a variable

    ldy ONES_DIGIT_OFFSET   ; get the ones digit offset for the SCORE
    lda DIGITS,Y            ; load the digit bit pattern from lookup table
    and #$0F                ; mask/remove the graphics for the tens digit
    ora SCORE_SPRITE        ; merge it with the saved tens digit sprite
    sta SCORE_SPRITE        ; and save it
    sta WSYNC               ; wait for the end of scanline
    sta PF1                 ; update the playfield to display the SCORE sprite

    ldy TENS_DIGIT_OFFSET+1 ; get the left digit offset for the TIMER
    lda DIGITS,Y            ; load the digit pattern from lookup table
    and #$F0                ; mask/remove the graphics for the ones digit
    sta TIMER_SPRITE        ; save the TIMER tens digit pattern in a variable

    ldy ONES_DIGIT_OFFSET+1 ; get the ones digit offset for the TIMER
    lda DIGITS,Y            ; load digit pattern from the lookup table
    and #$0F                ; mask/remove the graphics for the tens digit
    ora TIMER_SPRITE        ; merge with the saved tens digit graphics
    sta TIMER_SPRITE        ; and save it

    jsr SLEEP_12_CYCLES     ; wastes some cycles

    sta PF1                 ; update the playfield for TIMER display

    ldy SCORE_SPRITE        ; preload for the next scanline
    sta WSYNC               ; wait for next scanline

    sty PF1                 ; update playfield for the SCORE display
    inc TENS_DIGIT_OFFSET
    inc TENS_DIGIT_OFFSET+1
    inc ONES_DIGIT_OFFSET
    inc ONES_DIGIT_OFFSET+1 ; increment all digits for the next line of data

    jsr SLEEP_12_CYCLES     ; waste some cycles

    dex                     ; decrement X
    sta PF1                 ; update the playfield for the TIMER display
    bne .SCORE_DIGIT_LOOP   ; if dex != 0, then branch to SCORE_DIGIT_LOOP

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC               ;
    sta WSYNC               ;
    sta WSYNC               ; three lines of padding

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display the remaining visible scanlines of our main game (2 line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GAME_VISIBLE_LINES:
    lda TERRAIN_COLOR
    sta COLUPF              ; set the terrain background color

    lda RIVER_COLOR
    sta COLUBK              ; set the river background color

    lda #%00000001
    sta CTRLPF              ; enable playfield reflection

    lda #$F0
    sta PF0                 ; setting PF0 bit pattern

    lda #$FC
    sta PF1                 ; setting PF1 bit pattern

    lda #0
    sta PF2                 ; setting PF2 bit pattern

    ldx #85                 ; frame counter is stored in X, using the 2-line kernel

.GAME_LINE_LOOP:            ; the dot in front is to indicate a nested loop
    ; using the 2-line kernel
    DRAW_MISSILE            ; macro to check if we should draw the missile

.INSIDE_JET_SPRITE:
    txa                     ; transfer X to A
    sec                     ; subtraction incoming - set carry flag
    sbc JET_YPOS            ; subtract sprite Y coordinate
    cmp #JET_HEIGHT         ; compare A with JET_HEIGHT - inside the sprite?
    bcc .DRAW_P0            ; if carry is now clear then draw P0
    lda #0                  ; else set lookup index to zero

.DRAW_P0:
    clc                     ; clear the carry before addition
    adc JET_ANIM_OFFSET     ; add animation offset to A (jump to correct frame address)

    tay                     ; transfer A to Y so we can work with the pointer
    lda (JET_SPRITE_PTR),Y  ; load P0 bitmap data from the lookup table with offset Y
    sta WSYNC               ; wait for the first scanline of P0 rendering
    sta GRP0                ; P0 bitmap is loaded in A, set graphics for P0

    lda (JET_COLOR_PTR),Y   ; same process for the color
    sta COLUP0              ; set P0 color

.INSIDE_BOMBER_SPRITE:
    txa
    sec
    sbc BOMBER_YPOS
    cmp #BOMBER_HEIGHT
    bcc .DRAW_P1
    lda #0

.DRAW_P1:
    tay

    lda #%00000101          ; maximum stretch pattern
    sta NUSIZ1              ; stretch P1 sprite

    lda (BOMBER_SPRITE_PTR),Y
    sta WSYNC
    sta GRP1

    lda (BOMBER_COLOR_PTR),Y
    sta COLUP1

    dex
    bne .GAME_LINE_LOOP     ; repeat next main game scanline until done

    lda #0
    sta JET_ANIM_OFFSET     ; reset jet animation offset to zero each frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK              ; turn on VBLANK again
    REPEAT 30
        sta WSYNC           ; display 30 recommended lines of VBLANK overscan
    REPEND

    lda #0
    sta VBLANK              ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process P0 joystick input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECK_P0_UP:
    lda #%00010000          ; P0 joystick up
    bit SWCHA
    bne CHECK_P0_DOWN       ; if bit pattern doesn't match bypass UP block

.P0_UP_PRESSED:
    lda JET_YPOS            ; checking if player is allowed to move UP
    cmp JET_YPOS_MAX
    beq END_INPUT_CHECK

    inc JET_YPOS            ; UP logic

CHECK_P0_DOWN:
    lda #%00100000          ; P0 joystick down
    bit SWCHA
    bne CHECK_P0_LEFT       ; if bit pattern doesn't match bypass DOWN block

.P0_DOWN_PRESSED:
    lda JET_YPOS            ; checking if player is allowed to move DOWN
    cmp JET_YPOS_MIN
    beq END_INPUT_CHECK

    dec JET_YPOS            ; DOWN logic

CHECK_P0_LEFT:
    lda #%01000000          ; P0 joystick down
    bit SWCHA
    bne CHECK_P0_RIGHT      ; if bit pattern doesn't match bypass LEFT block

.P0_LEFT_PRESSED:
    lda JET_XPOS            ; checking if player is allowed to move LEFT
    cmp JET_XPOS_MIN
    beq END_INPUT_CHECK

    dec JET_XPOS            ; LEFT logic
    lda #JET_HEIGHT         ; 9
    sta JET_ANIM_OFFSET     ; set animation offset to the second frame

CHECK_P0_RIGHT:
    lda #%10000000          ; P0 joystick down
    bit SWCHA
    bne CHECK_BTN           ; if bit pattern doesn't match bypass RIGHT block

.P0_RIGHT_PRESSED:
    lda JET_XPOS            ; checking if player is allowed to move RIGHT
    cmp JET_XPOS_MAX
    beq CHECK_BTN

    inc JET_XPOS            ; RIGHT logic
    lda #JET_HEIGHT         ; 9
    sta JET_ANIM_OFFSET     ; set animation offset to the second frame

CHECK_BTN:
    lda #%10000000          ; P0 joystick button pressed
    bit INPT4
    bne END_INPUT_CHECK     ; if bit pattern doesn't match bypass BTN block

.P0_BTN_PRESSED:
    lda JET_XPOS
    clc
    adc #5
    sta MISSILE_XPOS        ; load x-coordinate of P0 and set that as missile x-coordinate

    lda JET_YPOS
    clc
    adc #5
    sta MISSILE_YPOS        ; load y-coordinate of P0 and set that as missile y-coordinate

    jsr GENERATE_MISSILE_SOUND

END_INPUT_CHECK:            ; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UPDATE_BOMBER_POSITION:
    lda BOMBER_YPOS
    clc                     ; clear carry
    cmp #0                  ; compare bomber y-coordinate with 0
    bmi .RESET_BOMBER_POS   ; if it is negative, then reset bomber y-coordinate to the top
    dec BOMBER_YPOS         ; else move bomber down vertically every frame
    jmp END_POSITION_UPDATE

.RESET_BOMBER_POS
    jsr GET_RND_BOMBER_POS  ; call subroutine for random x-coordinate

.SET_SCORE_VALUES:
    ; TODO : use BCD to show decimal values of SCORE and TIMER
    sed                     ; set decimal mode for SCORE and TIMER values

    lda TIMER
    clc                     ; clear the carry before addition
    adc #1                  ; increment TIMER by 1
    sta TIMER               ; TIMER increments by 1 for every bomber launched

    cld                     ; clear decimal mode

END_POSITION_UPDATE:        ; fallback for position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check for collisions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CHECK_COLLISION_P0_P1:
    lda #%10000000          ; CXPPMM bit 7 detects P0-P1 collision
    bit CXPPMM              ; check CXPPMM bit 7 with the above pattern
    bne .CLSN_P0_P1         ; if collision between P0 and P1 happened - game over

    jsr SET_NO_CLSN_COLORS  ; set playfield color to green / blue

    jmp CHECK_COLLISION_M0_P1

.CLSN_P0_P1:
    jsr GAME_OVER           ; call GAME_OVER subroutine

CHECK_COLLISION_M0_P1:
    lda #%10000000          ; CXM0P bit 7 detects M0-P1 collision
    bit CXM0P               ; check CXM0P bit 7 with the above pattern
    bne .CLSN_M0_P1

    jmp END_CLSN_CHECK

.CLSN_M0_P1:
    sed                     ; set decimal mode
    lda SCORE
    clc
    adc #1
    sta SCORE               ; increase SCORE by 1 using decimal mode and store 
    cld                     ; clear decimal mode

    lda #0
    sta MISSILE_YPOS        ; reset the missile position

    jsr GENERATE_HIT_SOUND

.RESPAWN_BOMBER:
    lda #92
    sta BOMBER_YPOS         ; reset y-coordinate to starting point

    jsr GET_RND_BOMBER_POS  ; get new random x-coordinate

END_CLSN_CHECK:             ; fallback
    sta CXCLR               ; clear all collision flags before next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loop back to start new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp START_FRAME         ; continue to display next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate audio for the missile being fired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GENERATE_MISSILE_SOUND subroutine
    lda #4
    sta AUDV1               ; volume

    lda #15
    sta AUDF1               ; pitch

    lda #2
    sta AUDC1               ; tone type

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate audio for P1 being hit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GENERATE_HIT_SOUND subroutine
    lda #6
    sta AUDV1               ; volume

    lda #7
    sta AUDF1               ; pitch

    lda #3
    sta AUDC1               ; tone type

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate audio for the jet engine sound based on Y position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GENERATE_JET_SOUND subroutine
    lda #1
    sta AUDV0               ; volume

    lda JET_YPOS            ; take current Y position
    lsr                     ;
    lsr                     ;
    lsr                     ; perform 3x logical shift right to divide by 8
    sta TEMP                ; store in TEMP variable

    lda #31                 ; load 31 into A
    sec                     ; set carry
    sbc TEMP                ; subtract TEMP from A

    sta AUDF0               ; the result of subtraction is current frequency

    lda #8
    sta AUDC0               ; tone type

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine to set the color for the terrain and river to green / blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_NO_CLSN_COLORS subroutine
    lda #$09
    sta TERRAIN_COLOR       ; set color to green

    lda #$9E
    sta RIVER_COLOR         ; set color to blue

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0: player0, 1: player1, 2: missile0, 3: missile1, 4: ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SET_OBJECT_XPOS subroutine
    sta WSYNC                ; start a fresh new scanline
    sec                      ; make sure carry-flag is set before subtracion
.DIV_15_LOOP
    sbc #15                  ; subtract 15 from accumulator
    bcs .DIV_15_LOOP         ; loop until carry-flag is clear

    eor #7                   ; handle offset range from -8 to 7    
    asl
    asl
    asl
    asl                      ; four shift lefts to get only the top 4 bits

    sta HMP0,Y               ; store the fine offset to the correct HMxx
    sta RESP0,Y              ; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GAME_OVER subroutine
    lda #$30
    sta TERRAIN_COLOR       ; set terrain color to red
    sta RIVER_COLOR         ; set river color to red

    lda #0
    sta SCORE               ; SCORE = 0

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine to generate a linear-feedback shift register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate an LFSR random number
;; divide the random value by 4 to limit the size of the result to match the river
;; add 30 to compensate for the left green playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GET_RND_BOMBER_POS subroutine
    lda RANDOM
    asl
    eor RANDOM
    asl
    eor RANDOM
    asl
    asl
    eor RANDOM
    asl
    rol RANDOM              ; performs a series of shifts and bit operations

    lsr
    lsr                     ; divide the value by 4 by performing two right shifts
    sta BOMBER_XPOS         ; save the value to BOMBER_XPOS

    lda #30
    adc BOMBER_XPOS         ; add offset to compensate for the left green playfield
    sta BOMBER_XPOS         ; sets the new value to the BOMBER_XPOS

    lda #96
    sta BOMBER_YPOS         ; if it's out of bounds then reset to the top

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert the high and low nibbles of the variables SCORE and TIMER
;; into the offsets of digits lookup table so the values can be displayed,
;; where each digit has height of 5 bytes in the lookup table
;;
;; for the low nibble we need to multiply by 5 - we can use left shifts to
;; perform multiplication by 2, and then 2 again, and then finally perform
;; addition so that in the end we multiply by 5
;;
;; for the upper nibble since it's already times 16, we need to divide it
;; by 16 and then multiply by 5 - we can perform 4 right shifts to perform
;; division by 2^4, and then we multiply in the same way as above, so that
;; in the end we have (N/4) + (N/16)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GET_DIGIT_OFFSET subroutine
    ldx #1                  ; X register is the loop counter

.PREPARE_SCORE_LOOP         ; this will loop twice where X = 1, and then X = 0
    ; calculate ONES_DIGIT_OFFSET
    lda SCORE,X             ; load A with TIMER (X = 1) or SCORE (X = 0)
    and #%00001111          ; remove the tens digit by masking 4 bits
    sta TEMP                ; save the value of A into TEMP variable

    asl                     ;
    asl                     ;
    adc TEMP                ; multiply by 5

    sta ONES_DIGIT_OFFSET,X ; save A in ONES_DIGIT_OFFSET+1 or ONES_DIGIT_OFFSET+0

    ; calculate TENS_DIGIT_OFFSET
    lda SCORE,X             ; load A with TIMER (X = 1) or SCORE (X = 0)
    and #%11110000          ; remove the ones digit by masking 4 bits
    sta TEMP                ; save the value of A into TEMP variable

    lsr                     ;
    lsr                     ;
    sta TEMP                ; save the value of A into TEMP (it is N / 4)

    lsr                     ;
    lsr                     ; it is now N / 16
    adc TEMP                ; add the value saved in TEMP, i.e. we get N/4 + N/16

    sta TENS_DIGIT_OFFSET,X ; save A in TENS_DIGIT_OFFSET+1 or TENS_DIGIT_OFFSET+0

    dex                     ; X--
    bpl .PREPARE_SCORE_LOOP ; while X is positive loop to beginning

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles and rts also takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SLEEP_12_CYCLES subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DIGITS:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JET_SPRITE:
    .byte #%00000000        ; ........
    .byte #%00010100        ; ...#.#..
    .byte #%00111110        ; ..#####.
    .byte #%01111111        ; .#######
    .byte #%00011100        ; ...###..
    .byte #%00011100        ; ...###..
    .byte #%00001000        ; ....#...
    .byte #%00001000        ; ....#...
    .byte #%00001000        ; ....#...
; JET_HEIGHT = . - JET_SPR  ; subtract current mem address from the address at JET_SPRITE

JET_SPRITE_TURN:
    .byte #%00000000        ; ........
    .byte #%00001000        ; ....#...
    .byte #%00011100        ; ...###..
    .byte #%00111110        ; ..#####.
    .byte #%00011100        ; ...###..
    .byte #%00011100        ; ...###..
    .byte #%00001000        ; ....#...
    .byte #%00001000        ; ....#...
    .byte #%00001000        ; ....#...

BOMBER_SPRITE:
    .byte #%00000000        ; ........
    .byte #%00001000        ; ....#...
    .byte #%00001000        ; ....#...
    .byte #%00011100        ; ...###..
    .byte #%01001001        ; .#..#..#
    .byte #%01011101        ; .#.###.#
    .byte #%01111111        ; .#######
    .byte #%01111111        ; .#######
    .byte #%01011101        ; .#.###.#
; BOMBER_HEIGHT = . - BOMBER_SPR

JET_COLOR:
    .byte #$00
    .byte #$20;
    .byte #$44;
    .byte #$46;
    .byte #$44;
    .byte #$42;
    .byte #$40;
    .byte #$96;
    .byte #$30;

JET_COLOR_TURN:
    .byte #$00
    .byte #$20;
    .byte #$44;
    .byte #$46;
    .byte #$44;
    .byte #$42;
    .byte #$40;
    .byte #$96;
    .byte #$30;

BOMBER_COLOR:
    .byte #$AE;
    .byte #$C6;
    .byte #$C4;
    .byte #$CA;
    .byte #$B2;
    .byte #$B0;
    .byte #$D0;
    .byte #$E0;
    .byte #

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	org $FFFC				; defines origin to $FFFC
	.word RESET				; reset vector
	.word RESET				; interrupt vector
