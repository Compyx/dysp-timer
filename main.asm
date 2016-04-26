; vim: set et ts=8 sw=8 sts=8 syntax=64tass :
;
; DYSP cycle timing table generator
;
; 2016-04-25

        music_sid = "Louisiana.sid"
        music_init = $1000
        music_play = $1003

        * = $0801
        .word (+), 2016
        .null $9e, ^start
+       .word 0

start
        jsr $fda3
        jsr $fd15
        jsr $ff5b
        ldx #0
-       lda iface_text,x
        sta $0400,x
        inx
        cpx #iface_text_end-iface_text
        bne -
        sei
        lda #0
        jsr music_init
        lda #$35
        sta $01
        lda #$7f
        sta $dc0d
        sta $dd0d
        ldx #0
        stx $dc0e
        stx $dd0e
        stx $3fff
        lda #$01
        sta $d01a
        lda #$1b
        sta $d011
        lda #$29
        ldx #<irq1
        ldy #>irq1
        sta $d012
        stx $fffe
        sty $ffff
        ldx #<break
        ldy #>break
        stx $fffa
        sty $fffb
        stx $fffc
        sty $fffd
        bit $dc0d
        bit $dd0d
        inc $d019
        cli
        jmp *
irq1
        pha
        txa
        pha
        tya
        pha
        lda #$2a
        ldx #<irq2
        ldy #>irq2
        sta $d012
        stx $fffe
        sty $ffff
        lda #1
        inc $d019
        tsx
        cli
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
irq2
        txs
        ldx #8
-       dex
        bne -
        bit $ea
        lda $d012
        cmp $d012
        beq +
+
        ldx #$10
-       lda sprite_positions,x
        sta $d000,x
        dex
        bpl -
        lda #$ff
        sta $d015
        ldx #$10
-       dex
        bne -
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        ;lda $d012
        ;sta $0400
        nop
        nop
        nop
        nop
        jsr dysp
        lda #0
        sta $d021
        dec $d020
        jsr joystick2
        jsr calc_sprite_positions
        jsr set_delay
        jsr iface_update
        lda #0
        sta $d020
        lda #$f9
        ldx #<irq3
        ldy #>irq3
        sta $d012
        stx $fffe
        sty $ffff
        lda #1
        sta $d019
        pla
        tay
        pla
        tax
        pla
break   rti

irq3
        pha
        txa
        pha
        tya
        pha
        ldx #7
-       dex
        bne -
        stx $d011
        ldx #30
-       dex
        bne -
        lda #$1b
        sta $d011
        inc $d021
        jsr music_play
        dec $d021
        lda #$29
        ldx #<irq1
        ldy #>irq1
        sta $d012
        stx $fffe
        sty $ffff
        lda #1
        sta $d019
        pla
        tay
        pla
        tax
        pla
        rti


set_delay
        ldx #0
        ldy sprite_enable
        lda cycles,y
-       sta delay,x
        inx
        cpx #21
        bne -
        rts



        .align 256      ; avoid page boundary crossing in raster bars
dysp
        ldy #8
        ldx #0
-       lda d011_table + 0,x
        dec $d016
        sta $d011
        sty $d016
        lda d011_table,x
        sta $d021

        lda timing,x
        sta _delay + 1
_delay  bpl * + 2
        cpx #$e0
        cpx #$e0
        cpx #$e0
        cpx #$e0
        cpx #$e0
        cpx #$e0
        cpx #$e0
        cpx #$e0
        bit $ea
      ; lda d011_table,x
        ;sta $d021
        ;sta $d021
       inx
        cpx #64
        bne -
        rts

sprite_positions
        .byte $00, $32
        .byte $18, $32

        .byte $30, $32
        .byte $48, $32

        .byte $60, $32
        .byte $78, $a0

        .byte $90, $a0
        .byte $a8, $a0
        .byte $00

; d015 value
sprite_enable   .byte $00


CS_MIN = 0
CS_MAX = 17

sprite_bitmasks .byte $01, $02, $04, $08, $10, $20, $40, $80

calc_sprite_positions
        ldx #0
        ldy #0
-       lda sprite_enable
        and sprite_bitmasks,x
        beq +
        lda #$32        ; inside raster
        bne ++
+       lda #$a0        ; outside raster
+       sta sprite_positions + 1,y
        inx
        iny
        iny
        cpx #8
        bne -
        rts


iface_update
        ; sprite enable hex
        lda sprite_enable
        jsr hex_digits
        sta $0408
        stx $0409
        ; cycle skip for sprite enable
        ldx sprite_enable
        lda cycles,x
        jsr hex_digits
        sta $0426
        stx $0427

        ; sprite enable bit pattern
        ldy #7
        ldx #0
-       lda sprite_enable
        and sprite_bitmasks,x
        beq +
        lda #$31
        bne ++
+       lda #$30
+       sta $040c,y
        dey
        inx
        cpx #8
        bne -


        rts

hex_digits
        pha
        and #$0f
        cmp #$0a
        bcs +
        adc #$3a
+       sbc #$09
        tax
        pla
        lsr
        lsr
        lsr
        lsr
        cmp #$0a
        bcs +
        adc #$3a
+       sbc #$09
        rts


JOY_UP = $01
JOY_DOWN = $02
JOY_LEFT = $04
JOY_RIGHT = $08
JOY_FIRE = $10


joystick2
joy2_delay
        lda #8
        beq +
        dec joy2_delay + 1
        rts
+       lda $dc00
        sta $0450
        and #%00011111
        eor #%00011111
        bne +
        rts
+
        lda #8
        sta joy2_delay + 1
        ; rts
        lda $0450
        and #JOY_UP
        beq joy2_up
        lda $0450
        and #JOY_DOWN
        beq joy2_down
        lda $0450
        and #JOY_LEFT
        beq joy2_left
        lda $0450
        and #JOY_RIGHT
        beq joy2_right
        rts
joy2_up
        inc sprite_enable
        rts
joy2_down
        dec sprite_enable
        rts
joy2_left
        ldx sprite_enable
        lda cycles,x
        cmp #CS_MIN
        beq +
        dec cycles,x
+       rts
joy2_right
        ldx sprite_enable
        lda cycles,x
        cmp #CS_MAX
        beq +
        inc cycles,x
+       rts


iface_text
        .enc screen
        ;      0123456789012345678901234567890123456789
        .text "$d015: $00/%00000000     cycle skip: $00"
        .text "joy up/down               joy left/right"
iface_text_end





        .align 256
d011_table
.for row = 0, row < 64, row = row + 1
        .byte $18 + ((row + 3) & 7)
.next


; cycle delay table
timing
        .fill 2, 0      ; don't touch this, raster code starts early

        ; raster $32 starts here

delay   .fill 21, 17
        .fill 64, 0




        .align 256
; number of cycles to skip in the branch
cycles
;       skip cycles     $d015                   sprite(s) active

        ; $00-$07
        .byte 0         ; $00 - %00000000       no sprites
        .byte 3         ; $01 - %00000001                     0
        .byte 5         ; $02 - %00000010                   1
        .byte 5         ; $03 - %00000011                   1 0

        ; $04-$07
        .byte 5         ; $04 - %00000100                 2
        .byte 7         ; $05 - %00000101                 2   0
        .byte 7         ; $06 - %00000110                 2 1
        .byte 7         ; $07 - %00000111                 2 1 0

        ; $08-$0b
        .byte 5         ; $08 - %00001000               3
        .byte 8         ; $09 - %00001001               3     0
        .byte 9         ; $0a - %00001010               3   1
        .byte 9         ; $0b - %00001011               3   1 0

        ; $0c-$0f
        .byte 7         ; $0c - %00001100               3 2
        .byte 9         ; $0d - %00001101               3 2   0
        .byte 9         ; $0e - %00001110               3 2 1 
        .byte 9         ; $0f - %00001111               3 2 1 0

        ; $10-$13
        .byte 5         ; $10 - %00010000             4
        .byte 7         ; $11 - %00010001             4       0
        .byte 10        ; $12 - %00010010             4     1
        .byte 10        ; $13 - %00010011             4     1 0

        ; $14-$17
        .byte 9         ; $14 - %00010100             4   2
        .byte 11        ; $15 - %00010101             4   2   0
        .byte 11        ; $16 - %00010110             4   2 1
        .byte 11        ; $17 - %00010111             4   2 1 0

        ; $18-$1b
        .byte 7         ; $18 - %00011000             4 3
        .byte 10        ; $19 - %00011001             4 3     0
        .byte 11        ; $1a - %00011010             4 3   1
        .byte 11        ; $1b - %00011011             4 3   1 0

        ; $1c-$1f
        .byte 9         ; $1c - %00011100             4 3 2
        .byte 11        ; $1d - %00011101             4 3 2   0
        .byte 11        ; $1e - %00011110             4 3 2 1
        .byte 11        ; $1f - %00011111             4 3 2 1 0

        ; $20-$2f
        .byte $05, $08, $09, $09
        .byte $09, $0c, $0c, $0c
        .byte $09, $0c, $0d, $0d
        .byte $0b, $0d, $0d, $0d

        ; $30-$3f
        .byte $07, $09, $0c, $0c
        .byte $0b, $0d, $0d, $0d
        .byte $09, $0c, $0d, $0d
        .byte $0b, $0d, $0d, $0d

        ; $40-$4f
        .byte $05, $07, $0a, $0a
        .byte $0a, $0b, $0b, $0b
        .byte $0a, $0d, $0e, $0e
        .byte $0b, $0e, $0e, $0e

        ; $50-$5f
        .byte $09, $0b, $0e, $0e
        .byte $0d, $0f, $0f, $0f
        .byte $0b, $0e, $0f, $0f
        .byte $0d, $0f, $0f, $0f

        ; $60-$6f
        .byte $07, $0a, $0b, $0b
        .byte $0b, $0e, $0e, $0e
        .byte $0b, $0e, $0f, $0f
        .byte $0d, $0f, $0f, $0f

        ; $70-$7f
        .byte $09, $0b, $0e, $0e
        .byte $0d, $0f, $0f, $0f
        .byte $0b, $0e, $0f, $0f
        .byte $0d, $0f, $0f, $0f

        ; $80-$8f
        .byte $05, $08, $09, $09
        .byte $09, $0c, $0c, $0c
        .byte $09, $0d, $0d, $0d
        .byte $0c, $0d, $0d, $0d

        ; $90-$9f
        .byte $09, $0c, $0f, $0f
        .byte $0d, $10, $10, $10
        .byte $0c, $0f, $10, $10
        .byte $0d, $10, $10, $10

        ; $a0-$af
        .byte $09, $0c, $0d, $0d
        .byte $0d, $10, $10, $10
        .byte $0d, $10, $11, $11
        .byte $0f, $11, $11, $11

        ; $b0-$bf
        .byte $0b, $0d, $10, $10
        .byte $0f, $11, $11, $11
        .byte $0d, $10, $11, $11
        .byte $0f, $11, $11, $11
        
        ; $c0-$cf
        .byte $07, $09, $0c, $0c
        .byte $0c, $0d, $0d, $0d
        .byte $0c, $0f, $10, $10
        .byte $0d, $10, $10, $10

        ; $d0-$df
        .byte $0b, $0d, $10, $10
        .byte $0f, $11, $11, $11
        .byte $0d, $10, $11, $11
        .byte $0f, $11, $11, $11

        ; $e0-$ef
        .byte $09, $0c, $0d, $0d
        .byte $0d, $10, $10, $10
        .byte $0d ,$10, $11, $11
        .byte $0f, $11, $11, $11

        ; $f0-$ff
        .byte $0b, $0d, $10, $10
        .byte $0f, $11, $11, $11
        .byte $0d, $10, $11, $11
        .byte $0f, $11, $11, $11


.cerror * > $0fff, "code section too large!"



; Link music
        * = $1000
.binary music_sid, $7e

