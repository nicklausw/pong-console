; main file to my project, pong.


; banking
.memorymap
defaultslot 0
slotsize $8000
slot 0 $0000
slotsize $2000
slot 1 $c000
.endme
.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro


.include "tiles.asm"

; compression for tiles
.include "aplib-z80.asm"
.ramsection "aPlibstuff" bank 0 slot 1
aPLibMemory instanceof aPLibMemoryStruct
.ends

; the usual definitions
.define VDPControl $bf
.define VDPData $be
.define VRAMWrite $4000
.define CRAMWrite $c000

; all this stuff......
.smstag
.sdsctag 0.5, "pong", commentary, "nicklausw"

.section "commentary" free
commentary:
.db "There's no actual in-game changes in this"
.db "version. I just added comments on the code"
.db "where the range is calculated. Enjoy?" ; "
.ends


.struct paddle
    x db ; the paddle's
    y db ; coordinates
    y8 db ; collision check
    score db
.endst

.struct ball_struct
    velo_x db ; velocity
    velo_y db ; directions
    y8 db ; collision check
    x db ; the ball's
    y db ; coordinates
.endst


.ramsection "ram" bank 0 slot 1
    controller db
    game db
    calc db
    p1 instanceof paddle
    p2 instanceof paddle
    ball instanceof ball_struct
.ends


.bank 0 slot 0
.org $00
.section "begin" force
    di ; no interrupts
    im 1 ; go to $38 for interrupts
    ld sp,$dff0 ; load stack pose
    jp main ; go!
.ends


.org $38
.section "interrupt" force
    push af ; this part
    in a,$bf ; is required
    pop af ; for interrupts
    
    exx ; preserve all
    push af ; variables
    
    call GetCtrl ; get controls
    call GameHandle ; handle stuff
    
    call TileUpdate ; update
    call PrintScore ; the screen
    
    pop af ; bring
    exx ; variables back
    
    ei ; enable interrupts
    reti ; done.
.ends


.org $66
.section "pause" force
    retn ; nah. for now.
.ends


.section "main" free
main:
    call VDPStuff


    ; Clear RAM
    ld hl,$c000 ; ram location
    
-:  xor a ; set a to 0
    ld [hl],a ; clear byte
    inc hl ; go to next byte
    ld a,h ; check higher byte of hl
    cp $e0 ; are we done?
    jr nz,- ; if no, go back


    ; Clear VDP
    ld bc, $4000 ; vdp location
    call SetVDP
    
-:  xor a ; set a to 0
    out [VDPData],a ; clear byte
    dec bc ; lower counter by 1
    ld a,b ; load a with higher byte of bc
    or c ; all 0'd?
    jr nz,- ; nope, go back


    ld hl,$0000 | CRAMWrite
    call SetVDP
    ld hl,PaletteData
    ld bc,PaletteDataEnd-PaletteData
    call CopyVDP

    ld hl,$0010 | CRAMWrite
    call SetVDP
    ld hl,PaletteData
    ld bc,PaletteDataEnd-PaletteData
    call CopyVDP

    ; load tiles
    ld hl,tiles
    ld de,$0020
    di
    call vram_depack

    call ResetPoses

    ei
-:  halt
    jr -
.ends


.section "tile" free
TileUpdate:
    push hl
    push af

    call ScreenOff

    ld hl,$7f80
    call SetVDP

    ld a, [p1.x]
    out (VDPData), a
    ld a, $1
    out (VDPData), a

    ld a, [p1.x]
    out (VDPData), a
    ld a, $1
    out (VDPData), a

    ld a, [p1.x]
    out (VDPData), a
    ld a, $1
    out (VDPData), a


    ld a, [p2.x]
    out (VDPData), a
    ld a, $3
    out (VDPData), a

    ld a, [p2.x]
    out (VDPData), a
    ld a, $3
    out (VDPData), a
    
    ld a, [p2.x]
    out (VDPData), a
    ld a, $3
    out (VDPData), a

    ld a, [ball.x]
    out (VDPData), a
    ld a, $2
    out (VDPData), a

    ld hl,$7f00
    call SetVDP

    ld a, [p1.y]
    out (VDPData), a
    ld a, $cf
    out (VDPData), a
    
    ld hl,$7f01
    call SetVDP

    ld a, [p1.y]
    sbc a,7
    out (VDPData), a
    ld a, $cf
    out (VDPData), a

    ld hl,$7f02
    call SetVDP

    ld a, [p1.y]
    adc a,8
    out (VDPData), a
    ld a, $cf
    out (VDPData), a

    ld hl,$7f03
    call SetVDP

    ld a, [p2.y]
    sbc a,8
    out (VDPData), a
    ld a, $cf
    out (VDPData), a

    ld hl,$7f04
    call SetVDP

    ld a, [p2.y]
    adc a,8
    out (VDPData), a
    ld a, $cf
    out (VDPData), a

    ld hl,$7f05
    call SetVDP

    ld a, [p2.y]
    out (VDPData), a
    ld a, $cf
    out (VDPData), a

    ld hl,$7f06
    call SetVDP

    ld a, [ball.y]
    out (VDPData), a
    ld a, $cf
    out (VDPData), a

    call ScreenOn
    pop af
    pop hl
    ret
.ends


.section "GameHandle" free
GameHandle:
    ld a,[game]
    cp 1
    call z, ButtonHandle
    call z, BallUpdate

    ld a,[game]
    cp 1
    call z, BallUpdate
    ld a,[game]
    cp 1
    call z, CheckScore
    call nz,CheckOnePress
    ret

CheckOnePress:
    ld a,[controller]
    bit 4,a
    call z, GameOn
    ret

GameOn:
    ld a,1
    ld [game],a
    ret
.ends


.section "score" free
Freeze:
    xor a
    ld [game],a
    -: jr -

CheckScore:
    ld a,[p1.score]
    cp 15
    call z,Freeze

    ld a,[p2.score]
    cp 15
    call z,Freeze

    ld a,[ball.x]
    cp $0f
    call z,NewRoundP1
    cp $Ef
    call z,NewRoundP2
    ret


ResetPoses:
    ld [game],a
    ld a,$75
    ld [ball.x],a
    ld a,$60
    ld [ball.y],a
    ld a,$60
    ld [p1.y],a
    ld [p2.y],a
    ld a,$20
    ld [p1.x],a
    ld a,$CD
    ld [p2.x],a
    ret



NewRoundP1:
    xor a
    call ResetPoses
    ld a,[p1.score]
    inc a
    ld [p1.score],a
    ret

NewRoundP2:
    xor a
    call ResetPoses
    ld a,[p2.score]
    inc a
    ld [p2.score],a
    ret
.ends


.section "ball" free
BallLeft:
    ld a,[ball.x]
    dec a
    ld [ball.x],a
    ret

BallRight:
    ld a,[ball.x]
    inc a
    ld [ball.x],a
    ret


BallUp:
    ld a,[ball.y]
    cp 1
    jr nz,++
    ld a,$1
    ld [ball.velo_y],a
    jr BallDown
++: dec a
    ld [ball.y],a
    ret

BallDown:
    ld a,[ball.y]
    cp $BB
    jr nz,++
    xor a
    ld [ball.velo_y],a
    jr BallUp
++: inc a
    ld [ball.y],a
    ret


BallUpdate:
    ld a,[ball.velo_x]
    cp 0
    call z,BallLeft
    ld a,[ball.velo_x]
    cp 1
    call z,BallRight

    ld a,[ball.velo_y]
    cp 0
    call z,BallUp
    ld a,[ball.velo_y]
    cp 1
    call z,BallDown

    ld a,[ball.y]
    ld b,a
    ld a,[p1.y]
    call calcrange
    ld a,[calc]
    ld [p1.y8],a

    ld a,[ball.y]
    ld b,a
    ld a,[p2.y]
    call calcrange
    ld a,[calc]
    ld [p2.y8],a

    ld a,[ball.y]
    ld b,a
    ld a,[ball.y]
    call calcrange
    ld a,[calc]
    ld [ball.y8],a


    call BallP1X
    call BallP2X
    ret


BallP1X:
    ld a,[p1.x]
    adc a,8
    ld b,a
    ld a,[ball.x]
    cp b
    call z,BallP1Y
    ret

BallP1Y:
    ld a,[p1.y8]
    cp 2
    call z,P1Col
    ret

P1Col:
    ld a,[ball.velo_x]
    xor 1
    ld [ball.velo_x],a
    ret

BallP2X:
    ld a,[p2.x]
    sbc a,8
    ld b,a
    ld a,[ball.x]
    cp b
    call z,BallP2Y
    ret

BallP2Y:
    ld a,[p2.y8]
    cp 2
    call z,P2Col
    ret

P2Col:
    ld a,[ball.velo_x]
    xor 1
    ld [ball.velo_x],a
    ret
.ends


.section "data" free
SetVDP:
    push bc
    ld c,VDPControl
    out (c),l
    out (c),h
    pop bc
    ret

CopyVDP:
-:  ld a,(hl)    ; Get data byte
    out (VDPData),a
    inc hl       ; Point to next letter
    dec bc
    ld a,b
    or c
    jr nz,-
    ret


.asciitable
map " " to "~" = 0
.enda

PaletteData:
.db $00,$3f ; Black, white
PaletteDataEnd:
.ends

.section "controller" free
GetCtrl:
    push af
    in a,$dc
    ld [controller],a
    pop af
    ret

ButtonHandle:
    push af
    ld a,[controller]

    bit 0,a
    call z, _MoveUp

    bit 1,a
    call z, _MoveDo

    bit 6,a
    call z, _MoveLe

    bit 7,a
    call z, _MoveRi

    pop af
    ret

_MoveUp:
    push af
    ld a,[p1.y]
    cp 9
    jr z,+
    cp 8
    jr z,+
    sbc a,3
+:  ld [p1.y],a
    pop af
    ret

_MoveDo:
    push af
    ld a,[p1.y]
    cp $B0
    jr z,+
    cp $AF
    jr z,+
    adc a,3
+:  ld [p1.y],a
    pop af
    ret

_MoveLe:
    push af
    ld a,[p2.y]
    cp 9
    jr z,+
    cp 8
    jr z,+
    sbc a,3
+:  ld [p2.y],a
    pop af
    ret

_MoveRi:
    push af
    ld a,[p2.y]
    cp $B0
    jr z,+
    cp $AF
    jr z,+
    adc a,3
+:  ld [p2.y],a
    pop af
    ret
.ends


.section "VDP" free
Print:
    xor a
    out (VDPData),a
    ld a,[hl]
    daa
    ld b,a
    .rept 5
    srl a
    .endr
    adc a,4
    out (VDPData),a
    xor a
    out (VDPData),a
    ld a,b
.rept 4
    rl a
.endr

.rept 4
    srl a
.endr

    adc a,4
    out (VDPData),a
    ret
    

PrintScore:
    ld hl,$3C40
    call SetVDP
    ld hl,p1.score
    call Print

    ld hl,$3C80
    call SetVDP
    ld hl,p2.score
    call Print
    ret

ScreenOn:
    ld a,%01100000
    out (VDPControl),a
    ld a,$81
    out (VDPControl),a
    ret

ScreenOff:
    ld a,%00100000
    out (VDPControl),a
    ld a,$81
    out (VDPControl),a
    ret

; VDP initialisation data
VDPStuff:
    ld hl,VDPI
    ld b,$80
    ld c,VDPE-VDPI
-:  ld a,[hl]
    out [VDPControl],a
    ld a,b
    out [VDPControl],a
    inc hl
    dec c
    inc b
    ld a,c
    cp 0
    jr nz,-
    ret

VDPI:
.db $6 $a0 $ff $ff $ff $ff $00 $00 $00 $00 $ff
VDPE:
.ends


.section "range" free
calcrange:

    ; a = paddle y
    ; b = ball y

    ; with this part,
    ; we'll make sure the ball
    ; is within range of the top
    ; of the paddle

    ; a = (ball y)-13
    ; b = ball y
    ; c = paddle y
    ld c,a
    ld a,b
    sbc a,13
    jr c,++ ; it's in range, we're done

    ; a = paddle y
    ; b = (ball y)-13
    ; c = paddle y
    ld b,a
    ld a,c


    cp b ; compare paddle y to (ball y)-13
    jr z,++ ; they're the same
    jr nc,+ ; it's in range, keep going
    jp calcno ; not in range


    ; a = (ball y)-13
    ; b = (ball y)-13
    ; c = paddle y
+:  ld c,a
    ld a,b
    adc a,35 ; a = (ball y)+22
    jr c,++ ; it's in range, we're done

    ; a = paddle y
    ; b = (ball y)+22
    ld b,a
    ld a,c

    cp b ; compare a to b
    jr z,++ ; they're the same
    jr c,++ ; it's in range, keep going
    jp calcno ; not in range

    ++: ld a,2 ; it was in range
    ld [calc],a
    ret


calcno:
    ld a,1 ; it wasn't in range
    ld [calc],a
    ret
.ends
