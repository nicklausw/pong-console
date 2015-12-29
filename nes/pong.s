.p02

; defines
PPU_CTRL1        = $2000
PPU_CTRL2        = $2001
PPU_STATUS       = $2002
PPU_SPR_ADDR     = $2003
PPU_SPR_IO       = $2004
PPU_VRAM_ADDR1   = $2005
PPU_VRAM_ADDR2   = $2006
PPU_VRAM_IO      = $2007

APU_MODCTRL      = $4010
APU_SPR_DMA      = $4014

APU_PAD1         = $4016
APU_PAD2         = $4017

BTN_CHECK        = %00000001

SPR_ENABLED      = %00010000
BG_ENABLED       = %00001000
NO_L_CLIP        = %00000010

NMI_ENABLED      = %10000000
SPRITES_8x16     = %00100000
BG_PT_ADDR_O     = %00010000
SP_PT_ADDR_O     = %00001000
VRAM_INC         = %00000100
NT_20            = %00000000
NT_24            = %00000001
NT_28            = %00000010
NT_2C            = %00000011

paddle_1_pos = $10
paddle_2_pos = $e0

.segment "ZEROPAGE"

  chr_ram_v: .res 2
  nametable_var: .res 2
  playing: .res 1
  temp: .res 1

 .scope buttons
  .scope one
   ptr:
   btn_a:  .res 1
   btn_b:  .res 1
   start:  .res 1
   select: .res 1
   up:     .res 1
   down:   .res 1
   left:   .res 1
   right:  .res 1
  .endscope

  .scope two
   ptr:
   btn_a:  .res 1
   btn_b:  .res 1
   start:  .res 1
   select: .res 1
   up:     .res 1
   down:   .res 1
   left:   .res 1
   right:  .res 1
  .endscope
 .endscope

 .scope ball_velo
  h: .res 1
  v: .res 1
 .endscope

 .scope player
  .scope one
   pos:   .res 1
   score: .res 1
  .endscope

  .scope two
   pos:   .res 1
   score: .res 1
  .endscope
 .endscope



.segment "OAMRAM"
 .scope ball
  y_pos: .res 1
  tile:  .res 1
  attr:  .res 1
  x_pos: .res 1
 .endscope

 .scope paddle
  .scope one
   y_pos: .res 1
   tile:  .res 1
   attr:  .res 1
   x_pos: .res 1
  .endscope
  .scope one2
   y_pos: .res 1
   tile:  .res 1
   attr:  .res 1
   x_pos: .res 1
  .endscope
  .scope one3
   y_pos: .res 1
   tile:  .res 1
   attr:  .res 1
   x_pos: .res 1
  .endscope

  .scope two
   y_pos: .res 1
   tile:  .res 1
   attr:  .res 1
   x_pos: .res 1
  .endscope
  .scope two2
   y_pos: .res 1
   tile:  .res 1
   attr:  .res 1
   x_pos: .res 1
  .endscope
  .scope two3
   y_pos: .res 1
   tile:  .res 1
   attr:  .res 1
   x_pos: .res 1
  .endscope
 .endscope





.segment "HEADER"
; the header!
.byt "NES", $1a ; ines header
.byt $01 ; PRG-ROM pages (16kb)


.segment "VECTORS"
.word nmi, reset ; no irq!


.segment "CODE"

.proc reset
  sei ; no interrupts
  cld ; no decimal mode

 .repeat 3 ; 3 vblanks,
  ; for the sake of
  ; safety.
  jsr vblank_cycle
 .endrep


  ; load all ram with 0's.
  ; ld a with 0, then store
  ; a into the location + x.
  lda #$00
  ldx #$00

: sta $000,x
  sta $100,x
  sta $200,x
  sta $300,x
  sta $400,x
  sta $500,x
  sta $600,x
  sta $700,x
  inx
  bne :-


  ldx #$FF
  txs ; set up stack


  ; we need to blank some
  ; more stuff to prevent
  ; problems in initialization
  ldx #$00
  stx PPU_CTRL1
  stx PPU_CTRL2
  stx APU_MODCTRL


  jsr clear_nametable


  ; time to load the palette.
  ; write to $3F00
  lda #$3F
  sta PPU_VRAM_ADDR2
  lda #$00
  sta PPU_VRAM_ADDR2

  ldx #$00 ; counter
  ldy #$00 ; counter2

: lda palette,x
  sta PPU_VRAM_IO ; write to ppu
  inx
  cpx #$4; is it $4?
  bne :-  ; if not, go to -

  iny
  ldx #$00
  cpy #$8
  bne :-


  ; load chr data (thanks whoever put this on the nesdev wiki)
  lda #<data  ; load the source address into a pointer in zero page
  sta chr_ram_v
  lda #>data
  sta chr_ram_v+1


  ldy #0       ; starting index into the first page
  sty PPU_CTRL2  ; turn off rendering just in case


  lda #$00
  sta PPU_VRAM_ADDR2  ; load the destination address into the PPU
  sta PPU_VRAM_ADDR2
  ldx #1      ; number of 256-byte pages to copy


: lda (<chr_ram_v),y  ; copy one byte
  sta PPU_VRAM_IO
  iny
  bne :-  ; repeat until we finish the page
  inc chr_ram_v+1  ; go to the next page
  dex
  bne :-  ; repeat until we've copied enough pages


  ; load attributes
  lda PPU_STATUS
  lda #$23
  sta PPU_VRAM_ADDR2
  lda #$C0
  sta PPU_VRAM_ADDR2

  lda #$00
  sta PPU_VRAM_IO


  lda #$02
  sta ball::tile
  lda #$01
  sta paddle::one::tile
  sta paddle::one2::tile
  sta paddle::one3::tile
  lda #$03
  sta paddle::two::tile
  sta paddle::two2::tile
  sta paddle::two3::tile

  lda #paddle_1_pos
  sta paddle::one::x_pos
  sta paddle::one2::x_pos
  sta paddle::one3::x_pos
  lda #paddle_2_pos
  sta paddle::two::x_pos
  sta paddle::two2::x_pos
  sta paddle::two3::x_pos


  jsr refresh_paddles

  jsr refresh_ball


  lda #BG_ENABLED|SPR_ENABLED|NO_L_CLIP
  sta PPU_CTRL2

  lda #NMI_ENABLED
  sta PPU_CTRL1


: jsr vblank_cycle
  jmp :-
.endproc


.proc vblank_cycle
  lda PPU_STATUS
  bpl vblank_cycle
  rts
.endproc


.proc refresh_paddles
  lda #$71
  sta paddle::one::y_pos
  sta paddle::two::y_pos

  lda #$71+8
  sta paddle::one2::y_pos
  sta paddle::two2::y_pos

  lda #$71+16
  sta paddle::one3::y_pos
  sta paddle::two3::y_pos
  rts
.endproc


.proc refresh_ball
  lda #$7f
  sta ball::x_pos
  sta ball::y_pos
  rts
.endproc


.proc clear_nametable
  lda #$20
  sta PPU_VRAM_ADDR2
  lda #$00
  sta PPU_VRAM_ADDR2

  lda #$00
  ldy #$00

  jsr clear_loop

  lda #$28
  sta PPU_VRAM_ADDR2
  lda #$00
  sta PPU_VRAM_ADDR2

  lda #$00
  ldy #$00

  jsr clear_loop
  rts

  ; 4*$FF = $400, one nametable
clear_loop:
  sta PPU_VRAM_IO
  sta PPU_VRAM_IO
  sta PPU_VRAM_IO
  sta PPU_VRAM_IO
  dey
  bne clear_loop
  rts
.endproc


.proc nmi
  ; prepare the ram data for
  ; transfer to display unit
  lda #$00
  sta PPU_SPR_ADDR
  lda #$02
  sta APU_SPR_DMA

  jsr get_input

  lda buttons::one::btn_a
  cmp #$01
  bne :+
  sta playing


: lda playing
  cmp #$01
  bne end

  lda playing
  cmp #$01
  bne end

  jsr check_y_col
  jsr move_ball
  jsr check_x_col
  jsr move_paddles

end:
  jsr update_score
  jsr update_paddles
  rti
.endproc


.proc get_input
  lda #$01
  sta APU_PAD1
  lda #$00
  sta APU_PAD1

  ldx #$00

: lda APU_PAD1
  and #$1
  sta buttons::one::ptr,x
  inx
  cpx #$08
  bne :-
  rts
.endproc


.proc check_x_col
  lda ball::x_pos
  cmp #paddle_1_pos+7
  bne check_paddle_2

  jsr check_paddle_1_col

check_paddle_2:
  cmp #paddle_2_pos-7
  bne done

  jsr check_paddle_2_col

done:
  rts

check_paddle_1_col:
  lda paddle::one::y_pos
  sec
  sbc #$08
  sta temp

  lda ball::y_pos
  cmp temp
  bcs paddle_1_not_less_than

  jmp player_2_point

paddle_1_not_less_than:
  lda paddle::one3::y_pos
  clc
  adc #$0f
  sta temp

  lda ball::y_pos
  cmp temp
  bcc paddle_1_collide

  jmp player_2_point

paddle_1_collide:
  lda ball_velo::h
  eor #$01
  sta ball_velo::h
  rts



check_paddle_2_col:
  lda paddle::two::y_pos
  sec
  sbc #$08
  sta temp

  lda ball::y_pos
  cmp temp
  bcs paddle_2_not_less_than

  jmp player_1_point

paddle_2_not_less_than:
  lda paddle::two3::y_pos
  clc
  adc #$0f
  sta temp

  lda ball::y_pos
  cmp temp
  bcc paddle_2_collide

  jmp player_1_point


paddle_2_collide:
  lda ball_velo::h
  eor #$01
  sta ball_velo::h
  rts



player_1_point:
  ; player 1 gets a point
  ldx player::one::score
  inx
  jsr hex2dec
  stx player::one::score

  jsr refresh_paddles
  jsr refresh_ball

  lda #$00
  sta playing
  rts


player_2_point:
  ; player 2 gets a point
  ldx player::two::score
  inx
  jsr hex2dec
  stx player::two::score

  jsr refresh_paddles
  jsr refresh_ball

  lda #$00
  sta playing
  rts
.endproc


.proc hex2dec
  txa ; load a with x
  and #$0F ; lower bit only
  cmp #$0A ; is it not decimal
  bcc done ; we're done here if not

  txa ; load a with x again
  and #$F0 ; higher bit only
  cmp #$90 ; is it at 99?
  bne not_99 ; if not, skip next part

  lda #$0 ; back to 0
  jmp was_99 ; no need for the rest

not_99:
  clc ; clear carry
  adc #$10 ; add 10

was_99:
  tax ; give a back to x

done:
  rts
.endproc


.proc move_paddles
  lda buttons::one::up
  cmp #$01
  bne check_down

  ldx paddle::one::y_pos
  dex
  dex
  stx paddle::one::y_pos

  ldx paddle::one2::y_pos
  dex
  dex
  stx paddle::one2::y_pos

  ldx paddle::one3::y_pos
  dex
  dex
  stx paddle::one3::y_pos

check_down:
  lda buttons::one::down
  cmp #$01
  bne move_2

  ldx paddle::one::y_pos
  inx
  inx
  stx paddle::one::y_pos

  ldx paddle::one2::y_pos
  inx
  inx
  stx paddle::one2::y_pos

  ldx paddle::one3::y_pos
  inx
  inx
  stx paddle::one3::y_pos

move_2:
  lda paddle::two2::y_pos
  cmp ball::y_pos
  beq done
  bcc move_2_up

  ; move 2 down
  ldx paddle::two::y_pos
  dex
  dex
  stx paddle::two::y_pos

  ldx paddle::two2::y_pos
  dex
  dex
  stx paddle::two2::y_pos

  ldx paddle::two3::y_pos
  dex
  dex
  stx paddle::two3::y_pos
  jmp done

move_2_up:
  ldx paddle::two::y_pos
  inx
  inx
  stx paddle::two::y_pos

  ldx paddle::two2::y_pos
  inx
  inx
  stx paddle::two2::y_pos

  ldx paddle::two3::y_pos
  inx
  inx
  stx paddle::two3::y_pos

done:
  rts
.endproc

.proc check_y_col
  lda ball::y_pos
  cmp #$09 ; top of screen
  bne check_bottom

  lda ball_velo::v
  eor #$01
  sta ball_velo::v
  jmp done

check_bottom:
  cmp #240-17 ; bottom of screen
  bne done

  lda ball_velo::v
  eor #$01
  sta ball_velo::v

done:
  rts
.endproc


.proc move_ball
  lda ball_velo::h
  cmp #$01
  beq forward

  ; moving backward
  ldx ball::x_pos
  dex
  dex
  stx ball::x_pos
  jmp x_done


forward:
  ldx ball::x_pos
  inx
  inx
  stx ball::x_pos

x_done:
  lda ball_velo::v
  cmp #$01
  beq down

  ; moving up
  ldx ball::y_pos
  dex
  dex
  stx ball::y_pos
  jmp done

down:
  ldx ball::y_pos
  inx
  inx
  stx ball::y_pos

done:
  rts
.endproc


.proc update_score
  lda #$20
  sta PPU_VRAM_ADDR2
  lda #$42
  sta PPU_VRAM_ADDR2

  lda player::one::score
  jsr higher_bit
  clc
  adc #$04
  sta PPU_VRAM_IO

  lda player::one::score
  and #$0F ; lower bit
  clc
  adc #$04
  sta PPU_VRAM_IO


  lda #$20
  sta PPU_VRAM_ADDR2
  lda #$5b
  sta PPU_VRAM_ADDR2

  lda player::two::score
  jsr higher_bit
  clc
  adc #$04
  sta PPU_VRAM_IO

  lda player::two::score
  and #$0F ; lower bit
  clc
  adc #$04
  sta PPU_VRAM_IO

  lda #$20
  sta PPU_VRAM_ADDR2
  lda #$00
  sta PPU_VRAM_ADDR2

  rts
.endproc


.proc update_paddles
  rts
.endproc


.proc higher_bit
  ; get rid of lower bit
  and #%11110000

  ; now shift higher bit
  ; so it's at the lower bit
  lsr a
  lsr a
  lsr a
  lsr a
  rts
.endproc


palette:
.byt $0F,$30,$3F,$02


data:
.byt $00, $00, $00, $00, $00, $00, $00, $00
.byt $00, $00, $00, $00, $00, $00, $00, $00
.byt $01, $01, $01, $01, $01, $01, $01, $01
.byt $00, $00, $00, $00, $00, $00, $00, $00
.byt $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
.byt $00, $00, $00, $00, $00, $00, $00, $00
.byt $80, $80, $80, $80, $80, $80, $80, $80
.byt $00, $00, $00, $00, $00, $00, $00, $00
.byt $7c, $fe, $e7, $e7, $e7, $7f, $3e, $00
.byt $00, $38, $21, $21, $21, $03, $3e, $00
.byt $18, $3c, $1c, $1c, $1c, $7e, $3f, $00
.byt $00, $04, $04, $04, $04, $00, $3f, $00
.byt $7c, $fe, $7f, $7f, $fc, $fe, $7f, $00
.byt $00, $38, $61, $07, $1c, $00, $7f, $00
.byt $7e, $3f, $3c, $1e, $c7, $7f, $3e, $00
.byt $00, $37, $00, $18, $01, $03, $3e, $00
.byt $1c, $3e, $7e, $ee, $fe, $7f, $06, $00
.byt $00, $02, $12, $22, $00, $73, $06, $00
.byt $fc, $fe, $fc, $7e, $c7, $7f, $3e, $00
.byt $00, $3e, $00, $78, $01, $03, $3e, $00
.byt $7c, $fe, $fc, $fe, $e7, $7f, $3e, $00
.byt $00, $3e, $00, $38, $21, $03, $3e, $00
.byt $fe, $ff, $6f, $1e, $3c, $38, $18, $00
.byt $00, $39, $63, $06, $0c, $08, $18, $00
.byt $7c, $fe, $7f, $fe, $e7, $7f, $3e, $00
.byt $00, $38, $03, $38, $21, $03, $3e, $00
.byt $7c, $fe, $e7, $7f, $3f, $7f, $3e, $00
.byt $00, $38, $21, $01, $39, $03, $3e, $00
