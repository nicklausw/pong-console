; Z80/SMS aPLib decompression library
; version 1.2
; 1/12/2008
; Maxim (http://www.smspower.org/maxim)
; based on code by Dan Weiss (Dwedit) - see readme.txt
;
; Usage:
;
; .include this file in your code
; somewhere after that, an aPLibMemoryStruct called aPLibMemory must be defined
; somewhere in RAM, ie. "aPLibMemory instanceof aPLibMemoryStruct" inside a
; .ramsection or .enum
;
; Then either do
;
; ld hl,<source address>
; ld de,<destination address>
; call depack
;
; to decompress from ROM to RAM, or do
;
; ld hl,<source address>
; ld de,<destination address>
; di
; call vram_depack
; ei
;
; to decompress from ROM to VRAM (on Sega 8-bit systems). You don't need the 
; di/ei if you don't have interrupts changing the VRAM address.
;
; ROM usage:
;
; Shared:     41 bytes            Using depack only:      233 bytes
; RAM-only:  192 bytes     ->     Using vram_depack only: 294 bytes
; VRAM-only: 253 bytes            Using both depackers:   486 bytes
;
; RAM usage:
;
; aPLibMemoryStruct uses 5 bytes, and can be re-used after decompression. The
; amount of stack space it uses will depend on the data, but in my tests it
; never used more than 12 bytes.
;
; CPU usage:
;
; Speed depends on the data being decompressed. When decompressing from ROM to
; VRAM, better compressed data will tend to decompress slower; in a simple test,
; decompression was around 10KB per second. Decompression to RAM has not been
; benchmarked.
;
; Assembler dialect:
;
; This file is using WLA-DX syntax quite heavily. It may be possible to convert
; it to other assemblers which have compatible conditional directives.
; comment out this line to suppress the block size notifications
; (useful for optimising to see size changes)

.struct aPLibMemoryStruct
bits     db
byte     db ; not directly referenced, assumed to come after bits
LWM      db
R0       dw
.endst

; Reader's note:
;
; The code has been split into three sections:
; 1. Subroutines which are only referenced by calls, and which do no reading to
;    or writing from the destination, are arranged first. This allows better
;    code packing in the output (more finely divided blobs), and these
;    subroutines are common to both unpackers.
; 2. The original memory-to-memory (usually ROM-to-RAM) depacker follows. Its
;    structure has been arranged such that the entry point is in the middle -
;    this is so it can use jr to branch out to the various subsections to save
;    a few bytes, but it makes it somewhat harder to read. "depack" is the
;    entry point and "_aploop" is the main loop.
; 3. Part 2 was then copied and pasted and all parts reading from or writing to
;    the destination were replaced such that they correctly address and access
;    VRAM via ports $BE and $BF. Replacement functions were written for OUTI
;    and OTIR to perform the same tasks.
;    Thus, this part may have more potential for optimisation, since it's a
;    fairly "dumb" conversion.
; More optimisations may be possible; in general, size optimisations are
; favoured over speed.

.section "aPLib ap_getbit" free
.ifdef calcblocks
.block "aPLibPart1"
.endif
ap_getbit:
  push bc
    ld bc,(aPLibMemory.bits)
    rrc c
    jr nc,+
    ld b,(hl)
    inc hl
+:  ld a,c
    and b
    ld (aPLibMemory.bits),bc
  pop bc
  ret
.ifdef calcblocks
.endb
.endif
.ends

.section "aPLib ap_getbitbc" free
.ifdef calcblocks
.block "aPLibPart2"
.endif
ap_getbitbc: ;doubles BC and adds the read bit
  sla c
  rl b
  call ap_getbit
  ret z
  inc bc
  ret
.ifdef calcblocks
.endb
.endif
.ends

.section "aPLib ap_getgamma" free
.ifdef calcblocks
.block "aPLibPart3"
.endif
ap_getgamma:
  ld bc,1
-:call ap_getbitbc
  call ap_getbit
  jr nz,-
  ret
.ifdef calcblocks
.endb
.endif
.ends


.section "aPLib VRAM extra stuff" free
.ifdef calcblocks
.block "aPLibVRAMPart5"
.endif
ap_VRAMToDE_write:
  push af
    ld a,e
    out ($bf),a
    ld a,d
    or $40
-:  out ($bf),a
  pop af
  ret
ap_VRAMToHL_read:
  push af
    ld a,l
    out ($bf),a
    ld a,h
    jr - ; space optimisation
ap_VRAM_ldi_src_to_dest:
  call ap_VRAMToDE_write
  push bc
    ld c,$be
    outi
  pop bc
  dec bc
  inc de
  ret
ap_VRAM_ldir_dest_to_dest:
  ; This may be a major speed bottleneck
  ; possibly could take some stack space for a buffer? but that'd need a lot more code space
  ; if it uses overlapping source/dest then a buffer will break it
  push af
-:  call ap_VRAMToHL_read
    in a,($be)
    call ap_VRAMToDE_write
    out ($be),a
    dec bc
    inc de
    inc hl
    ld a,b
    or c
    jr nz,-
  pop af
  ret
.ifdef calcblocks
.endb
.endif
.ends

.section "VRAM aPLib main section" free
.ifdef calcblocks
.block "aPLibVRAMPart6"
.endif
_vram_apbranch2:
  ;use a gamma code * 256 for offset, another gamma code for length
  call ap_getgamma
  dec bc
  dec bc
  ld a,(aPLibMemory.LWM)
  or a
  jr nz,_vram_ap_not_LWM
  ;bc = 2? ; Maxim: I think he means 0
  ld a,b
  or c
  jr nz,_vram_ap_not_zero_gamma
  ;if gamma code is 2, use old R0 offset, and a new gamma code for length
  call ap_getgamma
  push hl
    ld h,d
    ld l,e
    push bc
      ld bc,(aPLibMemory.R0)
      sbc hl,bc
    pop bc
    call ap_VRAM_ldir_dest_to_dest
  pop hl
  jr _vram_ap_finishup

_vram_ap_not_zero_gamma:
  dec bc
_vram_ap_not_LWM:
  ;do I even need this code? ; Maxim: seems so, it's broken without it
  ;bc=bc*256+(hl), lazy 16bit way
  ld b,c
  ld c,(hl)
  inc hl
  ld (aPLibMemory.R0),bc
  push bc
    call ap_getgamma
    ex (sp),hl
    ;bc = len, hl=offs
    push de
      ex de,hl
      ;some comparison junk for some reason
      ; Maxim: optimised to use add instead of sbc
      ld hl,-32000
      add hl,de
      jr nc,+
      inc bc
+:    ld hl,-1280
      add hl,de
      jr nc,+
      inc bc
+:    ld hl,-128
      add hl,de
      jr c,+
      inc bc
      inc bc
+:    ;bc = len, de = offs, hl=junk
    pop hl
    push hl
      or a
      sbc hl,de
    pop de
    ;hl=dest-offs, bc=len, de = dest
    call ap_VRAM_ldir_dest_to_dest
  pop hl
_vram_ap_finishup:
  ld a,1
  ld (aPLibMemory.LWM),a
  jr _vram_aploop
_vram_apbranch1: ; Maxim: moved this one closer to where it's jumped from to allow jr to work and save 2 bytes
  call ap_VRAM_ldi_src_to_dest
  xor a
  ld (aPLibMemory.LWM),a
  jr _vram_aploop

vram_depack:
  ;hl = source
  ;de = dest (in VRAM)
  ;VRAM addresses are assumed to be stable (ie. di/ei around it)
  call ap_VRAM_ldi_src_to_dest ; first byte is always uncompressed
  xor a
  ld (aPLibMemory.LWM),a
  inc a
  ld (aPLibMemory.bits),a

_vram_aploop:
  call ap_getbit
  jr z, _vram_apbranch1
  call ap_getbit
  jr z, _vram_apbranch2
  call ap_getbit
  jr z, _vram_apbranch3
  ;LWM = 0
  xor a
  ld (aPLibMemory.LWM),a
  ;get an offset
  ld bc,0
  call ap_getbitbc
  call ap_getbitbc
  call ap_getbitbc
  call ap_getbitbc
  ld a,b
  or c
  jr nz,_vram_apbranch4
;  xor a  ;write a 0 ; Maxim: a is zero already (just failed nz test), optimise this line away
_WriteAToVRAMAndLoop:
  call ap_VRAMToDE_write
  out ($be),a
  inc de
  jr _vram_aploop
_vram_apbranch4:
  ex de,hl ;write a previous bit (1-15 away from dest)
  push hl
    sbc hl,bc
    call ap_VRAMToHL_read
    in a,($be)
  pop hl
  ex de,hl
  jr _WriteAToVRAMAndLoop
_vram_apbranch3:
  ;use 7 bit offset, length = 2 or 3
  ;if a zero is encountered here, it's EOF
  ld c,(hl)
  inc hl
  rr c
  ret z
  ld b,2
  jr nc,+
  inc b
+:;LWM = 1
  ld a,1
  ld (aPLibMemory.LWM),a

  push hl
    ld a,b
    ld b,0
    ;R0 = c
    ld (aPLibMemory.R0),bc
    ld h,d
    ld l,e
    or a
    sbc hl,bc
    ld c,a
    call ap_VRAM_ldir_dest_to_dest
  pop hl
  jr _vram_aploop
.ifdef calcblocks
.endb
.endif
.ends
