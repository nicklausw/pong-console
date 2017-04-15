; Nintendo Game Boy GB-Z80 program assembly using ca65

.setcpu "none"

.macro _op_require_imm val
	.if .xmatch (.left (1, {val}), {(}) && .xmatch (.right (1, {val}), {)})
		.assert 0, error, "unsupported addressing mode: (addr)"
	.endif
.endmacro

;**** Arithmetic

.macro _op_r op, mul, allown, val
	.if .xmatch ({val}, b)
		.byte 0*mul + op
	.elseif .xmatch ({val}, c)
		.byte 1*mul + op
	.elseif .xmatch ({val}, d)
		.byte 2*mul + op
	.elseif .xmatch ({val}, e)
		.byte 3*mul + op
	.elseif .xmatch ({val}, h)
		.byte 4*mul + op
	.elseif .xmatch ({val}, l)
		.byte 5*mul + op
	.elseif .xmatch ({val}, {(hl)})
		.byte 6*mul + op
	.elseif .xmatch ({val}, a)
		.byte 7*mul + op
	.elseif allown
		_op_require_imm val
		.byte allown + op
		.byte (val)
	.else
		.assert 0, error, "unsupported addressing mode"
	.endif
.endmacro

.define adc _op_r $88, 1, $46, ; val
.define sub _op_r $90, 1, $46, ; val
.define sbc _op_r $98, 1, $46, ; val
.define and _op_r $A0, 1, $46, ; val
.define xor _op_r $A8, 1, $46, ; val
.define or  _op_r $B0, 1, $46, ; val
.define cp  _op_r $B8, 1, $46, ; val

;**** LD

.macro _op_ld_r op, val
	_op_r $40 + op, 1, $06-$40, val
.endmacro

.macro _ld_rp_ind op, val
	.if .xmatch ({val}, a)
		.byte op
	.else
		.assert 0, error, "unsupported addressing mode: source must be A"
	.endif
.endmacro

.macro ld dest, src
	.if .xmatch ({dest}, a)
		.if .xmatch ({src}, {(bc)})
			.byte $0a
		.elseif .xmatch ({src}, {(de)})
			.byte $1a
		.elseif .xmatch ({src}, {(hl+)}) || .xmatch ({src}, {(hli)})
			.byte $2a
		.elseif .xmatch ({src}, {(hl-)}) || .xmatch ({src}, {(hld)})
			.byte $3a
		.elseif .xmatch ({src}, {(hl)})
			.byte $7e
		.elseif .xmatch (.left (1, {src}), {(}) && .xmatch (.right (1, {src}), {)})
			.if .xmatch ({src}, {(c)}) || .xmatch ({src}, {($ff00+c)})
				.byte $f2
			.elseif .xmatch (.left (3, {src}), {($ff00+})
				ldh dest, (<src)
			.else
				.byte $fa
				.word (src)
			.endif
		.else
			_op_ld_r $38, src 
		.endif
	.elseif .xmatch ({dest}, {(c)}) || .xmatch ({dest}, {($ff00+c)})
		.if !.xmatch ({src}, a )
			.assert 0, error, "source must be a"
		.endif
		.byte $e2
	.elseif .xmatch (.left (3, {dest}), {($ff00+})
		ldh (<dest), src
	.elseif .xmatch ({dest}, bc)
		_op_require_imm src
		.byte $01
		.word (src)
	.elseif .xmatch ({dest}, de)
		_op_require_imm src
		.byte $11
		.word (src)
	.elseif .xmatch ({dest}, hl)
		.if .xmatch (.left (2, {src}), sp+) || .xmatch (.left (2, {src}), sp-)
			.byte $f8
			.byte .right (.tcount ({src})-1, {src})
		.else
			_op_require_imm src
			.byte $21
			.word (src)
		.endif
	.elseif .xmatch ({dest}, sp)
		.if .xmatch ({src}, hl)
			.byte $f9
		.else
			_op_require_imm src
			.byte $31
			.word (src)
		.endif
	.elseif .xmatch ({dest}, pc)
		.if .xmatch ({src}, hl)
			.byte $e9
		.else
			.assert 0, error, "invalid source: must be hl"
		.endif
	.elseif .xmatch ({dest}, b)
		_op_ld_r $00, src 
	.elseif .xmatch ({dest}, c)
		_op_ld_r $08, src 
	.elseif .xmatch ({dest}, d)
		_op_ld_r $10, src 
	.elseif .xmatch ({dest}, e)
		_op_ld_r $18, src 
	.elseif .xmatch ({dest}, h)
		_op_ld_r $20, src 
	.elseif .xmatch ({dest}, l)
		_op_ld_r $28, src 
	.elseif .xmatch ({dest}, {(hl)})
		.if .xmatch ({src}, {(hl)})
			.assert 0, error, "unsupported addressing mode"
		.else
			_op_ld_r $30, src
		.endif
	.elseif .xmatch ({dest}, {(bc)})
		_ld_rp_ind $02, src
	.elseif .xmatch ({dest}, {(de)})
		_ld_rp_ind $12, src
	.elseif .xmatch ({dest}, {(hl+)}) || .xmatch ({dest}, {(hli)})
		_ld_rp_ind $22, src
	.elseif .xmatch ({dest}, {(hl-)}) || .xmatch ({dest}, {(hld)})
		_ld_rp_ind $32, src
	.elseif .xmatch (.left (1, {dest}), {(}) && .xmatch (.right (1, {dest}), {)})
		.if .xmatch ({src}, sp)
			.byte $08
			.word (dest)
		.elseif .xmatch ({src}, a)
			.byte $ea
			.word (dest)
		.else
			.assert 0, error, "unsupported addressing mode"
		.endif
	.else
		.assert 0, error, "unsupported addressing mode"
	.endif
.endmacro

.macro _op_ldh op, val
	.if .xmatch (.left (1, {val}), {(}) && .xmatch (.right (1, {val}), {)})
		.byte $00+ op
		.assert ((val) & ~$FF) = 0 || ((val) & ~$FF) = $FF00, error, "address out of range"
		.byte <(val)
	.else
		.assert 0, error, "memory must be (n)"
	.endif
.endmacro

.macro ldh dest, src
	.if .xmatch ({dest}, a)
		_op_ldh $F0, src
	.elseif !.xmatch ({src}, a)
		.assert 0, error, "source or dest must be A"
	.else
		_op_ldh $E0, dest
	.endif
.endmacro

.macro _op_rp op, oprp, val
	.if .xmatch ({val}, bc)
		.byte $00 + oprp
	.elseif .xmatch ({val}, de)
		.byte $10 + oprp
	.elseif .xmatch ({val}, hl)
		.byte $20 + oprp
	.elseif .xmatch ({val}, sp)
		.byte $30 + oprp
	.else
		op
	.endif
.endmacro

;**** INC/DEC/ADD

.macro _inc_dec op, oprp, val
	_op_rp {_op_r op + $04, $08, 0, val}, oprp, val
.endmacro

.macro inc val
	_inc_dec 0, $03, val
.endmacro

.macro dec val
	_inc_dec 1, $0B, val
.endmacro

.macro add dest, src
	.ifblank src
		_op_r $80, 1, $46, dest
	.elseif .xmatch ({dest}, hl)
		_op_rp {.assert 0, error, "unknown register pair"}, $09, src
	.elseif .xmatch ({dest}, sp)
		_op_require_imm src
		.byte $e8
		.byte (src)
	.else
		.assert 0, error, "unsupported addressing mode"
	.endif
.endmacro

;**** Control flow

.macro _op_cond op, cond
	.if .xmatch ({cond}, nz)
		.byte $00 + op
	.elseif .xmatch ({cond}, z)
		.byte $08 + op
	.elseif .xmatch ({cond}, nc)
		.byte $10 + op
	.elseif .xmatch ({cond}, c)
		.byte $18 + op
	.else
		.assert 0, error, "invalid condition"
	.endif
.endmacro

.macro ret cond
	.ifblank cond
		.byte $c9
	.else
		_op_cond $c0, cond
	.endif
.endmacro

.macro jr val, target
	.local @target, @distance, @next
	
	.ifblank target
		@target = val  ; "anchor" *-relative before we emit bytes
		.byte $18
	.else
		@target = target
		_op_cond $20, val
	.endif
	
	@distance = @target - @next
	.assert @distance >= -128 && @distance <= 127, error, "branch out of range"
	.byte <@distance
@next:
.endmacro

.macro _op_cond_abs op, uncond, val, addr
	.ifblank addr
		_op_require_imm val
		.byte uncond
		.word (val)
	.else
		_op_cond op, val
		_op_require_imm addr
		.word (addr)
	.endif
.endmacro

.macro jp cond, addr
	.if .blank ({addr}) && (.xmatch ({cond}, hl) || .xmatch ({cond}, {(hl)}))
		.byte $e9 ; ld pc, hl
	.else
		_op_cond_abs $C2, $C3, cond, addr
	.endif
.endmacro

.define call _op_cond_abs $C4, $CD, ; [cond,] addr

.macro rst val
	.if ((val) & ~$38) = 0
		.byte $C7 + ((val) & $38)
	.else
		.assert 0, error, "invalid RST address"
	.endif
.endmacro

;**** $CB

.macro _op_cb op, val
	.byte $cb
	_op_r op, 1, 0, val
.endmacro

.define rlc  _op_cb $00, ; val
.define rrc  _op_cb $08, ; val
.define rl   _op_cb $10, ; val
.define rr   _op_cb $18, ; val
.define sla  _op_cb $20, ; val
.define sra  _op_cb $28, ; val
.define swap _op_cb $30, ; val
.define srl  _op_cb $38, ; val

.macro _op_cb_bit op, bit, val
	.assert ((bit) & ~7) = 0, error, "bit must be 0-7"
	.byte $cb
	_op_r op + 8*(bit), 1, 0, val
.endmacro

.define bit _op_cb_bit $40, ; bit, val
.define res _op_cb_bit $80, ; bit, val
.define set _op_cb_bit $C0, ; bit, val

;**** Stack

.macro _op_stack op, val
	.if .xmatch ({val}, bc)
		.byte $01 + op
	.elseif .xmatch ({val}, de)
		.byte $11 + op
	.elseif .xmatch ({val}, hl)
		.byte $21 + op
	.elseif .xmatch ({val}, af)
		.byte $31 + op
	.else
		.assert 0, error, "invalid register pair"
	.endif
.endmacro

.define pop  _op_stack $C0, ; rp
.define push _op_stack $C4, ; rp

;**** Implied

.macro _op_implied op
	.byte op
.endmacro

.define nop		_op_implied $00
.define rlca	_op_implied $07
.define rrca	_op_implied $0f
.define stop	_op_implied $10
.define rla		_op_implied $17
.define rra		_op_implied $1F
.define daa		_op_implied $27
.define cpl		_op_implied $2F
.define scf		_op_implied $37
.define ccf		_op_implied $3F
.define halt	_op_implied $76
.define reti	_op_implied $D9
.define di		_op_implied $F3
.define ei		_op_implied $FB

;**** Alternate

.macro _op_ldd_ldi op, dest, src
	.if .xmatch ({dest}, a) && .xmatch ({src}, {(hl)})
		.byte $2a + op
	.elseif .xmatch ({dest}, {(hl)}) && .xmatch ({src}, a)
		.byte $22 + op
	.else
		.assert 0, error, "invalid addressing mode"
	.endif
.endmacro

.define ldi _op_ldd_ldi $00, ; dest, src
.define ldd _op_ldd_ldi $10, ; dest, src

.macro ldhl stack, val
	.if !.xmatch ({stack}, sp)
		.assert 0, error, "SP must be first operand"
	.endif
	.byte $f8, (val)
.endmacro
