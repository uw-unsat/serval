#lang rosette

(require "base.rkt"
         (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

; instructions

(struct adc-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(adc-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xD0 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(adc-r32-imm8 dst imm8) insn])
      (interpret-adc cpu dst (sign-extend imm8 (bitvector 32)))))])

(struct adc-r32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(adc-r32-r32 dst src) insn])
      (list '0x11 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(adc-r32-r32 dst src) insn])
      (interpret-adc cpu dst (gpr-ref cpu src))))])

(struct add-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(add-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xC0 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(add-r32-imm8 dst imm8) insn])
      (interpret-binary-op cpu dst (sign-extend imm8 (bitvector 32)) bvadd)))])

(struct add-r32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(add-r32-r32 dst src) insn])
      (list '0x01 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(add-r32-r32 dst src) insn])
      (interpret-binary-op cpu dst (gpr-ref cpu src) bvadd)))])

(struct and-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(and-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xE0 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(and-r32-imm8 dst imm8) insn])
      (interpret-logical cpu dst (sign-extend imm8 (bitvector 32)) bvand)))])


(struct and-r/m32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(and-r/m32-r32 dst src) insn])
      (list '0x21 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(and-r/m32-r32 dst src) insn])
      (interpret-logical cpu dst (gpr-ref cpu src) bvand)))])

(struct and-r32-r/m32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(and-r32-r/m32 dst src) insn])
      (list '0x23 (ModR/M '0xC0 src dst))))
  (define (instruction-run insn cpu)
    (match-let ([(and-r32-r/m32 dst src) insn])
      (interpret-logical cpu dst (gpr-ref cpu src) bvand)))])

(struct cmp-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(cmp-r32-imm8 src imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xF8 src) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(cmp-r32-imm8 src imm8) insn])
     (define src1 (gpr-ref cpu src))
     (define src2 (sign-extend imm8 (bitvector 32)))
     (flag-set-arithmetic! cpu bvsub src1 src2)))])

(struct mov-r8-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-r8-imm8 dst imm8) insn])
      (list (string->symbol (string-append "0xB" (number->string (gpr->idx dst))))
            imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(mov-r8-imm8 dst imm8) insn])
      (gpr-set! cpu dst imm8)))])

(struct mov-m32-imm32 (dst disp imm32)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-m32-imm32 dst disp imm32) insn])
      (list '0xC7 (ModOpcodeR/M '0x40 dst) disp imm32)))
  (define (instruction-run insn cpu)
    (match-let ([(mov-m32-imm32 dst disp imm32) insn])
      (define-values (block path) (resolve-mem-path cpu dst disp 4))
      (core:mblock-istore! block imm32 path)))])

(struct mov-m32-r32 (dst disp src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-m32-r32 dst disp src) insn])
      (list '0x89 (ModR/M '0x40 dst src) disp)))
  (define (instruction-run insn cpu)
    (match-let ([(mov-m32-r32 dst disp src) insn])
      (define-values (block path) (resolve-mem-path cpu dst disp 4))
      (core:mblock-istore! block (gpr-ref cpu src) path)))])

(struct mov-r32-imm32 (dst imm32)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-r32-imm32 dst imm32) insn])
      (list '0xC7 (ModOpcodeR/M '0xC0 dst) imm32)))
  (define (instruction-run insn cpu)
    (match-let ([(mov-r32-imm32 dst imm32) insn])
      (gpr-set! cpu dst imm32)))])

(struct mov-r32-m32 (dst src disp)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-r32-m32 dst src disp) insn])
      (list '0x8B (ModR/M '0x40 src dst) disp)))
  (define (instruction-run insn cpu)
    (match-let ([(mov-r32-m32 dst src disp) insn])
      (define-values (block path) (resolve-mem-path cpu src disp 4))
      (define val (core:mblock-iload block path))
      (gpr-set! cpu dst val)))])

; The two forms of mov-r32-32 instructions are encoded differently.

(struct mov-r/m32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-r/m32-r32 dst src) insn])
      (list '0x89 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(mov-r/m32-r32 dst src) insn])
      (gpr-set! cpu dst (gpr-ref cpu src))))])

(struct mov-r32-r/m32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mov-r32-r/m32 dst src) insn])
      (list '0x8B (ModR/M '0xC0 src dst))))
  (define (instruction-run insn cpu)
    (match-let ([(mov-r32-r/m32 dst src) insn])
      (gpr-set! cpu dst (gpr-ref cpu src))))])

(struct movzx-r32-r8 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(movzx-r32-r8 dst src) insn])
      (list '0x0F '0xB6 (ModR/M '0xC0 src dst))))
  (define (instruction-run insn cpu)
    (match-let ([(movzx-r32-r8 dst src) insn])
      (gpr-set! cpu dst (zero-extend (extract 7 0 (gpr-ref cpu src)) (bitvector 32)))))])

(struct jb-rel8 (rel8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(jb-rel8 rel8) insn])
      (list '0x72 rel8)))
  (define (instruction-run insn cpu)
    (match-let ([(jb-rel8 rel8) insn])
      (when (flag-ref cpu 'CF)
        (cpu-next! cpu (sign-extend rel8 (bitvector 32))))))])

(struct mul-r32 (src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mul-r32 src) insn])
      (list '0xF7 (ModOpcodeR/M '0xE0 src))))
  (define (instruction-run insn cpu)
    (match-let ([(mul-r32 src) insn])
      (interpret-mul cpu (gpr-ref cpu src))))])

(struct mul-m32 (src disp)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(mul-m32 src disp) insn])
      (list '0xF7 (ModOpcodeR/M '0x60 src) disp)))
  (define (instruction-run insn cpu)
    (match-let ([(mul-m32 src disp) insn])
      (define-values (block path) (resolve-mem-path cpu src disp 4))
      (define val (core:mblock-iload block path))
      (interpret-mul cpu val)))])

(struct neg-r32 (dst)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(neg-r32 dst) insn])
      (list '0xF7 (ModOpcodeR/M '0xD8 dst))))
  (define (instruction-run insn cpu)
    (match-let ([(neg-r32 dst) insn])
      (interpret-neg cpu dst)))])

(struct or-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(or-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xC8 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(or-r32-imm8 dst imm8) insn])
      (interpret-logical cpu dst (sign-extend imm8 (bitvector 32)) bvor)))])

(struct or-r32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(or-r32-r32 dst src) insn])
      (list '0x09 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(or-r32-r32 dst src) insn])
      (interpret-logical cpu dst (gpr-ref cpu src) bvor)))])

; mark the end of program
(struct ret ()
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (list '0xC3))
  (define (instruction-run insn cpu)
    (error 'ret))])

(struct sar-r32-cl (dst)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(sar-r32-cl dst) insn])
      (list '0xD3 (ModOpcodeR/M '0xF8 dst))))
  (define (instruction-run insn cpu)
    (match-let ([(sar-r32-cl dst) insn])
      (interpret-shift cpu dst (gpr-ref cpu 'ecx) bvashr)))])

(struct sar-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(sar-r32-imm8 dst imm8) insn])
      (list '0xC1 (ModOpcodeR/M '0xF8 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(sar-r32-imm8 dst imm8) insn])
      (interpret-shift cpu dst imm8 bvashr)))])

(struct sbb-r32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(sbb-r32-r32 dst src) insn])
      (list '0x19 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(sbb-r32-r32 dst src) insn])
      (interpret-sbb cpu dst (gpr-ref cpu src))))])

(struct sbb-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(sbb-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xD8 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(sbb-r32-imm8 dst imm8) insn])
      (interpret-sbb cpu dst (sign-extend imm8 (bitvector 32)))))])

(struct shl-r32-cl (dst)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shl-r32-cl dst) insn])
      (list '0xD3 (ModOpcodeR/M '0xE0 dst))))
  (define (instruction-run insn cpu)
    (match-let ([(shl-r32-cl dst) insn])
      (interpret-shift cpu dst (gpr-ref cpu 'ecx) bvshl)))])

(struct shl-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shl-r32-imm8 dst imm8) insn])
      (list '0xC1 (ModOpcodeR/M '0xE0 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(shl-r32-imm8 dst imm8) insn])
      (interpret-shift cpu dst imm8 bvshl)))])

(struct shr-r32-cl (dst)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shr-r32-cl dst) insn])
      (list '0xD3 (ModOpcodeR/M '0xE8 dst))))
  (define (instruction-run insn cpu)
    (match-let ([(shr-r32-cl dst) insn])
      (interpret-shift cpu dst (gpr-ref cpu 'ecx) bvlshr)))])

(struct shr-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shr-r32-imm8 dst imm8) insn])
      (list '0xC1 (ModOpcodeR/M '0xE8 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(shr-r32-imm8 dst imm8) insn])
      (interpret-shift cpu dst imm8 bvlshr)))])

(struct shld-r32-r32-cl (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shld-r32-r32-cl dst src) insn])
      (list '0x0F '0xA5 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(shld-r32-r32-cl dst src) insn])
      (interpret-shld cpu dst src (gpr-ref cpu 'ecx))))])

(struct shld-r32-r32-imm8 (dst src imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shld-r32-r32-imm8 dst src imm8) insn])
      (list '0x0F '0xA4 (ModR/M '0xC0 dst src) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(shld-r32-r32-imm8 dst src imm8) insn])
      (interpret-shld cpu dst src imm8)))])

(struct shrd-r32-r32-cl (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shrd-r32-r32-cl dst src) insn])
      (list '0x0F '0xAD (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(shrd-r32-r32-cl dst src) insn])
      (interpret-shrd cpu dst src (gpr-ref cpu 'ecx))))])

(struct shrd-r32-r32-imm8 (dst src imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(shrd-r32-r32-imm8 dst src imm8) insn])
      (list '0x0F '0xAC (ModR/M '0xC0 dst src) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(shrd-r32-r32-imm8 dst src imm8) insn])
      (interpret-shrd cpu dst src imm8)))])

(struct sub-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(sub-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xE8 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(sub-r32-imm8 dst imm8) insn])
      (interpret-binary-op cpu dst (sign-extend imm8 (bitvector 32)) bvsub)))])

(struct sub-r32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(sub-r32-r32 dst src) insn])
      (list '0x29 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(sub-r32-r32 dst src) insn])
      (interpret-binary-op cpu dst (gpr-ref cpu src) bvsub)))])

(struct xor-r32-imm8 (dst imm8)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(xor-r32-imm8 dst imm8) insn])
      (list '0x83 (ModOpcodeR/M '0xF0 dst) imm8)))
  (define (instruction-run insn cpu)
    (match-let ([(xor-r32-imm8 dst imm8) insn])
      (interpret-logical cpu dst (sign-extend imm8 (bitvector 32)) bvxor)))])

(struct xor-r/m32-r32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(xor-r/m32-r32 dst src) insn])
      (list '0x31 (ModR/M '0xC0 dst src))))
  (define (instruction-run insn cpu)
    (match-let ([(xor-r/m32-r32 dst src) insn])
      (interpret-logical cpu dst (gpr-ref cpu src) bvxor)))])

(struct xor-r32-r/m32 (dst src)
 #:transparent
 #:methods gen:instruction
 [(define (instruction-encode insn)
    (match-let ([(xor-r32-r/m32 dst src) insn])
      (list '0x33 (ModR/M '0xC0 src dst))))
  (define (instruction-run insn cpu)
    (match-let ([(xor-r32-r/m32 dst src) insn])
      (interpret-logical cpu dst (gpr-ref cpu src) bvxor)))])


; mod=0x40 (01): 1-byte displacement
; mod=0xC0 (11): register addressing mode
(define (decode code)
  (match code
    ; 83 /2 ib
    ; ADC r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xD0 dst) imm8)
     (adc-r32-imm8 dst imm8)]

    ; 11 /r
    ; ADC r/m32, r32
    [(list '0x11 (ModR/M '0xC0 dst src))
     (adc-r32-r32 dst src)]

    ; 83 /0 ib
    ; ADD r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xC0 dst) imm8)
     (add-r32-imm8 dst imm8)]

    ; 01 /r
    ; ADD r/m32, r32
    [(list '0x01 (ModR/M '0xC0 dst src))
     (add-r32-r32 dst src)]

    ; 83 /4 ib
    ; AND r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xE0 dst) imm8)
     (and-r32-imm8 dst imm8)]

    ; 21 /r
    ; AND r/m32, r32
    [(list '0x21 (ModR/M '0xC0 dst src))
     (and-r/m32-r32 dst src)]

    ; 23 /r
    ; AND r32, r/m32
    [(list '0x23 (ModR/M '0xC0 src dst))
     (and-r32-r/m32 dst src)]

    ; 83 /7 ib
    ; CMP r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xF8 src) imm8)
     (cmp-r32-imm8 src imm8)]

    ; 72 cb
    ; JB rel8
    [(list '0x72 rel8)
     (jb-rel8 rel8)]

    ; B0+ rb ib
    ; MOV r8, imm8
    [(list '0xB0 imm8)
     (mov-r8-imm8 'al imm8)]
    [(list '0xB1 imm8)
     (mov-r8-imm8 'cl imm8)]
    [(list '0xB2 imm8)
     (mov-r8-imm8 'dl imm8)]
    [(list '0xB3 imm8)
     (mov-r8-imm8 'bl imm8)]
    [(list '0xB4 imm8)
     (mov-r8-imm8 'ah imm8)]
    [(list '0xB5 imm8)
     (mov-r8-imm8 'ch imm8)]
    [(list '0xB6 imm8)
     (mov-r8-imm8 'dh imm8)]
    [(list '0xB7 imm8)
     (mov-r8-imm8 'bh imm8)]

    ; C7 /0 id
    ; MOV r/m32, imm32
    [(list '0xC7 (ModOpcodeR/M '0x40 dst) disp imm32)
     (mov-m32-imm32 dst disp imm32)]
    [(list '0xC7 (ModOpcodeR/M '0xC0 dst) imm32)
     (mov-r32-imm32 dst imm32)]

    ; 89 /r
    ; MOV r/m32,r32 (store)
    [(list '0x89 (ModR/M '0x40 dst src) disp)
     (mov-m32-r32 dst disp src)]
    [(list '0x89 (ModR/M '0xC0 dst src))
     (mov-r/m32-r32 dst src)]

    ; 8B /r
    ; MOV r32,r/m32 (load)
    [(list '0x8B (ModR/M '0xC0 src dst))
     (mov-r32-r/m32 dst src)]
    [(list '0x8B (ModR/M '0x40 src dst) disp)
     (mov-r32-m32 dst src disp)]

    ; 0F B6 /r
    ; MOVZX r32, r/m8
    [(list '0x0F '0xB6 (ModR/M '0xC0 src dst))
     (movzx-r32-r8 dst src)]

    ; F7 /4
    ; MUL r/m32
    [(list '0xF7 (ModOpcodeR/M '0xE0 src))
     (mul-r32 src)]
    [(list '0xF7 (ModOpcodeR/M '0x60 src) disp)
     (mul-m32 src disp)]

    ; F7 /3
    ; NEG r/m32
    [(list '0xF7 (ModOpcodeR/M '0xD8 dst))
     (neg-r32 dst)]

    ; 83 /1 ib
    ; OR r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xC8 dst) imm8)
     (or-r32-imm8 dst imm8)]

    ; 09 /r
    ; OR r/m32, r32
    [(list '0x09 (ModR/M '0xC0 dst src))
     (or-r32-r32 dst src)]

    ; D3 /7
    ; SAR r/m32, CL
    [(list '0xD3 (ModOpcodeR/M '0xF8 dst))
     (sar-r32-cl dst)]

    ; C1 /7 ib
    ; SAR r/m32, imm8
    [(list '0xC1 (ModOpcodeR/M '0xF8 dst) imm8)
     (sar-r32-imm8 dst imm8)]

    ; 83 /3 ib
    ; SBB r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xD8 dst) imm8)
     (sbb-r32-imm8 dst imm8)]

    ; 19 /r
    ; SBB r/m32, r32
    [(list '0x19 (ModR/M '0xC0 dst src))
     (sbb-r32-r32 dst src)]

    ; D3 /4
    ; SHL r/m32, CL
    [(list '0xD3 (ModOpcodeR/M '0xE0 dst))
     (shl-r32-cl dst)]

    ; C1 /4 ib
    ; SHL r/m32, imm8
    [(list '0xC1 (ModOpcodeR/M '0xE0 dst) imm8)
     (shl-r32-imm8 dst imm8)]

    ; 0F A5 /r
    ; SHLD r/m32, r32, CL
    [(list '0x0F '0xA5 (ModR/M '0xC0 dst src))
     (shld-r32-r32-cl dst src)]

    ; 0F A4 /r
    ; SHLD r/m32, r32, imm8
    [(list '0x0F '0xA4 (ModR/M '0xC0 dst src) imm8)
     (shld-r32-r32-imm8 dst src imm8)]

    ; C1 /5 ib
    ; SHR r/m32, imm8
    [(list '0xC1 (ModOpcodeR/M '0xE8 dst) imm8)
     (shr-r32-imm8 dst imm8)]

    ; D3 /5
    ; SHR r/m32, CL
    [(list '0xD3 (ModOpcodeR/M '0xE8 dst))
     (shr-r32-cl dst)]

    ; 0F AD /r
    ; SHRD r/m32, r32, CL
    [(list '0x0F '0xAD (ModR/M '0xC0 dst src))
     (shrd-r32-r32-cl dst src)]

    ; 0F AC /r
    ; SHRD r/m32, r32, imm8
    [(list '0x0F '0xAC (ModR/M '0xC0 dst src) imm8)
     (shrd-r32-r32-imm8 dst src imm8)]

    ; 83 /5 ib
    ; SUB r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xE8 dst) imm8)
     (sub-r32-imm8 dst imm8)]

    ; 29 /r
    ; SUB r/m32, r32
    [(list '0x29 (ModR/M '0xC0 dst src))
     (sub-r32-r32 dst src)]

    ; 83 /1 ib
    ; OR r/m32, imm8
    [(list '0x83 (ModOpcodeR/M '0xF0 dst) imm8)
     (xor-r32-imm8 dst imm8)]

    ; 31 /r
    ; XOR r/m32, r32
    [(list '0x31 (ModR/M '0xC0 dst src))
     (xor-r/m32-r32 dst src)]

    ; 33 /r
    ; XOR r32, r/m32
    [(list '0x33 (ModR/M '0xC0 src dst))
     (xor-r32-r/m32 dst src)]

;     ; 73 cb
;     ; JAE rel8
;     [(list '0x73 rel8)
;      (instruction '(JAE rel8) (list rel8) size)]

;     ; 74 cb
;     ; JE rel8
;     [(list '0x74 rel8)
;      (instruction '(JE rel8) (list rel8) size)]

;     ; 75 cb
;     ; JNE rel8
;     [(list '0x75 rel8)
;      (instruction '(JNE rel8) (list rel8) size)]

;     ; 76 cb
;     ; JBE rel8
;     [(list '0x76 rel8)
;      (instruction '(JBE rel8) (list rel8) size)]

;     ; 77 cb
;     ; JA rel8
;     [(list '0x77 rel8)
;      (instruction '(JA rel8) (list rel8) size)]

;     ; 7C cb
;     ; JL rel8
;     [(list '0x7C rel8)
;      (instruction '(JL rel8) (list rel8) size)]

;     ; 7D cb
;     ; JGE rel8
;     [(list '0x7D rel8)
;      (instruction '(JGE rel8) (list rel8) size)]

;     ; 7E cb
;     ; JLE rel8
;     [(list '0x7E rel8)
;      (instruction '(JLE rel8) (list rel8) size)]

;     ; 7F cb
;     ; JG rel8
;     [(list '0x7F rel8)
;      (instruction '(JG rel8) (list rel8) size)]
  ))


; helpers

(define (interpret-adc cpu dst v2)
  (define v1 (gpr-ref cpu dst))
  (define cf (flag-ref cpu 'CF))
  (define result (bvadd v1 v2 (if cf (bv 1 32) (bv 0 32))))
  (gpr-set! cpu dst result)
  ; SF, ZF, and PF are set according to the result
  (flag-set-result! cpu result)
  ; OF/CF are set to signed/unsigned overflow
  (flag-set! cpu 'OF (core:bvsadd-overflow? v1 v2 cf))
  (flag-set! cpu 'CF (core:bvuadd-overflow? v1 v2 cf))
  ; too lazy
  (flag-havoc! cpu 'AF))

(define (interpret-sbb cpu dst v2)
  (define v1 (gpr-ref cpu dst))
  (define cf (flag-ref cpu 'CF))
  (define result (bvsub v1 v2 (if cf (bv 1 32) (bv 0 32))))
  (gpr-set! cpu dst result)
  ; SF, ZF, and PF are set according to the result
  (flag-set-result! cpu result)
  ; OF/CF are set to signed/unsigned overflow
  (flag-set! cpu 'OF (core:bvssub-overflow? v1 v2 cf))
  (flag-set! cpu 'CF (core:bvusub-overflow? v1 v2 cf))
  ; too lazy
  (flag-havoc! cpu 'AF))

(define (flag-set-arithmetic! cpu proc v1 v2)
  ; SF, ZF, and PF are set according to the result
  (flag-set-result! cpu (proc v1 v2))
  ; OF/CF are set to signed/unsigned overflow
  (define-values (signed-overflow? unsigned-overflow?)
    (cond
      [(equal? proc bvadd)
       (values core:bvsadd-overflow? core:bvuadd-overflow?)]
      [(equal? proc bvsub)
       (values core:bvssub-overflow? core:bvusub-overflow?)]
      [(equal? proc bvmul)
       (values core:bvsmul-overflow? core:bvumul-overflow?)]))
  (flag-set! cpu 'OF (signed-overflow? v1 v2))
  (flag-set! cpu 'CF (unsigned-overflow? v1 v2))
  ; too lazy
  (flag-havoc! cpu 'AF))

(define (interpret-neg cpu dst)
  (define val (gpr-ref cpu dst))
  (gpr-set! cpu dst (bvneg val))
  (flag-set-arithmetic! cpu bvsub (bv 0 32) val))

(define (interpret-binary-op cpu dst v2 proc)
  (define v1 (gpr-ref cpu dst))
  (gpr-set! cpu dst (proc v1 v2))
  (flag-set-arithmetic! cpu proc v1 v2))

(define (interpret-logical cpu dst val proc)
  (define result (proc (gpr-ref cpu dst) val))
  (gpr-set! cpu dst result)
  ; SF, ZF, and PF are set according to the result
  (flag-set-result! cpu result)
  ; OF and CF are cleared
  (flag-clear! cpu 'CF)
  (flag-clear! cpu 'OF)
  ; AF is undefined
  (flag-havoc! cpu 'AF))

(define (interpret-mul cpu v2)
  (define v1 (gpr-ref cpu 'eax))
  (define n (core:bv-size v1))
  (define 2n (+ n n))
  (define upper
    (extract (sub1 2n) n
             (bvmul (zero-extend v1 (bitvector 2n))
                    (zero-extend v2 (bitvector 2n)))))
  (gpr-set! cpu 'eax (bvmul v1 v2))
  (gpr-set! cpu 'edx upper)
  ; OF/CF are set to 0 if upper is 0
  (flag-set! cpu 'OF (! (core:bvzero? upper)))
  (flag-set! cpu 'CF (! (core:bvzero? upper)))
  ; SF, ZF, AF, PF are undefined
  (flag-havoc! cpu 'SF)
  (flag-havoc! cpu 'ZF)
  (flag-havoc! cpu 'AF)
  (flag-havoc! cpu 'PF))

; masked to 5 bits
(define (shift-count x)
  (zero-extend (extract 4 0 x) (bitvector 32)))

(define (flag-set-shift! cpu count result)
  ; flags are unaffected if count is 0
  (when (! (core:bvzero? count))
    ; SF, ZF, and PF are set according to the result
    (flag-set-result! cpu result)
    ; too lazy to model CF/OF for shifts
    (flag-havoc! cpu 'CF)
    (flag-havoc! cpu 'OF)
    ; AF is undefined
    (flag-havoc! cpu 'AF)))

(define (interpret-shift cpu dst count proc)
  (set! count (shift-count count))
  (define result (proc (gpr-ref cpu dst) count))
  (gpr-set! cpu dst result)
  (flag-set-shift! cpu count result))

; Shift dst to left count bits while shifting bits from src in from the right.
(define (interpret-shld cpu dst src count)
  (set! count (shift-count count))
  ; When count is 0, bvlshr by 32 bits shifts all bits out (unlike x86).
  (define result (bvor (bvshl (gpr-ref cpu dst) count)
                       (bvlshr (gpr-ref cpu src) (bvsub (bv 32 32) count))))
  (gpr-set! cpu dst result)
  (flag-set-shift! cpu count result))

; Shift dst to right count bits while shifting bits from src in from the left.
(define (interpret-shrd cpu dst src count)
  (set! count (shift-count count))
  ; When count is 0, bvshl by 32 bits shifts all bits out (unlike x86).
  (define result (bvor (bvlshr (gpr-ref cpu dst) count)
                       (bvshl (gpr-ref cpu src) (bvsub (bv 32 32) count))))
  (gpr-set! cpu dst result)
  (flag-set-shift! cpu count result))

(define (resolve-mem-path cpu reg disp size)
  (when (integer? size)
    (set! size (bv size 32)))

  (define base32 (gpr-ref cpu reg))
  (define disp32 (sign-extend disp (bitvector 32)))

  (define mr (core:guess-mregion-from-addr #:dbg current-pc-debug (cpu-mregions cpu) base32 disp32))
  (define addr (bvadd base32 disp32))
  (core:bug-on (! (core:mregion-inbounds? mr addr size))
   #:dbg current-pc-debug
   #:msg (format "resolve-mem-path: address out of range:\n addr: ~e\n block: ~e" addr (core:mregion-name mr)))

  (define block (core:mregion-block mr))
  (define offset (bvsub addr (bv (core:mregion-start mr) 32)))
  (define path (core:mblock-path block offset size #:dbg current-pc-debug))
  (values block path))


(define (interpret-instr cpu insn)
  (instruction-run insn cpu)
  ; bump the pc
  (cpu-next! cpu (bv (instruction-size insn) 32)))

(define (interpret-program cpu program)
  (define instructions (program-instructions program))
  (for/all ([pc (cpu-pc cpu) #:exhaustive])
    (begin
      (set-current-pc-debug! pc)
      (set-cpu-pc! cpu pc)
      (cond
        [(hash-has-key? instructions pc)
         (define insn (hash-ref instructions pc))
         (cond
           ; no function call for now
           [(ret? insn) (void)]
           [else
             (interpret-instr cpu insn)
             (interpret-program cpu program)])]
        [else
         (core:bug-on #t
          #:dbg current-pc-debug
          #:msg (format "No instruction at pc ~e\n" pc))]))))
