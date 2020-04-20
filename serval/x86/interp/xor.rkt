#lang rosette

(require
  "common.rkt")

(provide
  xor-eax-imm32
  xor-rax-imm32
  xor-r/m32-imm32
  xor-r/m64-imm32
  xor-r/m32-imm8
  xor-r/m64-imm8
  xor-r/m32-r32
  xor-r/m64-r64
  xor-r32-r/m32
  xor-r64-r/m64)


(define (interpret-xor cpu dst v2)
  (define v1 (cpu-gpr-ref cpu dst))
  (define result (bvxor v1 v2))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-clear! cpu 'CF 'OF 'AF))

; 35 id
(define-insn xor-eax-imm32 (imm32)
  #:decode [((byte #x35) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #x35) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-xor cpu eax imm32)))

; REX.W + 35 id
(define-insn xor-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #x35) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #x35) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-xor cpu rax (sign-extend imm32 (bitvector 64)))))

; 81 /6 id
(define-insn xor-r/m32-imm32 (dst imm32)
  #:decode [((byte #x81) (/6 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (byte #x81) (/6 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r dst) (byte #x81) (/6 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-xor cpu dst imm32)))

; REX.W + 81 /6 id
(define-insn xor-r/m64-imm32 (dst imm32)
  #:decode [((rex.w/r b) (byte #x81) (/6 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst) (byte #x81) (/6 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-xor cpu dst (sign-extend imm32 (bitvector 64)))))

; 83 /6 ib
(define-insn xor-r/m32-imm8 (dst imm8)
  #:decode [((byte #x83) (/6 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/6 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r dst) (byte #x83) (/6 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-xor cpu dst (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /6 ib
(define-insn xor-r/m64-imm8 (dst imm8)
  #:decode [((rex.w/r b) (byte #x83) (/6 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r dst) (byte #x83) (/6 dst) (encode-imm imm8))
  (lambda (cpu dst imm32)
    (interpret-xor cpu dst (sign-extend imm8 (bitvector 64)))))

; 31 /r
(define-insn xor-r/m32-r32 (dst src)
  #:decode [((byte #x31) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x31) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x31) (/r src dst))
  (lambda (cpu dst src)
    (interpret-xor cpu dst (trunc 32 (cpu-gpr-ref cpu src)))))

; REX.W + 31 /r
(define-insn xor-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x31) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x31) (/r src dst))
  (lambda (cpu dst src)
    (interpret-xor cpu dst (cpu-gpr-ref cpu src))))

; 33 /r
(define-insn xor-r32-r/m32 (dst src)
  #:decode [((byte #x33) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
           [((rex/r r b) (byte #x33) (/r reg r/m))
            (list (gpr32 r reg) (gpr32 b r/m))]
  #:encode (list (rex/r dst src) (byte #x33) (/r dst src))
  (lambda (cpu dst src)
    (interpret-xor cpu dst (trunc 32 (cpu-gpr-ref cpu src)))))

; REX.W + 33 /r
(define-insn xor-r64-r/m64 (dst src)
  #:decode [((rex.w/r r b) (byte #x33) (/r reg r/m))
            (list (gpr64 r reg) (gpr64 b r/m))]
  #:encode (list (rex.w/r dst src) (byte #x33) (/r dst src))
  (lambda (cpu dst src)
    (interpret-xor cpu dst (cpu-gpr-ref cpu src))))
