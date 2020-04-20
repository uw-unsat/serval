#lang rosette

(require
  "common.rkt")

(provide
  or-eax-imm32
  or-rax-imm32
  or-r/m32-imm32
  or-r/m64-imm32
  or-r/m32-imm8
  or-r/m64-imm8
  or-r/m32-r32
  or-r/m64-r64)


(define (interpret-or cpu dst v2)
  (define v1 (cpu-gpr-ref cpu dst))
  (define result (bvor v1 v2))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-clear! cpu 'CF 'OF 'AF))

; 0D id
(define-insn or-eax-imm32 (imm32)
  #:decode [((byte #x0D) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #x0D) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-or cpu eax imm32)))

; REX.W + 0D id
(define-insn or-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #x0D) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #x0D) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-or cpu rax (sign-extend imm32 (bitvector 64)))))

; 81 /1 id
(define-insn or-r/m32-imm32 (dst imm32)
  #:decode [((byte #x81) (/1 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (byte #x81) (/1 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r dst) (byte #x81) (/1 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-or cpu dst imm32)))

; REX.W + 81 /1 id
(define-insn or-r/m64-imm32 (dst imm32)
  #:decode [((rex.w/r b) (byte #x81) (/1 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst) (byte #x81) (/1 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-or cpu dst (sign-extend imm32 (bitvector 64)))))

; 83 /1 ib
(define-insn or-r/m32-imm8 (dst imm8)
  #:decode [((byte #x83) (/1 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/1 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r dst) (byte #x83) (/1 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-or cpu dst (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /1 ib
(define-insn or-r/m64-imm8 (dst imm8)
  #:decode [((rex.w/r b) (byte #x83) (/1 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r dst) (byte #x83) (/1 dst) (encode-imm imm8))
  (lambda (cpu dst imm32)
    (interpret-or cpu dst (sign-extend imm8 (bitvector 64)))))

; 09 /r
(define-insn or-r/m32-r32 (dst src)
  #:decode [((byte #x09) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x09) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x09) (/r src dst))
  (lambda (cpu dst src)
    (interpret-or cpu dst (trunc 32 (cpu-gpr-ref cpu src)))))

; REX.W + 09 /r
(define-insn or-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x09) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x09) (/r src dst))
  (lambda (cpu dst src)
    (interpret-or cpu dst (cpu-gpr-ref cpu src))))
