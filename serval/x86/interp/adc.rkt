#lang rosette

(require
  "common.rkt")

(provide
  adc-r/m32-imm8
  adc-r/m64-imm8
  adc-r/m32-r32
  adc-r/m64-r64)


(define (interpret-adc cpu dst v2)
  (define n (core:bv-size v2))
  (define v1 (cpu-gpr-ref cpu dst))
  (define carry (bitvector->bool (cpu-flag-ref cpu 'CF)))
  (define result (bvadd v1 v2 (bool->bitvector carry n)))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-set! cpu 'CF (bool->bitvector (core:bvuadd-overflow? v1 v2 carry)))
  (cpu-flag-set! cpu 'OF (bool->bitvector (core:bvsadd-overflow? v1 v2 carry)))
  (cpu-flag-set! cpu 'AF (bool->bitvector (core:bvuadd-overflow? (trunc 4 v1) (trunc 4 v2) carry))))

; 83 /2 ib
(define-insn adc-r/m32-imm8 (dst imm8)
  #:decode [((byte #x83) (/2 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/2 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r dst) (byte #x83) (/2 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-adc cpu dst (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /2 ib
(define-insn adc-r/m64-imm8 (dst imm8)
  #:decode [((rex.w/r b) (byte #x83) (/2 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r dst) (byte #x83) (/2 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-adc cpu dst (sign-extend imm8 (bitvector 64)))))

; 11 /r
(define-insn adc-r/m32-r32 (dst src)
  #:decode [((byte #x11) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x11) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x11) (/r src dst))
  (lambda (cpu dst src)
    (interpret-adc cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 11 /r
(define-insn adc-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x11) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x11) (/r src dst))
  (lambda (cpu dst src)
    (interpret-adc cpu dst (cpu-gpr-ref cpu src))))
