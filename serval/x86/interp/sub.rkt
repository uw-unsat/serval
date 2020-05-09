#lang rosette

(require
  "common.rkt")

(provide
  sub-eax-imm32
  sub-rax-imm32
  sub-r/m32-imm32
  sub-r/m64-imm32
  sub-r/m32-imm8
  sub-r/m64-imm8
  sub-r/m32-r32
  sub-r/m64-r64
  sub-r32-r/m32
  sub-r64-r/m64)


(define (interpret-sub cpu dst v2)
  (define v1 (cpu-gpr-ref cpu dst))
  (define result (bvsub v1 v2))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-set! cpu 'CF (bool->bitvector (core:bvusub-overflow? v1 v2)))
  (cpu-flag-set! cpu 'OF (bool->bitvector (core:bvssub-overflow? v1 v2)))
  (cpu-flag-set! cpu 'AF (bool->bitvector (core:bvusub-overflow? (trunc 4 v1) (trunc 4 v2)))))

; 2D id
(define-insn sub-eax-imm32 (imm32)
  #:decode [((byte #x2D) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #x2D) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-sub cpu eax imm32)))

; REX.W + 2D id
(define-insn sub-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #x2D) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #x2D) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-sub cpu rax (sign-extend imm32 (bitvector 64)))))

; 81 /5 id
(define-insn sub-r/m32-imm32 (dst imm32)
  #:decode [((byte #x81) (/5 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
          [((rex/r b) (byte #x81) (/5 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r dst) (byte #x81) (/5 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-sub cpu dst imm32)))

; REX.W + 81 /5 id
(define-insn sub-r/m64-imm32 (dst imm32)
  #:decode [((rex.w/r r b) (byte #x81) (/5 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r dst) (byte #x81) (/5 dst) (encode-imm imm32))
  (lambda (cpu dst imm32)
    (interpret-sub cpu dst (sign-extend imm32 (bitvector 64)))))

; 83 /5 ib
(define-insn sub-r/m32-imm8 (dst imm8)
  #:decode [((byte #x83) (/5 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/5 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r dst) (byte #x83) (/5 dst) (encode-imm imm8))
  (lambda (cpu dst imm8)
    (interpret-sub cpu dst (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /5 ib
(define-insn sub-r/m64-imm8 (dst imm8)
  #:decode [((rex.w/r b) (byte #x83) (/5 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r dst) (byte #x83) (/5 dst) (encode-imm imm8))
  (lambda (cpu dst imm32)
    (interpret-sub cpu dst (sign-extend imm8 (bitvector 64)))))

; 29 /r
(define-insn sub-r/m32-r32 (dst src)
  #:decode [((byte #x29) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x29) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src dst) (byte #x29) (/r src dst))
  (lambda (cpu dst src)
    (interpret-sub cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 29 /r
(define-insn sub-r/m64-r64 (dst src)
  #:decode [((rex.w/r r b) (byte #x29) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src dst) (byte #x29) (/r src dst))
  (lambda (cpu dst src)
    (interpret-sub cpu dst (cpu-gpr-ref cpu src))))

; 2B /r
(define-insn sub-r32-r/m32 (dst src)
  #:decode [((byte #x2B) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
           [((rex/r r b) (byte #x2B) (/r reg r/m))
            (list (gpr32 r reg) (gpr32 b r/m))]
  #:encode (list (rex/r dst src) (byte #x2B) (/r dst src))
  (lambda (cpu dst src)
    (interpret-sub cpu dst (cpu-gpr-ref cpu src))))

(define-insn sub-r32-m32 (dst src)
  #:decode [((byte #x2B) (modr/m (== (bv #b00 2)) reg r/m))
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) #f 32))]
  #:encode (let ([ed (register-encode dst)]
                 [es (register-encode src)])
             (list (rex/r (car ed) (first es)) (byte #x2B) (modr/m (second es) (cdr ed) (third es)) (fourth es)))
  (lambda (cpu dst src)
    (interpret-sub cpu dst (cpu-gpr-ref cpu src))))

; REX.W + 2B /r
(define-insn sub-r64-r/m64 (dst src)
  #:decode [((rex.w/r r b) (byte #x2B) (/r reg r/m))
            (list (gpr64 r reg) (gpr64 b r/m))]
  #:encode (list (rex.w/r dst src) (byte #x2B) (/r dst src))
  (lambda (cpu dst src)
    (interpret-sub cpu dst (cpu-gpr-ref cpu src))))
