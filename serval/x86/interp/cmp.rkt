#lang rosette

(require
  "common.rkt")

(provide
  cmp-eax-imm32
  cmp-rax-imm32
  cmp-r/m32-imm32
  cmp-r/m64-imm32
  cmp-r/m32-imm8
  cmp-r/m64-imm8
  cmp-r/m32-r32
  cmp-r/m64-r64
  cmp-r32-r/m32
  cmp-r64-r/m64)


(define (interpret-cmp cpu src1 v2)
  (define v1 (cpu-gpr-ref cpu src1))
  (define result (bvsub v1 v2))
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-set! cpu 'CF (bool->bitvector (core:bvusub-overflow? v1 v2)))
  (cpu-flag-set! cpu 'OF (bool->bitvector (core:bvssub-overflow? v1 v2)))
  (cpu-flag-set! cpu 'AF (bool->bitvector (core:bvusub-overflow? (trunc 4 v1) (trunc 4 v2)))))

; 3D id
(define-insn cmp-eax-imm32 (imm32)
  #:decode [((byte #x3D) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #x3D) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-cmp cpu eax imm32)))

; REX.W + 3D id
(define-insn cmp-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #x3D) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #x3D) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-cmp cpu rax (sign-extend imm32 (bitvector 64)))))

; 81 /7 id
(define-insn cmp-r/m32-imm32 (src1 imm32)
  #:decode [((byte #x81) (/7 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (byte #x81) (/7 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r src1) (byte #x81) (/7 src1) (encode-imm imm32))
  (lambda (cpu src1 imm32)
    (interpret-cmp cpu src1 imm32)))

; REX.W + 81 /7 id
(define-insn cmp-r/m64-imm32 (src1 imm32)
  #:decode [((rex.w/r r b) (byte #x81) (/7 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r src1) (byte #x81) (/7 src1) (encode-imm imm32))
  (lambda (cpu src1 imm32)
    (interpret-cmp cpu src1 (sign-extend imm32 (bitvector 64)))))

; 83 /7 ib
(define-insn cmp-r/m32-imm8 (src1 imm8)
  #:decode [((byte #x83) (/7 r/m) i0)
            (list (gpr32-no-rex r/m) i0)]
           [((rex/r b) (byte #x83) (/7 r/m) i0)
            (list (gpr32 b r/m) i0)]
  #:encode (list (rex/r src1) (byte #x83) (/7 src1) (encode-imm imm8))
  (lambda (cpu src1 imm8)
    (interpret-cmp cpu src1 (sign-extend imm8 (bitvector 32)))))

; REX.W + 83 /7 ib
(define-insn cmp-r/m64-imm8 (src1 imm8)
  #:decode [((rex.w/r b) (byte #x83) (/7 r/m) i0)
            (list (gpr64 b r/m) i0)]
  #:encode (list (rex.w/r src1) (byte #x83) (/7 src1) (encode-imm imm8))
  (lambda (cpu src1 imm32)
    (interpret-cmp cpu src1 (sign-extend imm8 (bitvector 64)))))

; 39 /r
(define-insn cmp-r/m32-r32 (src1 src2)
  #:decode [((byte #x39) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x39) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src2 src1) (byte #x39) (/r src2 src1))
  (lambda (cpu src1 src2)
    (interpret-cmp cpu src1 (trunc 32 (cpu-gpr-ref cpu src2)))))

; REX.W + 39 /r
(define-insn cmp-r/m64-r64 (src1 src2)
  #:decode [((rex.w/r r b) (byte #x39) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src2 src1) (byte #x39) (/r src2 src1))
  (lambda (cpu src1 src2)
    (interpret-cmp cpu src1 (cpu-gpr-ref cpu src2))))

; 3B /r
(define-insn cmp-r32-r/m32 (src1 src2)
  #:decode [((byte #x3B) (/r reg r/m))
            (list (gpr32-no-rex reg) (gpr32-no-rex r/m))]
           [((rex/r r b) (byte #x3B) (/r reg r/m))
            (list (gpr32 r reg) (gpr32 b r/m))]
  #:encode (list (rex/r src1 src2) (byte #x3B) (/r src1 src2))
  (lambda (cpu src1 src2)
    (interpret-cmp cpu src1 (trunc 32 (cpu-gpr-ref cpu src2)))))

(define-insn cmp-r32-m32 (src1 src2)
  #:decode [((byte #x3B) (modr/m (== (bv #b00 2)) reg r/m))
            (list (gpr32-no-rex reg) (register-indirect (gpr64-no-rex r/m) #f 32))]
  #:encode (let ([es1 (register-encode src1)]
                 [es2 (register-encode src2)])
             (list (rex/r (car es1) (first es2)) (byte #x3B) (modr/m (second es2) (cdr es1) (third es2)) (fourth es2)))
  (lambda (cpu src1 src2)
    (interpret-cmp cpu src1 (cpu-gpr-ref cpu src2))))

; REX.W + 3B /r
(define-insn cmp-r64-r/m64 (src1 src2)
  #:decode [((rex.w/r r b) (byte #x3B) (/r reg r/m))
            (list (gpr64 r reg) (gpr64 b r/m))]
  #:encode (list (rex.w/r src1 src2) (byte #x3B) (/r src1 src2))
  (lambda (cpu src1 src2)
    (interpret-cmp cpu src1 (cpu-gpr-ref cpu src2))))
