#lang rosette

(require
  "common.rkt")

(provide
  test-eax-imm32
  test-rax-imm32
  test-r/m32-imm32
  test-r/m64-imm32
  test-r/m32-r32
  test-r/m64-r64)


(define (interpret-test cpu src1 v2)
  (define n (core:bv-size v2))
  (define v1 (trunc n (cpu-gpr-ref cpu src1)))
  (define result (bvand v1 v2))
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-clear! cpu 'CF 'OF)
  (cpu-flag-havoc! cpu 'AF))

; A9 id
(define-insn test-eax-imm32 (imm32)
  #:decode [((byte #xA9) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (byte #xA9) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-test cpu eax imm32)))

; REX.W + A9 id
(define-insn test-rax-imm32 (imm32)
  #:decode [((rex.w/r) (byte #xA9) i0 i1 i2 i3)
            (list (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r) (byte #xA9) (encode-imm imm32))
  (lambda (cpu imm32)
    (interpret-test cpu rax (sign-extend imm32 (bitvector 64)))))

; F7 /0 id
(define-insn test-r/m32-imm32 (src1 imm32)
  #:decode [((byte #xF7) (/0 r/m) i0 i1 i2 i3)
            (list (gpr32-no-rex r/m) (decode-imm i0 i1 i2 i3))]
           [((rex/r b) (byte #xF7) (/0 r/m) i0 i1 i2 i3)
            (list (gpr32 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex/r src1) (byte #xF7) (/0 src1) (encode-imm imm32))
  (lambda (cpu src1 imm32)
    (interpret-test cpu src1 imm32)))

; REX.W + F7 /0 id
(define-insn test-r/m64-imm32 (src1 imm32)
  #:decode [((rex.w/r r b) (byte #xF7) (/0 r/m) i0 i1 i2 i3)
            (list (gpr64 b r/m) (decode-imm i0 i1 i2 i3))]
  #:encode (list (rex.w/r src1) (byte #xF7) (/0 src1) (encode-imm imm32))
  (lambda (cpu src1 imm32)
    (interpret-test cpu src1 (sign-extend imm32 (bitvector 64)))))

; 85 /r
(define-insn test-r/m32-r32 (src1 src2)
  #:decode [((byte #x85) (/r reg r/m))
            (list (gpr32-no-rex r/m) (gpr32-no-rex reg))]
           [((rex/r r b) (byte #x85) (/r reg r/m))
            (list (gpr32 b r/m) (gpr32 r reg))]
  #:encode (list (rex/r src2 src1) (byte #x85) (/r src2 src1))
  (lambda (cpu src1 src2)
    (interpret-test cpu src1 (trunc 32 (cpu-gpr-ref cpu src2)))))

; REX.W + 85 /r
(define-insn test-r/m64-r64 (src1 src2)
  #:decode [((rex.w/r r b) (byte #x85) (/r reg r/m))
            (list (gpr64 b r/m) (gpr64 r reg))]
  #:encode (list (rex.w/r src2 src1) (byte #x85) (/r src2 src1))
  (lambda (cpu src1 src2)
    (interpret-test cpu src1 (cpu-gpr-ref cpu src2))))
