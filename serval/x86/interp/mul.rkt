#lang rosette

(require
  "common.rkt")

(provide
  mul-r/m32
  mul-r/m64)


(define (interpret-mul cpu src n)
  (define v1 (trunc n (cpu-gpr-ref cpu rax)))
  (define v2 (cpu-gpr-ref cpu src))
  (define lower ((core:bvmul-proc) v1 v2))
  (define upper ((core:bvmulhu-proc) v1 v2))
  (define upper-bit (if (bvzero? upper) (bv #b0 1) (bv #b1 1)))
  (cpu-gpr-set! cpu rax (zero-extend lower (bitvector 64)))
  (cpu-gpr-set! cpu rdx (zero-extend upper (bitvector 64)))
  ; CF and OF are cleared if the higher-order bits are 0
  (cpu-flag-set! cpu 'CF upper-bit)
  (cpu-flag-set! cpu 'OF upper-bit)
  ; SF, ZF, AF, and PF are undefined
  (cpu-flag-havoc! cpu 'SF 'ZF 'AF 'PF))

; F7 /4
(define-insn mul-r/m32 (src)
  #:decode [((byte #xF7) (/4 r/m))
            (list (gpr32-no-rex r/m))]
           [((rex/r b) (byte #xF7) (/4 r/m))
            (list (gpr32 b r/m))]
  #:encode (list (rex/r src) (byte #xF7) (/4 src))
  (lambda (cpu src)
    (interpret-mul cpu src 32)))

(define-insn mul-m32 (src)
  #:decode [((byte #xF7) (modr/m (== (bv #b01 2)) (== (bv 4 3)) r/m) disp8)
            (list (register-indirect (gpr64-no-rex r/m) disp8 32))]
           [((byte #xF7) (modr/m (== (bv #b10 2)) (== (bv 4 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64-no-rex r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32))]
           [((rex/r b) (byte #xF7) (modr/m (== (bv #b01 2)) (== (bv 4 3)) r/m) disp8)
            (list (register-indirect (gpr64 b r/m) disp8 32))]
           [((rex/r b) (byte #xF7) (modr/m (== (bv #b10 2)) (== (bv 4 3)) r/m) disp32.0 disp32.1 disp32.2 disp32.3)
            (list (register-indirect (gpr64 b r/m) (decode-imm disp32.0 disp32.1 disp32.2 disp32.3) 32))]
  #:encode (let ([e (register-encode src)])
             (list (rex/r (first e)) (byte #xF7) (modr/m (second e) (bv 4 3) (third e)) (fourth e)))
  (lambda (cpu src)
    (interpret-mul cpu src 32)))

; REX.W + F7 /4
(define-insn mul-r/m64 (src)
  #:decode [((rex.w/r b) (byte #xF7) (/4 r/m))
            (list (gpr64 b r/m))]
  #:encode (list (rex.w/r src) (byte #xF7) (/4 src))
  (lambda (cpu src)
    (interpret-mul cpu src 64)))
