#lang rosette

(require
  "common.rkt")

(provide
  neg-r/m32
  neg-r/m64)


(define (interpret-neg cpu dst)
  (define v (cpu-gpr-ref cpu dst))
  (define n (core:bv-size v))
  (define result (bvneg v))
  (cpu-gpr-set! cpu dst result)
  (cpu-pf+zf+sf-set! cpu result)
  (cpu-flag-set! cpu 'CF (bool->bitvector (core:bvusub-overflow? (bv 0 n) v)))
  (cpu-flag-set! cpu 'OF (bool->bitvector (core:bvssub-overflow? (bv 0 n) v)))
  (cpu-flag-set! cpu 'AF (bool->bitvector (core:bvusub-overflow? (bv 0 4) (trunc 4 v)))))

; F7 /3
(define-insn neg-r/m32 (dst)
  #:decode [((byte #xF7) (/3 r/m))
            (list (gpr32-no-rex r/m))]
           [((rex/r b) (byte #xF7) (/3 r/m))
            (list (gpr32 b r/m))]
  #:encode (list (rex/r dst) (byte #xF7) (/3 dst))
  (lambda (cpu dst)
    (interpret-neg cpu dst)))

; REX.W + F7 /3
(define-insn neg-r/m64 (dst)
  #:decode [((rex.w/r b) (byte #xF7) (/3 r/m))
            (list (gpr64 b r/m))]
  #:encode (list (rex.w/r dst) (byte #xF7) (/3 dst))
  (lambda (cpu dst)
    (interpret-neg cpu dst)))
