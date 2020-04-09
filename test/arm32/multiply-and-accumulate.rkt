#lang racket/base

(require
  "lib.rkt")

; avoid RdHi == RdLo
(define (fixup-umuls/umulls ctor)
  (lambda (RdHi RdLo Rm Rn)
    (when (equal? RdHi RdLo)
      (define r0 (arm32:integer->gpr 0))
      (define r1 (arm32:integer->gpr 1))
      (set! RdLo (if (equal? RdLo r0) r1 r0)))
    (ctor RdHi RdLo Rm Rn)))

(define tests
  (test-suite+ "Multiply and Accumulate"
    (arm32-case* [choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15]
      mul
      muls)
    (arm32-case* [choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15]
      mls)
    (arm32-case* [choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15]
      (fixup-umuls/umulls arm32:umull)
      (fixup-umuls/umulls arm32:umulls))
))

(module+ test
  (time (run-tests tests)))
