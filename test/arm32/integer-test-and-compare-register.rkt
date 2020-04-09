#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Integer Test and Compare (two register, immediate shift)"
    (arm32-case* [choose-reg choose-imm5 choose-stype choose-reg]
      tst-register
      teq-register
      cmp-register
      cmn-register)
))

(module+ test
  (time (run-tests tests)))
