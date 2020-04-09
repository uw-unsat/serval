#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Integer Test and Compare (one register and immediate)"
    (arm32-case* [choose-reg choose-imm12]
      tst-immediate
      teq-immediate
      cmp-immediate
      cmn-immediate)
))

(module+ test
  (time (run-tests tests)))
