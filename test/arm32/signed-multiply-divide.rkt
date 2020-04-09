#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Signed multiply, Divide"
    (arm32-case* [choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15]
      sdiv
      udiv)
))

(module+ test
  (time (run-tests tests)))
