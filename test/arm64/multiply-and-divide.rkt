#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Multiply and divide"
    ; Multiply
    (arm64-case* [choose-sf choose-reg choose-reg choose-reg choose-reg]
      madd
      msub)
    ; Divide
    (arm64-case* [choose-sf choose-reg choose-reg choose-reg]
      udiv
      sdiv)
))

(module+ test
  (time (run-tests tests)))
