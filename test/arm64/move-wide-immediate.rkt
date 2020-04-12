#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Move (wide immediate)"
    (arm64-case* [choose-sf choose-hw choose-imm16 choose-reg]
      movn
      movz
      movk)
))

(module+ test
  (time (run-tests tests)))
