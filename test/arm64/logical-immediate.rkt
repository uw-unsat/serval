#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Logical (immediate)"
    (arm64-case* [choose-sf choose-sf choose-imm6 choose-imm6 choose-reg choose-reg]
      and-immediate
      ands-immediate
      eor-immediate
      orr-immediate)
))

(module+ test
  (time (run-tests tests)))
