#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Logical (shifted register)"
    (arm64-case* [choose-sf choose-shift choose-reg choose-imm6 choose-reg choose-reg]
      and-shifted-register
      bic-shifted-register
      orr-shifted-register
      orn-shifted-register
      eor-shifted-register
      eon-shifted-register
      ands-shifted-register
      bics-shifted-register)
))

(module+ test
  (time (run-tests tests)))
