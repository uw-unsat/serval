#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Arithmetic (shifted register)"
    (arm64-case* [choose-sf choose-shift choose-reg choose-imm6 choose-reg choose-reg]
      add-shifted-register
      adds-shifted-register
      sub-shifted-register
      subs-shifted-register)
))

(module+ test
  (time (run-tests tests)))
