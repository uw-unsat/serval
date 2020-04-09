#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Branch (immediate)"
    (arm32-case* [choose-imm24]
      b)
))

(module+ test
  (time (run-tests tests)))
