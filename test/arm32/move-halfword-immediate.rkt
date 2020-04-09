#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Move Halfword (immediate)"
    (arm32-case* [choose-imm4 choose-reg/no-r15 choose-imm12]
      movt
      movw)
))

(module+ test
  (time (run-tests tests)))
