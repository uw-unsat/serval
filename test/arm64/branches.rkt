#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Arithmetic (immediate)"
    ; Conditional branch
     (arm64-case* [choose-imm19 choose-cond]
       b.cond)
    ; Unconditional branch (immediate)
     (arm64-case* [choose-imm26]
       b
       bl)
))

(module+ test
  (time (run-tests tests)))
