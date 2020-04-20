#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "SBB"
    sbb-r/m32-imm8
    sbb-r/m64-imm8
    sbb-r/m32-r32
    sbb-r/m64-r64
))

(module+ test
  (time (run-tests tests)))
