#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "MOVZX"
    movzx-r32-r/m8
    movzx-r64-r/m8
    movzx-r32-r/m16
    movzx-r64-r/m16
))

(module+ test
  (time (run-tests tests)))
