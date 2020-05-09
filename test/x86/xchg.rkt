#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "XCHG"
    xchg-r/m32-r32
    xchg-r/m64-r64
))

(module+ test
  (time (run-tests tests)))
