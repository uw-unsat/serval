#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "MUL"
    mul-r/m32
    mul-r/m64
))

(module+ test
  (time (run-tests tests)))
