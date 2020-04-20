#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "NEG"
    neg-r/m32
    neg-r/m64))

(module+ test
  (time (run-tests tests)))
