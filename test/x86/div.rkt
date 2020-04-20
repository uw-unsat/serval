#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "DIV"
    div-r/m32
    div-r/m64
))

(module+ test
  (time (run-tests tests)))
