#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "BSWAP"
    bswap-r32
    bswap-r64
))

(module+ test
  (time (run-tests tests)))
