#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "SHRD"
    shrd-r/m32-r32-imm8
    shrd-r/m64-r64-imm8
    shrd-r/m32-r32-cl
    shrd-r/m64-r64-cl
))

(module+ test
  (time (run-tests tests)))
