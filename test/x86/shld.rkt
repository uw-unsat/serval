#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "SHLD"
    shld-r/m32-r32-imm8
    shld-r/m64-r64-imm8
    shld-r/m32-r32-cl
    shld-r/m64-r64-cl
))

(module+ test
  (time (run-tests tests)))
