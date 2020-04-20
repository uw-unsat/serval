#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "TEST"
    test-eax-imm32
    test-rax-imm32
    test-r/m32-imm32
    test-r/m64-imm32
    test-r/m32-r32
    test-r/m64-r64
))

(module+ test
  (time (run-tests tests)))
