#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "SUB"
    sub-eax-imm32
    sub-rax-imm32
    sub-r/m32-imm32
    sub-r/m64-imm32
    sub-r/m32-imm8
    sub-r/m64-imm8
    sub-r/m32-r32
    sub-r/m64-r64
    sub-r32-r/m32
    sub-r64-r/m64
))

(module+ test
  (time (run-tests tests)))
