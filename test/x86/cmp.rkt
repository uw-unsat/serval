#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "CMP"
    cmp-eax-imm32
    cmp-rax-imm32
    cmp-r/m32-imm32
    cmp-r/m64-imm32
    cmp-r/m32-imm8
    cmp-r/m64-imm8
    cmp-r/m32-r32
    cmp-r/m64-r64
    cmp-r32-r/m32
    cmp-r64-r/m64
))

(module+ test
  (time (run-tests tests)))
