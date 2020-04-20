#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "OR"
    or-eax-imm32
    or-rax-imm32
    or-r/m32-imm32
    or-r/m64-imm32
    or-r/m32-imm8
    or-r/m64-imm8
    or-r/m32-r32
    or-r/m64-r64
))

(module+ test
  (time (run-tests tests)))
