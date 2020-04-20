#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "AND"
    and-eax-imm32
    and-rax-imm32
    and-r/m32-imm32
    and-r/m64-imm32
    and-r/m32-imm8
    and-r/m64-imm8
    and-r/m32-r32
    and-r/m64-r64
    and-r32-r/m32
    and-r64-r/m64
))

(module+ test
  (time (run-tests tests)))
