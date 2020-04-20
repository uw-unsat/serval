#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "XOR"
    xor-eax-imm32
    xor-rax-imm32
    xor-r/m32-imm32
    xor-r/m64-imm32
    xor-r/m32-imm8
    xor-r/m64-imm8
    xor-r/m32-r32
    xor-r/m64-r64
    xor-r32-r/m32
    xor-r64-r/m64
))

(module+ test
  (time (run-tests tests)))
