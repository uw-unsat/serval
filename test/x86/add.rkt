#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "ADD"
    add-eax-imm32
    add-rax-imm32
    add-r/m32-imm32
    add-r/m64-imm32
    add-r/m32-imm8
    add-r/m64-imm8
    add-r/m32-r32
    add-r/m64-r64
))

(module+ test
  (time (run-tests tests)))
