#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "MOV"
    mov-r/m32-imm32
    mov-r/m32-r32
    mov-r/m64-imm32
    mov-r/m64-r64
    mov-r8-imm8
    mov-r8*-imm8
    mov-r32-imm32
    mov-r32-r/m32
    mov-r64-imm64
))

(module+ test
  (time (run-tests tests)))
