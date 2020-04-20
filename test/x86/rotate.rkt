#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "ROL/ROR"
    rol-r/m16-1 rol-r/m32-1 rol-r/m64-1
    ror-r/m16-1 ror-r/m32-1 ror-r/m64-1
    rol-r/m16-cl rol-r/m32-cl rol-r/m64-cl
    ror-r/m16-cl ror-r/m32-cl ror-r/m64-cl
    rol-r/m16-imm8 rol-r/m32-imm8 rol-r/m64-imm8
    ror-r/m16-imm8 ror-r/m32-imm8 ror-r/m64-imm8
))

(module+ test
  (time (run-tests tests)))
