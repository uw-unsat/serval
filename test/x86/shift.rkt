#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "SAR/SHL/SHR"
    sar-r/m32-1 sar-r/m64-1
    shl-r/m32-1 shl-r/m64-1
    shr-r/m32-1 shr-r/m64-1
    sar-r/m32-cl sar-r/m64-cl
    shl-r/m32-cl shl-r/m64-cl
    shr-r/m32-cl shr-r/m64-cl
    sar-r/m32-imm8 sar-r/m64-imm8
    shl-r/m32-imm8 shl-r/m64-imm8
    shr-r/m32-imm8 shr-r/m64-imm8
))

(module+ test
  (time (run-tests tests)))
