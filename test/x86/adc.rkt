#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "ADC"
    adc-r/m32-imm8
    adc-r/m64-imm8
    adc-r/m32-r32
    adc-r/m64-r64
))

(module+ test
  (time (run-tests tests)))
