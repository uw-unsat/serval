#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Shift (register)"
    ; test only with Rm=0 (offset) and Rn=sp for now
    (arm64-case* [choose-sp choose-option choose-sf choose-sp choose-reg]
      strb
      strh
      str32
      str64
      ldrb
      ldrh
      ldr32
      ldr64)
))

(module+ test
  (time (run-tests tests)))
