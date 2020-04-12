#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Bitfield move"
    (arm64-case* [choose-sf choose-imm6 choose-imm6 choose-reg choose-reg]
      sbfm
      bfm
      ubfm)
    ; Shift (immediate), aliases of sbfm/ubfm
    (arm64-case* [choose-sf choose-imm6 choose-reg choose-reg]
      ; no ror yet
      asr
      lsl
      lsr)
    ; Sign-extend and Zero-extend, aliaes of sbfm/ubfm
    (arm64-case* [choose-sf choose-reg choose-reg]
      sxtb
      sxth)
    (arm64-case* [choose-reg choose-reg]
      sxtw
      uxtb
      uxth)
))

(module+ test
  (time (run-tests tests)))
