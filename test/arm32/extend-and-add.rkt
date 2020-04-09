#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Extend and Add"
    (arm32-case* [choose-reg/no-r15 choose-rotate choose-reg/no-r15]
      sxtb16 sxtb sxth
      uxtb16 uxtb uxth)
))

(module+ test
  (time (run-tests tests)))
