#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Shift (register)"
    (arm64-case* [choose-sf choose-reg choose-reg choose-reg]
      lslv
      lsrv
      asrv
      rorv)
))

(module+ test
  (time (run-tests tests)))
