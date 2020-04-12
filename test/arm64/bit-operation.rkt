#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Bit operation"
    (arm64-case* [choose-sf choose-reg choose-reg]
      rev
      rev16)
    (arm64-case* [choose-reg choose-reg]
      rev32)
))

(module+ test
  (time (run-tests tests)))
