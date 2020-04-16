#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Miscellaneous"
    (arm32-case* [choose-reg/no-r15]
      blx-register)
))

(module+ test
  (time (run-tests tests)))
