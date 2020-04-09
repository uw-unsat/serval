#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Logical Arithmetic (three register, register shift)"
    (arm32-case* [choose-reg/no-r15 choose-reg/no-r15 choose-reg/no-r15 choose-stype choose-reg/no-r15]
      orr-register-shifted-register
      orrs-register-shifted-register)
    (arm32-case* [choose-reg/no-r15 choose-reg/no-r15 choose-stype choose-reg/no-r15]
      mov-register-shifted-register
      movs-register-shifted-register)
))

(module+ test
  (time (run-tests tests)))
