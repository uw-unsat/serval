#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Logical Arithmetic (two register and immediate)"
    (arm32-case* [choose-reg choose-reg choose-imm12]
      orr-immediate)
    (arm32-case* [choose-reg choose-reg/no-r15 choose-imm12]
      orrs-immediate)
    (arm32-case* [choose-reg choose-imm12]
      mov-immediate)
    (arm32-case* [choose-reg/no-r15 choose-imm12]
      movs-immediate)
))

(module+ test
  (time (run-tests tests)))
