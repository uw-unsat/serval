#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Logical Arithmetic (three register, immediate shift)"
    (arm32-case* [choose-reg choose-reg choose-imm5 choose-stype choose-reg]
      orr-register)
    (arm32-case* [choose-reg choose-imm5 choose-stype choose-reg]
      mov-register)
    (arm32-case* [choose-reg choose-reg/no-r15 choose-imm5 choose-stype choose-reg]
      orrs-register)
    (arm32-case* [choose-reg/no-r15 choose-imm5 choose-stype choose-reg]
      movs-register)
))

(module+ test
  (time (run-tests tests)))
