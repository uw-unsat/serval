#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Integer Data Processing (three register, immediate shift)"
    (arm32-case* [choose-reg choose-reg choose-imm5 choose-stype choose-reg]
      and-register
      eor-register
      sub-register
      rsb-register
      add-register
      adc-register
      sbc-register
      rsc-register)
    (arm32-case* [choose-reg choose-reg/no-r15 choose-imm5 choose-stype choose-reg]
      ands-register
      eors-register
      subs-register
      rsbs-register
      adds-register
      adcs-register
      sbcs-register
      rscs-register)
))

(module+ test
  (time (run-tests tests)))
