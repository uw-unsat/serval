#lang racket/base

(require
  "lib.rkt")

(define tests
  (test-suite+ "Integer Data Processing (two register and immediate)"
    (arm32-case* [choose-reg choose-reg choose-imm12]
      and-immediate
      eor-immediate
      sub-immediate
      rsb-immediate
      add-immediate
      adc-immediate
      sbc-immediate
      rsc-immediate)
    (arm32-case* [choose-reg choose-reg/no-r15 choose-imm12]
      ands-immediate
      eors-immediate
      subs-immediate
      rsbs-immediate
      adds-immediate
      adcs-immediate
      sbcs-immediate
      rscs-immediate)
))

(module+ test
  (time (run-tests tests)))
