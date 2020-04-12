#lang racket/base

(require "lib.rkt")

(define tests
  (test-suite+ "Arithmetic (immediate)"
    (arm64-case* [choose-sf choose-sh choose-imm12 choose-reg choose-reg]
      add-immediate
      adds-immediate
      sub-immediate
      subs-immediate)
))

(module+ test
  (time (run-tests tests)))
