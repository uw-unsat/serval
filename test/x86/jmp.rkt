#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "JMP"
    jmp-rel8
    jmp-rel32
))

(module+ test
  (time (run-tests tests)))
