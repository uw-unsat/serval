#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "JMP"
    jmp-rel8
    jmp-rel32
    jmp-r/m64-no-rex
))

(module+ test
  (time (run-tests tests)))
