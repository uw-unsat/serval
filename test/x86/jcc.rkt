#lang racket/base

(require
  "lib.rkt")

(define tests
  (x86-suite "Jcc"
    ja-rel8 jae-rel8
    jb-rel8 jbe-rel8
    je-rel8
    jg-rel8 jge-rel8
    jl-rel8 jle-rel8
    jne-rel8
    ja-rel32 jae-rel32
    jb-rel32 jbe-rel32
    je-rel32
    jg-rel32 jge-rel32
    jl-rel32 jle-rel32
    jne-rel32
))

(module+ test
  (time (run-tests tests)))
