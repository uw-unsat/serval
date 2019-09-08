#lang racket/base

(require racket/generic)

(provide (all-defined-out))

(define-generics engine
  (engine-ptr engine)
  (engine-reg-enum engine)
  (engine-reg-type engine reg))
