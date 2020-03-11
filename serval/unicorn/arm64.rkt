#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

(require "const/arm64.rkt"
         "engine.rkt")

(struct arm64-engine (ptr mode)
  #:methods gen:engine
  [(define (engine-ptr engine)
     (arm64-engine-ptr engine))
   (define (engine-reg-enum engine)
     _uc_arm64_reg)
   (define (engine-reg-type engine reg)
     (case reg
      ; FIXME: VFP/Neon registers
      [else _uint64]))])
