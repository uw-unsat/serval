#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

(require "const/arm.rkt"
         "engine.rkt")

(struct arm-engine (ptr mode)
  #:methods gen:engine
  [(define (engine-ptr engine)
     (arm-engine-ptr engine))
   (define (engine-reg-enum engine)
     _uc_arm_reg)
   (define (engine-reg-type engine reg)
     (case reg
      [else _uint32]))])
