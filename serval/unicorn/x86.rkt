#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

(require "const/x86.rkt"
         "engine.rkt")

(define-cstruct _uc_x86_mmr ([selector _uint16] [base _uint64] [limit _uint32] [flags _uint32]))

(define-cstruct _uc_x86_msr ([rid _uint32] [value _uint64]))

(struct x86-engine (ptr mode)
  #:methods gen:engine
  [(define (engine-ptr engine)
     (x86-engine-ptr engine))
   (define (engine-reg-enum engine)
     _uc_x86_reg)
   (define (engine-reg-type engine reg)
     (case reg
      [(idtr gdtr ldtr tr) _uc_x86_mmr]
      [(msr) _uc_x86_msr]
      ; FIXME: FP/SEE registers
      ; be safe - default to 64-bit
      [else _uint64]))])
