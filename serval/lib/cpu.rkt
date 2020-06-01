#lang rosette

(require "memmgr.rkt")

(provide (except-out (all-defined-out) __gen-cpu-memmgr))

(define (__gen-cpu-memmgr gen-cpu)
  (gen-cpu-memmgr gen-cpu))

(define-generics gen-cpu
  (gen-cpu-memmgr gen-cpu)
  (gen-cpu-bitwidth gen-cpu)
  (gen-cpu-pc gen-cpu)
  #:fallbacks [
    ; By default, get bitwidth from underlying memmgr
    (define (gen-cpu-bitwidth gen-cpu)
      (memmgr-bitwidth (__gen-cpu-memmgr gen-cpu)))
  ])