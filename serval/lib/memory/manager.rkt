#lang rosette

(provide (all-defined-out))

(define target-pointer-bitwidth (make-parameter 64))

(define-generics memmgr
  (memmgr-alloc! memmgr size alignment proc #:dbg dbg)
  (memmgr-load memmgr addr off size #:dbg dbg)
  (memmgr-store! memmgr addr off data size #:dbg dbg)
  (memmgr-memset! memmgr addr value len #:dbg dbg)
  (memmgr-invariants memmgr)
  (memmgr-atomic-begin memmgr)
  (memmgr-atomic-end memmgr)
  #:fallbacks [
    (define (memmgr-atomic-begin memmgr) (void))
    (define (memmgr-atomic-end memmgr) (void))
    (define (memmgr-invariants memmgr) #t)
  ])
