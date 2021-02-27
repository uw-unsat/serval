#lang rosette

(require "../debug.rkt")

(provide (all-defined-out))

(define target-pointer-bitwidth (make-parameter 64))

(define-generics memmgr
  (memmgr-alloc! memmgr size alignment proc)
  (memmgr-load memmgr addr off size)
  (memmgr-store! memmgr addr off data size)
  (memmgr-memset! memmgr addr value len)
  (memmgr-bitwidth memmgr)
  (memmgr-invariants memmgr)
  (memmgr-atomic-begin memmgr)
  (memmgr-atomic-end memmgr)
  #:fallbacks [
    (define (memmgr-alloc! memmgr size alignment proc)
      (bug #:msg (format "memmgr: ~v does not provide an allocator" memmgr)))
    (define (memmgr-memset! memmgr addr value len)
      (bug #:msg (format "memmgr: ~v does not implement memset" memmgr)))
    (define (memmgr-atomic-begin memmgr) (void))
    (define (memmgr-atomic-end memmgr) (void))
    (define (memmgr-invariants memmgr) #t)
  ])
