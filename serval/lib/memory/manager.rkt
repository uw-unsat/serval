#lang rosette

(require "../debug.rkt")

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
    (define (memmgr-alloc! memmgr size alignment proc #:dbg dbg)
      (bug #:msg (format "memmgr: ~v does not provide an allocator" memmgr) #:dbg dbg))
    (define (memmgr-memset! memmgr addr value len #:dbg dbg)
      (bug #:msg (format "memmgr: ~v does not implement memset" memmgr) #:dbg dbg))
    (define (memmgr-atomic-begin memmgr) (void))
    (define (memmgr-atomic-end memmgr) (void))
    (define (memmgr-invariants memmgr) #t)
  ])
