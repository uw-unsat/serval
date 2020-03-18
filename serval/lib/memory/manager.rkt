#lang rosette

(provide (all-defined-out))

(define-generics memmgr
  (memmgr-alloc! memmgr size alignment proc #:dbg dbg)
  (memmgr-load memmgr addr off size #:dbg dbg)
  (memmgr-store! memmgr addr off data size #:dbg dbg)
  (memmgr-memset! memmgr addr value len #:dbg dbg)
  (memmgr-invariants memmgr))
