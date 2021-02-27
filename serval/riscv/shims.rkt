#lang rosette

(require "base.rkt"
         "../lib/memmgr.rkt"
         (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

; void *memset(void *p, int c, size_t len)
(define (memset-shim cpu)
  (define ptr (gpr-ref cpu 'a0))
  (define c (extract 7 0 (gpr-ref cpu 'a1)))
  (define len (gpr-ref cpu 'a2))

  ; Use memmgr to implement memset.
  (define memmgr (cpu-memmgr cpu))
  (memmgr-memset! memmgr ptr c len)

  ; Fake a 'ret' instruction to return control to calling function.
  (set-cpu-pc! cpu (gpr-ref cpu 'x1))
  (havoc-caller-saved! cpu)
  (gpr-set! cpu 'a0 ptr))
