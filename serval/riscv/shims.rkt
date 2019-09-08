#lang rosette

(require "base.rkt"
         (prefix-in core: "../lib/core.rkt"))

(provide (all-defined-out))

; void *memset(void *p, int c, size_t len)
(define (memset-shim cpu)
  (define ptr (gpr-ref cpu 'a0))
  (define c (extract 7 0 (gpr-ref cpu 'a1)))
  (define len (gpr-ref cpu 'a2))

  (define mregion (core:guess-mregion-from-addr (cpu-mregions cpu) ptr (bv 0 12) #:dbg current-pc-debug))
  (core:bug-on (eq? mregion #f) #:msg "memset-shim: failed to guess mregion" #:dbg current-pc-debug)

  (core:bug-on (not (core:mregion-inbounds? mregion ptr len))
    #:dbg current-pc-debug #:msg "memset-shim: address out of range")

  (define offset (bvsub ptr (bv (core:mregion-start mregion) (XLEN))))

  (when (not (bveq len (bv 0 (XLEN))))
    (core:mblock-memset! (core:mregion-block mregion) c offset len #:dbg current-pc-debug))

  ; Fake a 'ret' instruction to return control to calling function
  (set-cpu-pc! cpu (gpr-ref cpu 'x1))
  (havoc-caller-saved! cpu)
  (gpr-set! cpu 'a0 ptr))
