#lang rosette

(require (prefix-in core: serval/lib/core))

(core:target-pointer-bitwidth 64)

(define (check-read-write-bv64)

  (define memmgr (core:make-flat-memmgr))
  (define-symbolic* addr (bitvector 64))
  (define-symbolic* off (bitvector 64))
  (define-symbolic* data (bitvector 64))

  (core:memmgr-store! memmgr addr off data (bv 8 64))

  (define readdata (core:memmgr-load memmgr addr off (bv 8 64)))

  (core:check-unsat? (verify (assert (bveq data readdata)))))

(define flat-memory-tests
  (core:test-suite+
   "Tests for flat memory model"
    (core:test-case+ "Check r/w bv64" (check-read-write-bv64))
  ))

(module+ test
  (time (core:run-tests flat-memory-tests)))