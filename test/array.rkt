#lang rosette

(require (except-in rackunit fail)
         rackunit/text-ui
         rosette/lib/roseunit
         serval/llvm
         serval/lib/unittest
         serval/lib/core)

(require "generated/racket/test/array.globals.rkt"
         "generated/racket/test/array.map.rkt")

(require "generated/racket/test/array.ll.rkt")

(define N 4)

(define (inv)
  (define-symbolic i (bitvector 64))
  (define b0 (symbol->block 'as))
  (define b1 (symbol->block 'arr))
  (forall (list i) (=> (bvult i (bv N 64))
                       (equal? (mblock-iload b0 (list i 'y))
                               (mblock-iload b1 (list (bvurem (bvadd1 i) (bv 4 64))))))))

(define (check-array-spec)
  (parameterize ([current-machine (make-machine symbols globals)])
    (define-symbolic x (bitvector 32))
    (define pre (inv))
    (define asserted
      (with-asserts-only (@test x)))
    (define post (inv))
    ; no UB triggered
    (check-equal? (asserts) null)
    (for-each (lambda (x) (check-unsat? (verify (assert x)))) asserted)
    ; check if the invariant holds
    (check-unsat? (verify (assert (implies pre post))))))

(define array-tests
  (test-suite+
   "Tests for array.c"

   (test-case+ "check-array-spec" (check-array-spec))))

(module+ test
  (time (run-tests array-tests)))
