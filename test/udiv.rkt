#lang rosette

(require (except-in rackunit fail)
         rackunit/text-ui
         rosette/lib/roseunit
         (prefix-in llvm: serval/llvm)
         serval/lib/unittest
         serval/lib/core)

(require "generated/racket/test/udiv.ll.rkt")

(define-symbolic x y i32)

(define (check-udiv x y #:result [result (/ x y)])
  (check-equal? (@udiv (bv x i32) (bv y i32)) (bv result i32)))

(define (check-udiv-concrete)
  (check-udiv 1 0 #:result 0)
  (check-udiv 42 2)
  (check-true (vc-true? (vc))))

(define (spec-udiv x y)
  (if (bvzero? y) (bv 0 i32) (bvudiv x y)))

(define (check-udiv-symbolic)
  (check-equal? (@udiv x (bv 0 i32)) (bv 0 i32))
  (check-equal? (@udiv x (bv 1 i32)) x)
  (check-equal? (@udiv (bv 0 i32) y) (bv 0 i32))
  (check-true (vc-true? (vc))))

(define (check-udiv-spec)
  ; bogus spec
  (define bogus-cond (equal? (@udiv x y) (bvudiv x y)))
  (define sol (verify (assert bogus-cond)))
  (check-sat sol)
  (check-equal? (evaluate y sol) (bv 0 32))
  ; llvm udiv (assumes y != 0)
  (define r (with-vc (equal? (@udiv x y) (llvm:udiv x y))))
  (define v (result-state r))
  (assert (=> (vc-asserts v) (result-value r)))
  ; verify
  (assert (equal? (@udiv x y) (spec-udiv x y))))

(define (check-udiv-buggy)
  (@udiv_buggy x y))

(define udiv-tests
  (test-suite+
   "Tests for udiv.c"

   (parameterize ([llvm:current-machine (llvm:make-machine)])
     (test-case+ "udiv-concrete" (check-udiv-concrete))
     (test-case+ "udiv-symbolic" (check-udiv-symbolic))
     (test-case+ "udiv-spec" (check-udiv-spec))
     (test-failure-case+ "udiv-buggy" (check-udiv-buggy)))))

(module+ test
  (time (run-tests udiv-tests)))
