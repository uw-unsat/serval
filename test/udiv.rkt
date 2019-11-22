#lang rosette

(require (except-in rackunit fail)
         rackunit/text-ui
         rosette/lib/roseunit
         serval/llvm
         serval/lib/unittest
         serval/lib/core)

(require "generated/racket/test/udiv.ll.rkt")


(define-symbolic x y i32)

(define (check-udiv x y #:result [result (/ x y)])
  (check-equal? (@udiv (bv x i32) (bv y i32)) (bv result i32)))

(define (check-udiv-concrete)
  (check-udiv 1 0 #:result 0)
  (check-udiv 42 2)
  (check-equal? (asserts) null))

(define (spec-udiv x y)
  (if (bvzero? y) (bv 0 i32) (bvudiv x y)))

(define (check-udiv-symbolic)
  (check-equal? (@udiv x (bv 0 i32)) (bv 0 i32))
  (check-equal? (@udiv x (bv 1 i32)) x)
  (check-equal? (@udiv (bv 0 i32) y) (bv 0 i32))
  (check-equal? (asserts) null))

(define (check-udiv-spec)
  ; bogus spec
  (define bogus-cond (equal? (@udiv x y) (bvudiv x y)))
  (check-equal? (asserts) null)
  (define sol (verify (assert bogus-cond)))
  (check-sat sol)
  (check-equal? (evaluate y sol) (bv 0 32))
  ; llvm udiv (assumes y != 0)
  (define-values (llvm-cond asserted)
    (with-asserts (equal? (@udiv x y) (udiv x y))))
  (check-equal? (length asserted) 1)
  (check-unsat? (verify (assert (=> (first asserted) llvm-cond))))
  ; verify
  (define cond (equal? (@udiv x y) (spec-udiv x y)))
  (check-equal? (asserts) null)
  (check-unsat? (verify (assert cond))))

(define (check-udiv-buggy)
  (define asserted
    (with-asserts-only
        (@udiv_buggy x y)))
  (check-equal? (asserts) null)
  (check-equal? (length asserted) 1)
  (define cond (first asserted))
  (define sol (verify (assert cond)))
  ;(for-each (lambda (x) (displayln x))
  ;          (assert-backtrace sol))
  (check-sat sol))

(define udiv-tests
  (test-suite+
   "Tests for udiv.c"

   (parameterize ([current-machine (make-machine)])
     (test-case+ "udiv-concrete" (check-udiv-concrete))
     (test-case+ "udiv-symbolic" (check-udiv-symbolic))
     (test-case+ "udiv-spec" (check-udiv-spec))
     (test-case+ "udiv-buggy" (check-udiv-buggy)))))

(module+ test
  (time (run-tests udiv-tests)))
