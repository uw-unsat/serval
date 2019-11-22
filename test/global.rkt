#lang rosette

(require (except-in rackunit fail)
         rackunit/text-ui
         rosette/lib/roseunit
         serval/lib/core
         serval/lib/unittest
         serval/llvm)

(require "generated/racket/test/global.globals.rkt"
         "generated/racket/test/global.map.rkt")

(require "generated/racket/test/global.ll.rkt")

(define N 4096)

(define (check-global-val x)
  (parameterize ([current-machine (make-machine symbols globals)])
    (define exp (bv x 64))
    (check-not-equal? (@get_value) exp)
    (@set_value exp)
    (define act (@get_value))
    (check-equal? act exp)))

(define (check-global-concrete)
  (check-global-val 42)
  (check-global-val 0)
  (check-equal? (asserts) null))

(define (check-global-symbolic)
  (define-symbolic v64 (bitvector 64))
  (define-symbolic v32 (bitvector 32))
  ; @val i64
  (parameterize ([current-machine (make-machine symbols globals)])
    (@set_value v64)
    (check-equal? (asserts) null)
    (check-equal? (@get_value) v64))
  ; @vals [10 x i32]
  ; constant index
  (parameterize ([current-machine (make-machine symbols globals)])
    (@set_value_i (bv 1 64) v32)
    (check-equal? (asserts) null)
    (check-equal? (@get_value_i (bv 1 64)) v32))
  ; symbolic index
  (parameterize ([current-machine (make-machine symbols globals)])
    (define pre (bvult v64 (bv N 64)))
    (define-values (cond asserted)
      (with-asserts
        (begin
          (@set_value_i v64 v32)
          (equal? (@get_value_i v64) v32))))
    (check-equal? (asserts) null)
    (for-each (lambda (x) (check-unsat? (verify (assert x)))) asserted)
    (check-sat (solve (assert cond)))
    (check-unsat? (verify (assert (=> pre cond))))))

(define-symbolic spec-vals (~> i64 i32))

(define (spec-set_value_i i val)
  (let ([old spec-vals])
    (set! spec-vals (lambda (x) (if (equal? x i) val (old x))))))

(define (vals-eqv?)
  (define-symbolic i i64)
  (define block (symbol->block 'vals))
  (forall (list i) (=> (bvult i (bv N 64))
                       (equal? (spec-vals i) (mblock-iload block (list i))))))

(define (check-global-spec)
  (parameterize ([current-machine (make-machine symbols globals)])
    (define-symbolic i i64)
    (define-symbolic v i32)
    (define pre (vals-eqv?))
    (spec-set_value_i i v)
    (define asserted
      (with-asserts-only (@set_value_i i v)))
    (define post (vals-eqv?))
    ; no UB triggered
    (check-equal? (asserts) null)
    (for-each (lambda (x) (check-unsat? (verify (assert x)))) asserted)
    ; verify spec against impl
    (check-unsat? (verify (assert (=> pre post))))))

(define global-tests
  (test-suite+
   "Tests for global.c"

   (test-case+ "global-concrete" (check-global-concrete))
   (test-case+ "global-symbolic" (check-global-symbolic))
   (test-case+ "global-spec" (check-global-spec))))

(module+ test
  (time (run-tests global-tests)))
