#lang rosette

(require (except-in rackunit fail)
         rackunit/text-ui
         rosette/lib/roseunit
         serval/lib/core
         serval/lib/unittest
         (prefix-in llvm: serval/llvm))

(require "generated/racket/test/inttoptr.globals.rkt"
         "generated/racket/test/inttoptr.map.rkt")

(require "generated/racket/test/inttoptr.ll.rkt")

(define (check-test test-fn)
  (parameterize ([llvm:current-machine (llvm:make-machine symbols globals)])
    (define ret (test-fn))
    (check-true (bvzero? ret))
  ))

(define inttoptr-tests
  (test-suite+
   "Tests for inttoptr.c"

   (test-case+ "test1" (check-test @test1))
   (test-case+ "test2" (check-test @test2))

  ))

(module+ test
  (time (run-tests inttoptr-tests)))
