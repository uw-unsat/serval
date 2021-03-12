#lang rosette

(require (except-in rackunit fail)
         rackunit/text-ui
         rosette/lib/roseunit
         serval/lib/unittest
         serval/lib/debug
         serval/lib/core)

(define (check-bug-info)
  (define-symbolic a b boolean?)
  (bug-assert a #:msg (lambda (sol) (format "a is ~v" (evaluate a sol))))
  (bug-assert a #:msg "another assertion about a")
  (bug-assert b #:msg (lambda (sol) (format "b is ~v" (evaluate b sol))))
  (bug-assert b #:msg "another assertion about b")
  (void))

(define bug-info-tests
  (test-suite+
   "Tests for bug info"
   (test-failure-case+ "check-bug-info" (check-bug-info))))

(module+ test
  (time (run-tests bug-info-tests)))
