#lang rosette

(require "debug.rkt" rackunit rackunit/text-ui rosette/lib/roseunit)

(provide (all-defined-out) run-tests (all-from-out rackunit) (all-from-out rosette/lib/roseunit))

(define quickcheck-max-success (make-parameter 100))

(define-simple-check (check-unsat? sol) (unsat? sol))
(define-simple-check (check-sat? sol) (sat? sol))

; A version of test-case that clears all Rosette state after; executing and
; inserts an additional check that the test does not leave around unnecessary
; assertions.
(define-syntax-rule (test-case+ name body ...)
  (test-case name (begin
    (printf "Running test ~v\n" name)
    (time (with-asserts-only
      (parameterize ([current-bitwidth (current-bitwidth)]
                     [term-cache (hash-copy (term-cache))]
                     [current-solver (current-solver)]
                     [current-oracle (oracle (current-oracle))])
        (check-asserts-only (begin body ...))
        (check-equal? (asserts) null))))
    (bug-clear!)
    (printf "Finished test ~v\n" name))))

(define-syntax-rule (check-asserts expr)
  (let-values ([(result asserted) (with-asserts expr)])
    (check-unsat? (verify (assert (apply && asserted))))
    result))

(define-syntax-rule (check-asserts-only expr)
  (let ([asserted (with-asserts-only expr)])
    (check-unsat? (verify (assert (apply && asserted))))
    (void)))


; random testing

; generate a random value from a symbolic expression
(define (arbitrary expr)
  (define syms (symbolics expr))
  (define sol
    (for/hash [(v syms)]
      (values v (cond
        [(boolean? v)
         (zero? (random 2))]
        [(bv? v)
         (apply concat (build-list (bitvector-size (type-of v))
                                   (lambda (x) (bv (random 2) 1))))]))))
  (evaluate expr (sat sol)))

(define-syntax-rule (quickcheck body ...)
  (let ([n (quickcheck-max-success)])
    (for ([i (in-range n)])
      body ...)
    (printf "+++ OK, passed ~a tests.\n" n)))
