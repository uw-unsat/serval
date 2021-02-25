#lang rosette

(require "debug.rkt" rackunit rackunit/text-ui rosette/lib/roseunit)

(provide (all-defined-out) run-tests (all-from-out rackunit) (all-from-out rosette/lib/roseunit))

(define quickcheck-max-success (make-parameter 100))

(define-simple-check (check-unsat? sol) (unsat? sol))
(define-simple-check (check-sat? sol) (sat? sol))

(define (color-string prefix s [out (current-output-port)])
  (if (terminal-port? out)
      (string-append prefix s "\033[0m")
      s))

(define color-succ (curry color-string "\033[0;32m"))

(define (run-test+ proc)
  (parameterize ([current-bitwidth (current-bitwidth)]
                 [current-solver (current-solver)]
                 [assert-db (hash-copy (assert-db))])
    (define result (with-vc vc-true (verify (proc))))
    (check-true (normal? result))
    (check-unsat? (result-value result))))

(define-syntax-rule (test-case+ name body ...)
  (test-case name (begin
    (printf "~a ~v\n" (color-succ "[ RUN      ]") name)
    (define (proc) (begin body ...))
    (define-values (result cpu-time real-time gc-time) (time-apply run-test+ (list proc)))
    (printf "~a ~v (~v ms)\n" (color-succ "[       OK ]") name real-time))))

(define (run-failure-test+ proc)
  (parameterize ([current-bitwidth (current-bitwidth)]
                 [current-solver (current-solver)]
                 [assert-db (hash-copy (assert-db))])
    (define result (with-vc vc-true (verify (proc))))
    (check-false (unsat? (result-value result)))))

(define-syntax-rule (test-failure-case+ name body ...)
  (test-case name (begin
    (printf "~a ~v\n" (color-succ "[ RUN      ]") name)
    (define (proc) (begin body ...))
    (define-values (result cpu-time real-time gc-time) (time-apply run-failure-test+ (list proc)))
    (printf "~a ~v (~v ms)\n" (color-succ "[       OK ]") name real-time))))

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

(define-syntax-rule (check-vc expr)
  (begin
    (let ([result (with-vc expr)])
      (check-true (normal? result))
      (let ([v (result-state result)])
        (check-unsat? (verify (begin (assume (vc-assumes v)) (assert (vc-asserts v)))))
        (result-value result)))))

(define-syntax-rule (quickcheck body ...)
  (let ([n (quickcheck-max-success)])
    (for ([i (in-range n)])
      body ...)
    (printf "+++ OK, passed ~a tests.\n" n)))
