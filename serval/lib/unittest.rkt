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

(define (verify/debug-proc proc)
  (parameterize ([current-bitwidth (current-bitwidth)]
                 [current-solver (current-solver)])
    (clear-bug-info!)
    (clear-vc!)
    (define result (with-vc vc-true (proc)))
    (when (failed? result)
      (printf "procedure exited abnormally.\n")
      (parameterize ([error-print-width 9999])
        (displayln (exn-message (result-value result))))
      (proc) ; Run it again to get information about the concrete failure.
      (raise (result-value result)))
    (define assocs (result-value result))
    (define v (result-state result))
    (define s (current-solver))
    (solver-assert s (list (vc-assumes v) (! (vc-asserts v))))
    (define model (solver-check s))
    (solver-clear s)
    (define info null)
    (when (sat? model)
      (when (list? assocs)
        (set! info (map (lambda (p) (make-check-info (car p) (evaluate (cdr p) model))) assocs)))
      (printf "Failed assertions:\n")
      (for ([bug (get-bug-info model)])
        (displayln (bug-format bug model))))
    (with-check-info*
      info
      (thunk (check-unsat? model)))
    model))

(define-syntax-rule (test-case+ name body ...)
  (test-case name (begin
    (clear-terms!)
    (printf "~a ~v\n" (color-succ "[ RUN      ]") name)
    (define (proc) (begin body ...))
    (define-values (result cpu-time real-time gc-time) (time-apply verify/debug-proc (list proc)))
    (printf "~a ~v (~vms cpu) (~vms real) (~v terms)\n" (color-succ "[       OK ]") name cpu-time real-time (terms-count)))))

(define-syntax-rule (test-failure-case+ name body ...)
  (test-case name (begin
    (printf "~a ~v\n" (color-succ "[ RUN      ]") name)
    (define (proc) (begin body ...))
    (define-values (result cpu-time real-time gc-time)
      (time-apply (thunk* (check-exn exn:fail? (thunk (verify/debug-proc proc)))) null))
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
        (check-unsat? (verify (assert (vc-asserts v))))
        (result-value result)))))

(define-syntax-rule (quickcheck body ...)
  (let ([n (quickcheck-max-success)])
    (for ([i (in-range n)])
      body ...)
    (printf "+++ OK, passed ~a tests.\n" n)))
