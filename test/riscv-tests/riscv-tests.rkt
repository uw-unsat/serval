#lang rosette

(require
  serval/lib/unittest
  serval/riscv/interp
  serval/riscv/objdump
  serval/riscv/base)

(define (run-test testfile)
  (define instrs (dynamic-require testfile 'instructions))
  (define arch (dynamic-require testfile 'architecture))
  (parameterize
    ([XLEN
      (case arch
        [(riscv:rv64) 64]
        [(riscv:rv32) 32])])

    (define cpu (init-cpu))

    (define (handle-failure e)
      (displayln e)
      (define s (format "FAILURE --> Test ~e failed in ~e\n" (bitvector->integer (gpr-ref cpu 'x21)) testfile))
      (assert #f s))

    (define asserted
      (with-asserts-only
        (with-handlers
          ([exn:fail? handle-failure])
          (interpret-objdump-program cpu instrs))))

    (check-unsat? (verify (assert (apply && asserted))))

    (printf "SUCCESS --> ~e\n" testfile)))

(define riscv-tests
  (test-suite+
   "Add test"
     (for ([filename (current-command-line-arguments)])
       (displayln filename)
       (test-case+ filename (run-test (build-path "../" filename))))
  ))

(module+ test
  (time (run-tests riscv-tests)))
