#lang rosette/safe

(require
  serval/lib/unittest
  (prefix-in riscv: serval/riscv/objdump)
  (prefix-in noop: "generated/racket/test/noop.asm.rkt")
)

(define (check-noop-riscv)
  (define cpu (riscv:init-cpu))
  (check-equal? (asserts) null)
  (riscv:interpret-objdump-program cpu noop:instructions)
  (check-equal? (asserts) null))

(define noop-tests
  (test-suite+
   "Tests for RISC-V noop"
    (test-case+ "noop" (check-noop-riscv))
  ))

(module+ test
  (time (run-tests noop-tests)))
