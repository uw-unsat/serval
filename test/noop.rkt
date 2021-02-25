#lang rosette/safe

(require
  serval/lib/unittest
  serval/riscv/interp
  (prefix-in riscv: serval/riscv/objdump)
  (prefix-in noop: "generated/racket/test/noop.asm.rkt")
)

(define (check-noop-riscv)
  (define cpu (riscv:init-cpu))
  (riscv:interpret-objdump-program cpu noop:instructions)
  (check-true (vc-true? (vc))))

(define noop-tests
  (test-suite+
   "Tests for RISC-V noop"
    (test-case+ "noop" (check-noop-riscv))
  ))

(module+ test
  (time (run-tests noop-tests)))
