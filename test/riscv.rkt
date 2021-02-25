#lang rosette/safe

(require
  serval/lib/unittest
  serval/riscv/interp
  serval/riscv/base
  serval/riscv/interp)

(define (check-jalr-clears-least-bit)
  (define cpu (init-cpu))
  (gpr-set! cpu 'a0 (bv #xffff (XLEN)))
  (define i (jalr (bv 0 12) (gpr->idx 'a0) (bv 0 5)))
  (interpret-insn cpu i)
  (check-equal? (cpu-pc cpu) (bv #xfffe (XLEN))))

(define riscv-tests
  (test-suite+
   "Tests for RISC-V behavior"
     (test-case+ "Check jalr clears least bit" (check-jalr-clears-least-bit))))

(module+ test
  (time (run-tests riscv-tests)))
