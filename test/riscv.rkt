#lang rosette/safe

(require
  serval/lib/unittest
  serval/riscv/interp
  serval/riscv/base
  serval/riscv/synth)

(define (check-jalr-clears-least-bit)
  (define cpu (init-cpu))
  (gpr-set! cpu 'a0 (bv #xffff (XLEN)))
  (define i (instr 'jalr 'zero 'a0 #f (bv 0 12) 4))
  (interpret-instr cpu i)
  (check-equal? (cpu-pc cpu) (bv #xfffe (XLEN)))
  (check-equal? (asserts) null))

(define riscv-tests
  (test-suite+
   "Tests for RISC-V behavior"
     (test-case+ "Check jalr clears least bit" (check-jalr-clears-least-bit))))

(module+ test
  (time (run-tests riscv-tests)))
