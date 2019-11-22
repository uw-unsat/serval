#lang rosette/safe

(require
  serval/lib/unittest
  serval/riscv/interp
  serval/riscv/base
  serval/riscv/synth)

(define (testcase)
  (define cpu (init-cpu))

  (define s (symbolics cpu))
  (define insn (??-binary-op-imm))

  (interpret-instr cpu insn)

  (define sol
    (synthesize
      #:forall s
      #:guarantee (assert (equal? (gpr-ref cpu 'a5) (gpr-ref cpu 'a0)))))

  (displayln (evaluate insn (complete-solution sol (symbolics insn)))))

(define synth-tests
  (test-suite+
   "Tests for random synthesis thing"
     (test-case+ "random thing" (testcase))))

(module+ test
  (time (run-tests synth-tests)))
