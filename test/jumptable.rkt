#lang rosette/safe

(require
  serval/lib/unittest
  serval/lib/core
  (prefix-in riscv: serval/riscv/objdump)
  (prefix-in jumptable: "generated/racket/test/jumptable.map.rkt")
  (prefix-in jumptable: "generated/racket/test/jumptable.globals.rkt")
  (prefix-in jumptable: "generated/racket/test/jumptable.asm.rkt")
)

(define (find-symbol-start name)
  (define sym (find-symbol-by-name jumptable:symbols name))
  (bv (car sym) (riscv:XLEN)))

(define (check-jumptable)
  (define cpu (riscv:init-cpu jumptable:symbols jumptable:globals))
  (define mret (find-symbol-start 'mret))
  (define init-table (find-symbol-start 'init_table))
  (define call-func (find-symbol-start 'call_func))

  (riscv:gpr-set! cpu 'ra mret)
  (riscv:set-cpu-pc! cpu init-table)
  (check-asserts-only (riscv:interpret-objdump-program cpu jumptable:instructions))

  (define-symbolic* x y (bitvector 64))

  (riscv:gpr-set! cpu 'a0 x)
  (riscv:gpr-set! cpu 'a1 y)
  (riscv:gpr-set! cpu 'a5 (make-bv64))
  (riscv:gpr-set! cpu 'ra mret)
  (riscv:set-cpu-pc! cpu call-func)

  (check-asserts-only (riscv:interpret-objdump-program cpu jumptable:instructions))

  (define result (riscv:gpr-ref cpu 'a0))

  (check-unsat? (verify (assert (bveq result (bvadd x (bvand y (bv 7 64)))))))

  (void))

(define jumptable-tests
  (test-suite+
   "Tests for RISC-V jumptable"
    (test-case+ "check jumptable" (check-jumptable))
  ))

(module+ test
  (time (run-tests jumptable-tests)))
