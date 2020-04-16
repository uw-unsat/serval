#lang rosette

(require
  "common.rkt")

(provide
  blx-register)


(define (interpret-blx-register cpu Rm)
  (define m Rm)
  (when (r15? m)
    (unpredictable))

  (define target (cpu-gpr-ref cpu Rm))

  (define next-instr-addr (bvsub (pc-store-value cpu) (bv 4 32)))
  (cpu-gpr-set! cpu (integer->gpr 14) next-instr-addr)
  (bx-write-pc cpu target 'INDCALL))


(define-insn (Rm)
  #:encode (lambda (op0 op1) (list (bv #b00010 5) (bv op0 2) (bv 0 1) (bv -1 12) (bv 0 1) (bv op1 3) Rm))
  [(#b01 #b011) blx-register interpret-blx-register])
