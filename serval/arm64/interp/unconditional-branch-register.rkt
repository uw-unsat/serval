#lang rosette

(require
  "common.rkt")

(provide
  ret br blr)


(define (interpret-br cpu Rn)
  (define target (cpu-gpr-ref cpu Rn))
  (branch-to cpu target))

(define (interpret-blr cpu Rn)
  (define target (cpu-gpr-ref cpu Rn))
  (define pc (cpu-pc-ref cpu))
  (cpu-gpr-set! cpu (integer->gpr 30) (bvadd pc (bv 4 (type-of pc))))
  (branch-to cpu target))


(define-insn (Rn)
  #:encode (lambda (opc op2 op3 op4) (list (bv #b1101011 7) (bv opc 4) (bv op2 5) (bv op3 6) Rn (bv op4 5)))
  [(#b0010 #b11111 #b000000 #b00000) ret interpret-br]
  [(#b0000 #b11111 #b000000 #b00000) br  interpret-br]
  [(#b0001 #b11111 #b000000 #b00000) blr interpret-blr])
