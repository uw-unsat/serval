#lang rosette

(require
  "common.rkt")

(provide
  b)


(define (decode imm24)
  (define imm32 (sign-extend (concat imm24 (bv #b00 2)) (bitvector 32)))
  (values imm32))

(define (interpret-b cpu imm24)
  (define-values (imm32) (decode imm24))
  (branch-write-pc cpu (bvadd (cpu-gpr-ref cpu (integer->gpr 15)) imm32) 'DIR))


(define-insn (imm24)
  #:encode (lambda (H) (list (bv #b101 3) (bv H 1) imm24))
  [(#b0) b interpret-b])
