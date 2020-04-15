#lang rosette

(require
  "common.rkt")

(provide
  b.cond)


(define (decode imm19)
  (sign-extend (concat imm19 (bv #b00 2)) (bitvector 64)))

(define (interpret-b.cond cpu imm19 cond)
  (define offset (decode imm19))
  (when (condition-holds cpu cond)
    (branch-to cpu (bvadd (cpu-pc-ref cpu) offset))))


(define-insn (imm19 cond)
  #:encode (lambda (o1 o0) (list (bv #b0101010 7) (bv o1 1) imm19 (bv o0 1) cond))
  [(#b0 #b0) b.cond interpret-b.cond])
