#lang rosette

(require
  "common.rkt")

(provide
  b bl)


(define (decode imm26)
  (sign-extend (concat imm26 (bv #b00 2)) (bitvector 64)))

(define (interpret-b cpu imm26)
  (define offset (decode imm26))
  (branch-to cpu (bvadd (cpu-pc-ref cpu) offset)))

(define (interpret-bl cpu imm26)
  (define offset (decode imm26))
  (cpu-gpr-set! cpu (integer->gpr 30) (bvadd (cpu-pc-ref cpu) (bv 4 64)))
  (branch-to cpu (bvadd (cpu-pc-ref cpu) offset)))


(define-insn (imm26)
  #:encode (lambda (op) (list (bv op 1) (bv #b00101 5) imm26))
  [(#b0) b  interpret-b]
  [(#b1) bl interpret-bl])
