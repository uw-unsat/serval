#lang rosette

(require
  "common.rkt")

(provide
  madd msub)


(define (interpret-madd/msub cpu sf Rm Ra Rn Rd sub_op)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define a Ra)
  (define destsize (if (bveq sf (bv #b1 1)) 64 32))

  (define operand1 (trunc destsize (cpu-gpr-ref cpu n)))
  (define operand2 (trunc destsize (cpu-gpr-ref cpu m)))
  (define operand3 (trunc destsize (cpu-gpr-ref cpu a)))

  (define result (sub_op operand3 ((core:bvmul-proc) operand1 operand2)))
  (cpu-gpr-set! cpu d result))

(define (interpret-madd cpu sf Rm Ra Rn Rd)
  (interpret-madd/msub cpu sf Rm Ra Rn Rd bvadd))

(define (interpret-msub cpu sf Rm Ra Rn Rd)
  (interpret-madd/msub cpu sf Rm Ra Rn Rd bvsub))


(define-insn (sf Rm Ra Rn Rd)
  #:encode (lambda (o0) (list sf (bv #b00 2) (bv #b11011 5) (bv #b00 3) Rm (bv o0 1) Ra Rn Rd))
  [(#b0) madd interpret-madd]
  [(#b1) msub interpret-msub])
