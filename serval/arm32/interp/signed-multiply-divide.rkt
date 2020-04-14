#lang rosette

(require
  "common.rkt")

(provide
  sdiv
  udiv)


(define (decode Rd Rm Rn)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (when (|| (r15? d) (r15? n) (r15? m))
    (unpredictable))
  (values d n m))

(define (interpret-sdiv/udiv proc cpu Rd Rm Rn)
  (define-values (d n m) (decode Rd Rm Rn))
  (define operand1 (cpu-gpr-ref cpu n))
  (define operand2 (cpu-gpr-ref cpu m))
  (define result (if (bvzero? operand2) (bv 0 32) ((proc) operand1 operand2)))
  (cpu-gpr-set! cpu d result))

(define interpret-sdiv
  (curry interpret-sdiv/udiv core:bvsdiv-proc))

(define interpret-udiv
  (curry interpret-sdiv/udiv core:bvudiv-proc))


(define-insn (Rd Rm Rn)
  #:encode (lambda (op1 Ra op2) (list (bv #b01110 5) (bv op1 3) Rd (bv Ra 4) Rm (bv op2 3) (bv #b1 1) Rn))
  [(#b001 #b1111 #b000) sdiv interpret-sdiv]
  [(#b011 #b1111 #b000) udiv interpret-udiv])
