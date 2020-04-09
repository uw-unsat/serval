#lang rosette

(require
  "common.rkt")

(provide
  rev
  rev16)


(define (decode Rd Rm)
  (define d Rd)
  (define m Rm)
  (when (|| (r15? d) (r15? m))
    (unpredictable))
  (values d m))

(define (interpret-rev cpu Rd Rm)
  (define-values (d m) (decode Rd Rm))
  (define x (cpu-gpr-ref cpu m))
  (define result (concat (extract 7 0 x) (extract 15 8 x) (extract 23 16 x) (extract 31 24 x)))
  (cpu-gpr-set! cpu d result))

(define (interpret-rev16 cpu Rd Rm)
  (define-values (d m) (decode Rd Rm))
  (define x (cpu-gpr-ref cpu m))
  (define result (concat (extract 23 16 x) (extract 31 24 x) (extract 7 0 x) (extract 15 8 x)))
  (cpu-gpr-set! cpu d result))


(define-insn (Rd Rm)
  #:encode (lambda (o1 o2) (list (bv #b01101 5) (bv o1 1) (bv #b11 2) (bv #b1111 4) Rd (bv #b1111 4) (bv o2 1) (bv #b011 3) Rm))
  [(#b0 #b0) rev interpret-rev]
  [(#b0 #b1) rev16 interpret-rev16])
