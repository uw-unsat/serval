#lang rosette

(require
  "common.rkt")

(provide
  udiv sdiv)


(define (interpret-udiv/sdiv cpu sf Rm Rn Rd proc)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (trunc datasize (cpu-gpr-ref cpu m)))

  (define result
    (if (is-zero operand2)
        (zeros datasize)
        (proc operand1 operand2)))

  (cpu-gpr-set! cpu d result))

(define (interpret-udiv cpu sf Rm Rn Rd)
  (interpret-udiv/sdiv cpu sf Rm Rn Rd (core:bvudiv-proc)))

(define (interpret-sdiv cpu sf Rm Rn Rd)
  (interpret-udiv/sdiv cpu sf Rm Rn Rd (core:bvsdiv-proc)))


(define-insn (sf Rm Rn Rd)
  #:encode (lambda (o1) (list sf (bv #b0 1) (bv #b0 1) (bv #b11010110 8) Rm (bv #b00001 5) (bv o1 1) Rn Rd))
  [(#b0) udiv interpret-udiv]
  [(#b1) sdiv interpret-sdiv])
