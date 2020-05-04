#lang rosette

(require
  "common.rkt")

(provide
  and-immediate
  orr-immediate
  eor-immediate
  ands-immediate)


(define (decode sf N immr imms Rn Rd)
  (define d Rd)
  (define n Rn)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  ; sf == '0' && N != '0'
  (when (&& (bveq sf (bv #b0 1)) (! (bveq N (bv #b0 1))))
    (undefined))

  (define-values (imm _) (decode-bit-masks datasize N imms immr #t))

  (values d n datasize imm))


(define (interpret-and-immediate cpu sf N immr imms Rn Rd)
  (define-values (d n datasize imm) (decode sf N immr imms Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))

  (define result (bvand operand1 imm))
  (if (equal? d (integer->gpr 31))
      (cpu-sp-set! cpu result)
      (cpu-gpr-set! cpu d result)))


(define (interpret-orr-immediate cpu sf N immr imms Rn Rd)
  (define-values (d n datasize imm) (decode sf N immr imms Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))

  (define result (bvor operand1 imm))
  (if (equal? d (integer->gpr 31))
      (cpu-sp-set! cpu result)
      (cpu-gpr-set! cpu d result)))


(define (interpret-eor-immediate cpu sf N immr imms Rn Rd)
  (define-values (d n datasize imm) (decode sf N immr imms Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))

  (define result (bvxor operand1 imm))
  (if (equal? d (integer->gpr 31))
      (cpu-sp-set! cpu result)
      (cpu-gpr-set! cpu d result)))


(define (interpret-ands-immediate cpu sf N immr imms Rn Rd)
  (define-values (d n datasize imm) (decode sf N immr imms Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))

  (define result (bvand operand1 imm))
  (cpu-nzcv-set! cpu (nzcv (bit (sub1 datasize) result) (is-zero-bit result) (bv #b0 1) (bv #b0 1)))

  (cpu-gpr-set! cpu d result))


(define-insn (sf N immr imms Rn Rd)
  #:encode (lambda (opc) (list sf (bv opc 2) (bv #b100100 6) N immr imms Rn Rd))
  [(#b00) and-immediate interpret-and-immediate]
  [(#b01) orr-immediate interpret-orr-immediate]
  [(#b10) eor-immediate interpret-eor-immediate]
  [(#b11) ands-immediate interpret-ands-immediate])
