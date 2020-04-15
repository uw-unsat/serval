#lang rosette

(require
  "common.rkt")

(provide
  and-shifted-register
  bic-shifted-register
  orr-shifted-register
  orn-shifted-register
  eor-shifted-register
  eon-shifted-register
  ands-shifted-register
  bics-shifted-register)


(define (decode sf shift Rm imm6 Rn Rd)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  ; sf == '0' && imm6<5> == '1'
  (when (&& (bveq sf (bv #b0 1)) (bveq (bit 5 imm6) (bv #b1 1)))
    (undefined))

  (define shift_type (decode-shift shift))
  (define shift_amount (zero-extend imm6 (bitvector datasize)))

  (values d n m datasize shift_type shift_amount))


(define (interpret-and-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (define result (bvand operand1 operand2))
  (cpu-gpr-set! cpu d result))


(define (interpret-bic-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (set! operand2 (bvnot operand2))

  (define result (bvand operand1 operand2))
  (cpu-gpr-set! cpu d result))


(define (interpret-orr-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (define result (bvor operand1 operand2))
  (cpu-gpr-set! cpu d result))


(define (interpret-orn-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (set! operand2 (bvnot operand2))

  (define result (bvor operand1 operand2))
  (cpu-gpr-set! cpu d result))


(define (interpret-eor-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (define result (bvxor operand1 operand2))
  (cpu-gpr-set! cpu d result))


(define (interpret-eon-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (set! operand2 (bvnot operand2))

  (define result (bvxor operand1 operand2))
  (cpu-gpr-set! cpu d result))


(define (interpret-ands-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (define result (bvand operand1 operand2))
  (cpu-nzcv-set! cpu (nzcv (bit (sub1 datasize) result) (is-zero-bit result) (bv #b0 1) (bv #b0 1)))

  (cpu-gpr-set! cpu d result))


(define (interpret-bics-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (set! operand2 (bvnot operand2))

  (define result (bvand operand1 operand2))
  (cpu-nzcv-set! cpu (nzcv (bit (sub1 datasize) result) (is-zero-bit result) (bv #b0 1) (bv #b0 1)))

  (cpu-gpr-set! cpu d result))


(define-insn (sf shift Rm imm6 Rn Rd)
  #:encode (lambda (opc N) (list sf (bv opc 2) (bv #b01010 5) shift (bv N 1) Rm imm6 Rn Rd))
  [(#b00 #b0) and-shifted-register interpret-and-shifted-register]
  [(#b00 #b1) bic-shifted-register interpret-bic-shifted-register]
  [(#b01 #b0) orr-shifted-register interpret-orr-shifted-register]
  [(#b01 #b1) orn-shifted-register interpret-orn-shifted-register]
  [(#b10 #b0) eor-shifted-register interpret-eor-shifted-register]
  [(#b10 #b1) eon-shifted-register interpret-eon-shifted-register]
  [(#b11 #b0) ands-shifted-register interpret-ands-shifted-register]
  [(#b11 #b1) bics-shifted-register interpret-bics-shifted-register])
