#lang rosette

(require
  "common.rkt")

(provide
  add-shifted-register adds-shifted-register
  sub-shifted-register subs-shifted-register)


(define (decode sf shift Rm imm6 Rn Rd)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))

  ; shift == '11'
  (when (bveq shift (bv #b11 2))
    (undefined))
  ; sf == '0' && imm6<5> == '1'
  (when (&& (bveq sf (bv #b0 1)) (bveq (bit 5 imm6) (bv #b1 1)))
    (undefined))

  (define shift_type (decode-shift shift))
  (define shift_amount (zero-extend imm6 (bitvector datasize)))

  (values d n m datasize shift_type shift_amount))


(define (interpret-add-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (define-values (result _)
    (add-with-carry operand1 operand2 (bv #b0 1)))

  (cpu-gpr-set! cpu d result))


(define (interpret-adds-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (define-values (result nzcv)
    (add-with-carry operand1 operand2 (bv #b0 1)))

  (cpu-nzcv-set! cpu nzcv)

  (cpu-gpr-set! cpu d result))


(define (interpret-sub-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (set! operand2 (bvnot operand2))
  (define-values (result _)
    (add-with-carry operand1 operand2 (bv #b1 1)))

  (cpu-gpr-set! cpu d result))


(define (interpret-subs-shifted-register cpu sf shift Rm imm6 Rn Rd)
  (define-values (d n m datasize shift_type shift_amount) (decode sf shift Rm imm6 Rn Rd))
  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (shift-reg cpu m shift_type shift_amount))

  (set! operand2 (bvnot operand2))
  (define-values (result nzcv)
    (add-with-carry operand1 operand2 (bv #b1 1)))

  (cpu-nzcv-set! cpu nzcv)

  (cpu-gpr-set! cpu d result))


(define-insn (sf shift Rm imm6 Rn Rd)
  #:encode (lambda (op S) (list sf (bv op 1) (bv S 1) (bv #b01011 5) shift (bv #b0 1) Rm imm6 Rn Rd))
  [(#b0 #b0) add-shifted-register  interpret-add-shifted-register]
  [(#b0 #b1) adds-shifted-register interpret-adds-shifted-register]
  [(#b1 #b0) sub-shifted-register  interpret-sub-shifted-register]
  [(#b1 #b1) subs-shifted-register interpret-subs-shifted-register])
