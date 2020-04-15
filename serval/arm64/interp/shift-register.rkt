#lang rosette

(require
  "common.rkt")

(provide
  lslv lsrv asrv rorv)


(define (interpret-shift-variable cpu sf Rm op2 Rn Rd)
  (define d Rd)
  (define n Rn)
  (define m Rm)
  (define datasize (if (bveq sf (bv #b1 1)) 64 32))
  (define shift_type (decode-shift op2))

  (define operand1 (trunc datasize (cpu-gpr-ref cpu n)))
  (define operand2 (trunc datasize (cpu-gpr-ref cpu m)))

  ; The ARM manual uses (operand2 MOD datasize); use bit masking instead.
  (define mask (bv (sub1 datasize) datasize))
  (define result (shift-reg cpu n shift_type (bvand operand2 mask)))
  (cpu-gpr-set! cpu d result))


(define-insn (sf Rm op2 Rn Rd)
  #:encode (lambda () (list sf (bv #b0 1) (bv #b0 1) (bv #b11010110 8) Rm (bv #b0010 4) op2 Rn Rd))
  [() shift-variable interpret-shift-variable])

(define (lslv sf Rm Rn Rd)
  (shift-variable sf Rm (bv #b00 2) Rn Rd))

(define (lsrv sf Rm Rn Rd)
  (shift-variable sf Rm (bv #b01 2) Rn Rd))

(define (asrv sf Rm Rn Rd)
  (shift-variable sf Rm (bv #b10 2) Rn Rd))

(define (rorv sf Rm Rn Rd)
  (shift-variable sf Rm (bv #b11 2) Rn Rd))
