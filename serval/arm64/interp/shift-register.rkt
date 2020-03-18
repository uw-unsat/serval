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

(define (interpret-lslv cpu sf Rm Rn Rd)
  (interpret-shift-variable cpu sf Rm (bv #b00 2) Rn Rd))

(define (interpret-lsrv cpu sf Rm Rn Rd)
  (interpret-shift-variable cpu sf Rm (bv #b01 2) Rn Rd))

(define (interpret-asrv cpu sf Rm Rn Rd)
  (interpret-shift-variable cpu sf Rm (bv #b10 2) Rn Rd))

(define (interpret-rorv cpu sf Rm Rn Rd)
  (interpret-shift-variable cpu sf Rm (bv #b11 2) Rn Rd))


(define (shift-register op2)
  (lambda (sf Rm Rn Rd)
    (concat sf (bv #b0 1) (bv #b0 1) (bv #b11010110 8) Rm (bv #b0010 4) (bv op2 2) Rn Rd)))

(define-insn shift-register (sf Rm Rn Rd)
  [(#b00) lslv interpret-lslv]
  [(#b01) lsrv interpret-lsrv]
  [(#b10) asrv interpret-asrv]
  [(#b11) rorv interpret-rorv])
