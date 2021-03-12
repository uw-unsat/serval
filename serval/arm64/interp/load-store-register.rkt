#lang rosette

(require
  "common.rkt")

(provide
  strb strh str32 str64
  ldrb ldrh ldr32 ldr64)


(define (decode size Rm option S Rn Rt)
  (define scale (bitvector->natural size))

  (when (bveq (bit 1 option) (bv #b0 1))
    (undefined))

  (define extend_type (decode-reg-extend option))
  (define shift (if (bveq S (bv #b1 1)) scale 0))

  (define n Rn)
  (define t Rt)
  (define m Rm)

  (define regsize (if (bveq size (bv #b11 2)) 64 32))
  (define datasize (arithmetic-shift 8 scale))

  (values extend_type shift n t m regsize datasize))


(define (interpret-store-register cpu size Rm option S Rn Rt)
  (define-values (extend_type shift n t m regsize datasize) (decode size Rm option S Rn Rt))
  (define offset (extend-reg 64 cpu m extend_type shift))

  (define address
    (if (equal? n (integer->gpr 31))
        (begin
          (check-sp-alignment cpu)
          (cpu-sp-ref cpu))
        (cpu-gpr-ref cpu n)))

  (define data (trunc datasize (cpu-gpr-ref cpu t)))
  (core:memmgr-store! (cpu-memmgr cpu) address offset data (bv (quotient datasize 8) 64)))


(define (interpret-load-register cpu size Rm option S Rn Rt)
  (define-values (extend_type shift n t m regsize datasize) (decode size Rm option S Rn Rt))
  (define offset (extend-reg 64 cpu m extend_type shift))

  (define address
    (if (equal? n (integer->gpr 31))
        (begin
          (check-sp-alignment cpu)
          (cpu-sp-ref cpu))
        (cpu-gpr-ref cpu n)))

  (define data (core:memmgr-load (cpu-memmgr cpu) address offset (bv (quotient datasize 8) 64)))
  (cpu-gpr-set! cpu t (zero-extend data (bitvector regsize))))


(define-insn (size Rm option S Rn Rt)
  #:encode (lambda (opc) (list size (bv #b111 3) (bv #b0 1) (bv #b00 2) (bv opc 2) (bv #b1 1) Rm option S (bv #b10 2) Rn Rt))
  [(#b00) store-register interpret-store-register]
  [(#b01) load-register interpret-load-register])

(define (strb Rm option S Rn Rt)
  (store-register (bv #b00 2) Rm option S Rn Rt))

(define (strh Rm option S Rn Rt)
  (store-register (bv #b01 2) Rm option S Rn Rt))

(define (str32 Rm option S Rn Rt)
  (store-register (bv #b10 2) Rm option S Rn Rt))

(define (str64 Rm option S Rn Rt)
  (store-register (bv #b11 2) Rm option S Rn Rt))

(define (ldrb Rm option S Rn Rt)
  (load-register (bv #b00 2) Rm option S Rn Rt))

(define (ldrh Rm option S Rn Rt)
  (load-register (bv #b01 2) Rm option S Rn Rt))

(define (ldr32 Rm option S Rn Rt)
  (load-register (bv #b10 2) Rm option S Rn Rt))

(define (ldr64 Rm option S Rn Rt)
  (load-register (bv #b11 2) Rm option S Rn Rt))
