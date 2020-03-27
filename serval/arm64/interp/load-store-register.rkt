#lang rosette

(require
  "common.rkt")

(provide
  strb ldrb
  strh ldrh
  str32 ldr32
  str64 ldr64)


(define (decode size Rm option S Rn Rt)
  (define scale (bitvector->natural size))

  (when (bveq (core:bit 1 option) (bv #b0 1))
    (undefined))

  (define extend_type (decode-reg-extend option))
  (define shift (if (bveq S (bv #b1 1)) scale 0))

  (define n Rn)
  (define t Rt)
  (define m Rm)

  (define regsize (if (bveq size (bv #b11 2)) 64 32))
  (define datasize (arithmetic-shift 8 scale))

  (values extend_type shift n t m regsize datasize))


(define (interpret-load cpu size Rm option S Rn Rt)
  (define-values (extend_type shift n t m regsize datasize) (decode size Rm option S Rn Rt))
  (define offset (extend-reg 64 cpu m extend_type shift))

  (define address
    (if (bveq n (integer->gpr 31))
        (cpu-sp-ref cpu)
        (cpu-gpr-ref cpu n)))

  (define data (core:memmgr-load (cpu-memmgr cpu) address offset (bv (quotient datasize 8) 64) #:dbg (cpu-pc-ref cpu)))
  (cpu-gpr-set! cpu t (zero-extend data (bitvector regsize))))

(define (interpret-ldrb cpu Rm option S Rn Rt)
  (interpret-load cpu (bv #b00 2) Rm option S Rn Rt))

(define (interpret-ldrh cpu Rm option S Rn Rt)
  (interpret-load cpu (bv #b01 2) Rm option S Rn Rt))

(define (interpret-ldr32 cpu Rm option S Rn Rt)
  (interpret-load cpu (bv #b10 2) Rm option S Rn Rt))

(define (interpret-ldr64 cpu Rm option S Rn Rt)
  (interpret-load cpu (bv #b11 2) Rm option S Rn Rt))


(define (interpret-store cpu size Rm option S Rn Rt)
  (define-values (extend_type shift n t m regsize datasize) (decode size Rm option S Rn Rt))
  (define offset (extend-reg 64 cpu m extend_type shift))

  (define address
    (if (bveq n (integer->gpr 31))
        (cpu-sp-ref cpu)
        (cpu-gpr-ref cpu n)))

  (define data (trunc datasize (cpu-gpr-ref cpu t)))
  (core:memmgr-store! (cpu-memmgr cpu) address offset data (bv (quotient datasize 8) 64) #:dbg (cpu-pc-ref cpu))
)

(define (interpret-strb cpu Rm option S Rn Rt)
  (interpret-store cpu (bv #b00 2) Rm option S Rn Rt))

(define (interpret-strh cpu Rm option S Rn Rt)
  (interpret-store cpu (bv #b01 2) Rm option S Rn Rt))

(define (interpret-str32 cpu Rm option S Rn Rt)
  (interpret-store cpu (bv #b10 2) Rm option S Rn Rt))

(define (interpret-str64 cpu Rm option S Rn Rt)
  (interpret-store cpu (bv #b11 2) Rm option S Rn Rt))


(define (load-store-register size opc)
  (lambda (Rm option S Rn Rt)
    (concat (bv size 2) (bv #b111 3) (bv #b0 1) (bv #b00 2) (bv opc 2) (bv #b1 1) Rm option S (bv #b10 2) Rn Rt)))

(define-insn load-store-register (Rm option S Rn Rt)
  [(#b00 #b00) strb interpret-strb]
  [(#b00 #b01) ldrb interpret-ldrb]
  [(#b01 #b00) strh interpret-strh]
  [(#b01 #b01) ldrh interpret-ldrh]
  [(#b10 #b00) str32 interpret-str32]
  [(#b10 #b01) ldr32 interpret-ldr32]
  [(#b11 #b00) str64 interpret-str64]
  [(#b11 #b01) ldr64 interpret-ldr64])
