#lang rosette

(require
  "common.rkt")

(provide
  ldrd-immediate ldrh-immediate
  strd-immediate strh-immediate)


(define (decode-ldrd P U W Rn Rt imm4H imm4L)
  (when (equal? Rn (integer->gpr #b1111))
    (see "LDRD (literal)"))
  (when (bveq (bit 0 (unbox Rt)) (bv #b1 1))
    (unpredictable))
  (define t Rt)
  (define t2 (box (bvadd1 (unbox t))))
  (define n Rn)
  (define imm32 (zero-extend (concat imm4H imm4L) (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (&& wback (|| (equal? n t) (equal? n t2)))
    (unpredictable))
  (when (r15? t2)
    (unpredictable))
  (values t t2 n imm32 index add wback))

(define (interpret-ldrd cpu P U W Rn Rt imm4H imm4L)
  (define-values (t t2 n imm32 index add wback) (decode-ldrd P U W Rn Rt imm4H imm4L))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define mm (cpu-memmgr cpu))
  (define pc (cpu-pc cpu))
  (cpu-gpr-set! cpu t (core:memmgr-load mm address (bv 0 32) (bv 4 32)))
  (cpu-gpr-set! cpu t2 (core:memmgr-load mm address (bv 4 32) (bv 4 32)))
  (when wback
    (cpu-gpr-set! cpu n offset_addr)))


(define (decode-ldrh P U W Rn Rt imm4H imm4L)
  (when (equal? Rn (integer->gpr #b1111))
    (see "LDRH (literal)"))
  (when (&& (bveq P (bv #b0 1)) (bveq W (bv #b1 1)))
    (see "LDRHT"))
  (define t Rt)
  (define n Rn)
  (define imm32 (zero-extend (concat imm4H imm4L) (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (|| (r15? t) (&& wback (equal? n t)))
    (unpredictable))
  (values t n imm32 index add wback))

(define (interpret-ldrh cpu P U W Rn Rt imm4H imm4L)
  (define-values (t n imm32 index add wback) (decode-ldrh P U W Rn Rt imm4H imm4L))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define data (core:memmgr-load (cpu-memmgr cpu) address (bv 0 32) (bv 2 32)))
  (when wback
    (cpu-gpr-set! cpu n offset_addr))
  (cpu-gpr-set! cpu t (zero-extend data (bitvector 32))))


(define (decode-strd P U W Rn Rt imm4H imm4L)
  (when (bveq (bit 0 (unbox Rt)) (bv #b1 1))
    (unpredictable))
  (define t Rt)
  (define t2 (box (bvadd1 (unbox t))))
  (define n Rn)
  (define imm32 (zero-extend (concat imm4H imm4L) (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (&& wback (|| (r15? n) (equal? n t) (equal? n t2)))
    (unpredictable))
  (when (r15? t2)
    (unpredictable))
  (values t t2 n imm32 index add wback))

(define (interpret-strd cpu P U W Rn Rt imm4H imm4L)
  (define-values (t t2 n imm32 index add wback) (decode-strd P U W Rn Rt imm4H imm4L))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define mm (cpu-memmgr cpu))
  (define pc (cpu-pc cpu))
  (core:memmgr-store! mm address (bv 0 32) (cpu-gpr-ref cpu t) (bv 4 32))
  (core:memmgr-store! mm address (bv 4 32) (cpu-gpr-ref cpu t2) (bv 4 32))
  (when wback
    (cpu-gpr-set! cpu n offset_addr)))


(define (decode-strh P U W Rn Rt imm4H imm4L)
  (when (&& (bveq P (bv #b0 1)) (bveq W (bv #b1 1)))
    (see "STRHT"))
  (define t Rt)
  (define n Rn)
  (define imm32 (zero-extend (concat imm4H imm4L) (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (r15? t)
    (unpredictable))
  (when (&& wback (|| (r15? n) (equal? n t)))
    (unpredictable))
  (values t n imm32 index add wback))

(define (interpret-strh cpu P U W Rn Rt imm4H imm4L)
  (define-values (t n imm32 index add wback) (decode-strh P U W Rn Rt imm4H imm4L))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define data (extract 15 0 (cpu-gpr-ref cpu t)))
  (core:memmgr-store! (cpu-memmgr cpu) address (bv 0 32) data (bv 2 32))
  (when wback
    (cpu-gpr-set! cpu n offset_addr)))


(define-insn (P U W Rn Rt imm4H imm4L)
  #:encode (lambda (o1 op2) (list (bv #b000 3) P U (bv #b1 1) W (bv o1 1) Rn Rt imm4H (bv #b1 1) (bv op2 2) (bv #b1 1) imm4L))
  [(#b0 #b10) ldrd-immediate interpret-ldrd]
  [(#b1 #b01) ldrh-immediate interpret-ldrh]
  [(#b0 #b11) strd-immediate interpret-strd]
  [(#b0 #b01) strh-immediate interpret-strh])
