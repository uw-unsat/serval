#lang rosette

(require
  "common.rkt")

(provide
  str-immediate strb-immediate
  ldr-immediate ldrb-immediate)


(define (decode-str P U W Rn Rt imm12)
  (when (&& (bveq P (bv #b0 1)) (bveq W (bv #b1 1)))
    (see "STRT"))
  (define t Rt)
  (define n Rn)
  (define imm32 (zero-extend imm12 (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (&& wback (|| (r15? n) (equal? n t)))
    (unpredictable))
  (values t n imm32 index add wback))

(define (interpret-str cpu P U W Rn Rt imm12)
  (define-values (t n imm32 index add wback) (decode-str P U W Rn Rt imm12))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define data (if (r15? t) (pc-store-value cpu) (cpu-gpr-ref cpu t)))
  (core:memmgr-store! (cpu-memmgr cpu) address (bv 0 32) data (bv 4 32))
  (when wback
    (cpu-gpr-set! cpu n offset_addr)))


(define (decode-strb P U W Rn Rt imm12)
  (when (&& (bveq P (bv #b0 1)) (bveq W (bv #b1 1)))
    (see "STRBT"))
  (define t Rt)
  (define n Rn)
  (define imm32 (zero-extend imm12 (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (r15? t)
    (unpredictable))
  (when (&& wback (|| (r15? n) (equal? n t)))
    (unpredictable))
  (values t n imm32 index add wback))

(define (interpret-strb cpu P U W Rn Rt imm12)
  (define-values (t n imm32 index add wback) (decode-strb P U W Rn Rt imm12))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define data (extract 7 0 (cpu-gpr-ref cpu t)))
  (core:memmgr-store! (cpu-memmgr cpu) address (bv 0 32) data (bv 1 32))
  (when wback
    (cpu-gpr-set! cpu n offset_addr)))


(define (decode-ldr P U W Rn Rt imm12)
  (when (equal? Rn (integer->gpr #b1111))
    (see "LDR (literal)"))
  (when (&& (bveq P (bv #b0 1)) (bveq W (bv #b1 1)))
    (see "LDRT"))
  (define t Rt)
  (define n Rn)
  (define imm32 (zero-extend imm12 (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (&& wback (equal? n t))
    (unpredictable))
  (values t n imm32 index add wback))

(define (interpret-ldr cpu P U W Rn Rt imm12)
  (define-values (t n imm32 index add wback) (decode-ldr P U W Rn Rt imm12))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define data (core:memmgr-load (cpu-memmgr cpu) address (bv 0 32) (bv 4 32)))
  (when wback
    (cpu-gpr-set! cpu n offset_addr))
  (if (r15? t)
      (if (bveq (extract 2 0 address) (bv #b00 2))
          (load-write-pc cpu data)
          (unpredictable))
      (cpu-gpr-set! cpu t data)))


(define (decode-ldrb P U W Rn Rt imm12)
  (when (equal? Rn (integer->gpr #b1111))
    (see "LDRB (literal)"))
  (when (&& (bveq P (bv #b0 1)) (bveq W (bv #b1 1)))
    (see "LDRBT"))
  (define t Rt)
  (define n Rn)
  (define imm32 (zero-extend imm12 (bitvector 32)))
  (define index (bveq P (bv #b1 1)))
  (define add (bveq U (bv #b1 1)))
  (define wback (|| (bveq P (bv #b0 1)) (bveq W (bv #b1 1))))
  (when (|| (r15? t) (&& wback (equal? n t)))
    (unpredictable))
  (values t n imm32 index add wback))

(define (interpret-ldrb cpu P U W Rn Rt imm12)
  (define-values (t n imm32 index add wback) (decode-ldrb P U W Rn Rt imm12))
  (define offset_addr ((if add bvadd bvsub) (cpu-gpr-ref cpu n) imm32))
  (define address (if index offset_addr (cpu-gpr-ref cpu n)))
  (define data (core:memmgr-load (cpu-memmgr cpu) address (bv 0 32) (bv 1 32)))
  (cpu-gpr-set! cpu t (zero-extend data (bitvector 32)))
  (when wback
    (cpu-gpr-set! cpu n offset_addr)))


(define-insn (P U W Rn Rt imm12)
  #:encode (lambda (o2 o1) (list (bv #b010 3) P U (bv o2 1) W (bv o1 1) Rn Rt imm12))
  [(#b0 #b0) str-immediate  interpret-str]
  [(#b1 #b0) strb-immediate interpret-strb]
  [(#b0 #b1) ldr-immediate  interpret-ldr]
  [(#b1 #b1) ldrb-immediate interpret-ldrb])
