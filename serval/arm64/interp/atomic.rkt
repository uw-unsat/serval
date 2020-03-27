#lang rosette

(require
  "common.rkt")

(provide
  ldadd32 ldadd64
  stadd32 stadd64)


(define (decode-ldadd* size A R Rs Rn Rt)
  (define t Rt)
  (define n Rn)
  (define s Rs)

  (define datasize (arithmetic-shift 8 (bitvector->natural size)))
  (define regsize (if (equal? datasize 64) 64 32))
  (define ldacctype (if (&& (bveq A (bv #b1 1)) (! (bveq Rt (bv #b11111 5)))) 'ORDEREDATOMICRW 'ATOMICRW))
  (define stacctype (if (bveq R (bv #b1 1)) 'ORDEREDATOMICRW 'ATOMICRW))
  (define tag_checked (! (bveq n (integer->gpr 31))))

  (values t n s datasize regsize ldacctype stacctype tag_checked))


(define (interpret-ldadd* cpu size A R Rs Rn Rt)
  (define-values (t n s datasize regsize ldacctype stacctype tag_checked) (decode-ldadd* size A R Rs Rn Rt))
  (define value (trunc datasize (cpu-gpr-ref cpu s)))
  (define address
    (if (bveq n (integer->gpr 31))
        (begin
          (check-sp-alignment cpu)
          (cpu-sp-ref cpu))
        (cpu-gpr-ref cpu n)))

  (define data (mem-atomic cpu address 'ADD value ldacctype stacctype))
  (unless (bveq t (integer->gpr 31))
    (cpu-gpr-set! t (zero-extend data (bitvector regsize)))))

(define (interpret-ldadd32 cpu Rs Rn Rt)
  (interpret-ldadd* cpu (bv #b10 2) (bv #b0 1) (bv #b0 1) Rs Rn Rt))

(define (interpret-ldadd64 cpu Rs Rn Rt)
  (interpret-ldadd* cpu (bv #b11 2) (bv #b0 1) (bv #b0 1) Rs Rn Rt))


(define (atomic size V A R o3 opc)
  (lambda (Rs Rn Rt)
    (concat (bv size 2) (bv #b111 3) (bv V 1) (bv #b00 2) (bv A 1) (bv R 1) (bv #b1 1) Rs (bv o3 1) (bv opc 3) (bv #b00 2) Rn Rt)))

(define-insn atomic (Rs Rn Rt)
  [(#b10 #b0 #b0 #b0 #b0 #b000) ldadd32 interpret-ldadd32]
  [(#b11 #b0 #b0 #b0 #b0 #b000) ldadd64 interpret-ldadd64])

(define (stadd32 Rs Rn)
  (ldadd32 Rs Rn (bv #b11111 5)))

(define (stadd64 Rs Rn)
  (ldadd64 Rs Rn (bv #b11111 5)))
