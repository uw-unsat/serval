#lang rosette

(require
  "common.rkt")

(provide
  ldadd32 ldadd64
  stadd32 stadd64)


(define (decode size A R Rs Rn Rt)
  (define t Rt)
  (define n Rn)
  (define s Rs)

  (define datasize (arithmetic-shift 8 (bitvector->natural size)))
  (define regsize (if (equal? datasize 64) 64 32))
  (define ldacctype (if (&& (bveq A (bv #b1 1)) (! (equal? Rt (integer->gpr #b11111)))) 'ORDEREDATOMICRW 'ATOMICRW))
  (define stacctype (if (bveq R (bv #b1 1)) 'ORDEREDATOMICRW 'ATOMICRW))
  (define tag_checked (! (equal? n (integer->gpr 31))))

  (values t n s datasize regsize ldacctype stacctype tag_checked))


(define (interpret-ldadd* cpu size A R Rs Rn Rt)
  (define-values (t n s datasize regsize ldacctype stacctype tag_checked) (decode size A R Rs Rn Rt))
  (define value (trunc datasize (cpu-gpr-ref cpu s)))
  (define address
    (if (equal? n (integer->gpr 31))
        (begin
          (check-sp-alignment cpu)
          (cpu-sp-ref cpu))
        (cpu-gpr-ref cpu n)))

  (define data (mem-atomic cpu address 'ADD value ldacctype stacctype))
  (unless (equal? t (integer->gpr 31))
    (cpu-gpr-set! cpu t (zero-extend data (bitvector regsize)))))


(define-insn (size A R Rs Rn Rt)
  #:encode (lambda (V o3 opc) (list size (bv #b111 3) (bv V 1) (bv #b00 2) A R (bv #b1 1) Rs (bv o3 1) (bv opc 3) (bv #b00 2) Rn Rt))
  [(#b0 #b0 #b000) ldadd* interpret-ldadd*])

(define (ldadd32 Rs Rn Rt)
  (ldadd* (bv #b10 2) (bv #b0 1) (bv #b0 1) Rs Rn Rt))

(define (ldadd64 Rs Rn Rt)
  (ldadd* (bv #b11 2) (bv #b0 1) (bv #b0 1) Rs Rn Rt))

(define (stadd32 Rs Rn)
  (ldadd32 Rs Rn (integer->gpr #b11111)))

(define (stadd64 Rs Rn)
  (ldadd64 Rs Rn (integer->gpr #b11111)))
