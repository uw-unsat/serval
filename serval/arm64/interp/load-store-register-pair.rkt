#lang rosette

(require
  "common.rkt")

(provide
  stp-preindex
  ldp-postindex)

(define (decode-stp opc imm7 Rt2 Rn Rt)
  (define n Rn)
  (define t Rt)
  (define t2 Rt2)
  (define scale
    (cond
      [(equal? opc (bv #b00 2)) (bv 2 64)]
      [(equal? opc (bv #b10 2)) (bv 3 64)]
      [else (undefined)]))
  (define datasize (bvshl (bv 8 64) scale))
  (define offset (bvshl (sign-extend imm7 (bitvector 64)) scale))
  (values n t t2 scale datasize offset))

(define ((interpret-stp wback postindex) cpu opc imm7 Rt2 Rn Rt)
  (define-values (n t t2 scale datasize offset) (decode-stp opc imm7 Rt2 Rn Rt))
  (define mm (cpu-memmgr cpu))

  (define dbytes (bvudiv datasize (bv 8 64)))

  (when (and wback (or (equal? t n) (equal? t2 n)) (not (equal? n (integer->gpr 31))))
    (undefined))

  (define address #f)
  (cond
    [(equal? n (integer->gpr 31))
      (check-sp-alignment cpu)
      (set! address (cpu-sp-ref cpu))]
    [else
      (set! address (cpu-gpr-ref cpu n))])

  (when (! postindex)
    (set! address (bvadd address offset)))

  (define data1 (cpu-gpr-ref cpu t))
  (define data2 (cpu-gpr-ref cpu t2))

  (core:memmgr-store! mm address (bv 0 64) data1 dbytes)
  (core:memmgr-store! mm (bvadd address dbytes) (bv 0 64) data2 dbytes)

  (when wback
    (when postindex
      (set! address (bvadd address offset)))
    (if (equal? n (integer->gpr 31))
        (cpu-sp-set! cpu address)
        (cpu-gpr-set! cpu n address)))

  (void))

(define (decode-ldp wback opc imm7 Rt2 Rn Rt)
  (define n Rn)
  (define t Rt)
  (define t2 Rt2)

  (define signed (! (equal? (bit 0 opc) (bv 0 1))))

  (define scale
    (cond
      [(equal? opc (bv #b00 2)) (bv 2 64)]
      [(equal? opc (bv #b10 2)) (bv 3 64)]
      [else (undefined)]))
  (define datasize (bvshl (bv 8 64) scale))
  (define offset (bvshl (sign-extend imm7 (bitvector 64)) scale))
  (define tag_checked
    (|| wback (! (equal? n (integer->gpr 31)))))

  (values n t t2 signed scale datasize offset tag_checked))

(define ((interpret-ldp wback postindex) cpu opc imm7 Rt2 Rn Rt)
  (define-values (n t t2 signed scale datasize offset tag_checked)
                 (decode-ldp wback opc imm7 Rt2 Rn Rt))
  (define mm (cpu-memmgr cpu))

  (define dbytes (bvudiv datasize (bv 8 64)))

  (when (&& wback (|| (equal? t n) (equal? t2 n)) (! (equal? n (integer->gpr 31))))
    (undefined))

  (when (equal? t t2)
    (undefined))

  (define address #f)
  (cond
    [(equal? n (integer->gpr 31))
      (check-sp-alignment cpu)
      (set! address (cpu-sp-ref cpu))]
    [else
      (set! address (cpu-gpr-ref cpu n))])

  (when (! postindex)
    (set! address (bvadd address offset)))

  (define data1 (core:memmgr-load mm address (bv 0 64) dbytes))
  (define data2 (core:memmgr-load mm (bvadd address dbytes) (bv 0 64) dbytes))

  (cond
    [signed
      (cpu-gpr-set! cpu t (sign-extend data1 (bitvector 64)))
      (cpu-gpr-set! cpu t2 (sign-extend data2 (bitvector 64)))]
    [else
      (cpu-gpr-set! cpu t (zero-extend data1 (bitvector 64)))
      (cpu-gpr-set! cpu t2 (zero-extend data2 (bitvector 64)))])

  (when wback
    (when postindex
      (set! address (bvadd address offset)))
    (cond
      [(equal? n (integer->gpr 31))
        (cpu-sp-set! cpu address)]
      [else
        (cpu-gpr-set! cpu n address)]))

  (void))

(define-insn (opc imm7 Rt2 Rn Rt)
  #:encode (lambda (V opc2 L) (list opc (bv #b101 3) (bv V 1) (bv opc2 3) (bv L 1) imm7 Rt2 Rn Rt))
  [(#b0 #b001 #b1) ldp-postindex (interpret-ldp #t #t)]
  [(#b0 #b011 #b0) stp-preindex  (interpret-stp #t #f)])
