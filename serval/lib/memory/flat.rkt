#lang rosette

; Flat memory model. Assumes Little endian.
; Memory is a single uninterpreted function from bv -> bv8

(provide make-flat-memmgr (struct-out flat-memmgr))

(require "manager.rkt" "../bvarith.rkt" "../uf.rkt" "../debug.rkt")

(define (check-addr-off addr off)
  (bug-on (! (equal? (bv-size (bvadd addr off)) (target-pointer-bitwidth)))
          #:msg "flat memory model: addr size wrong"))

(define (flat-memmgr-store! memmgr addr off data size)
  (define N (bitvector->natural size))
  (check-addr-off addr off)
  (for ([i (in-range N)])
    (define oldf (flat-memmgr-memory memmgr))
    (define newf (lambda (p)
      (if (equal? p (bvadd addr off (bv i (type-of addr))))
        (extract (- (* 8 (+ i 1)) 1) (* 8 i) data)
        (oldf p))))
    (set-flat-memmgr-memory! memmgr newf)))

(define (flat-memmgr-load memmgr addr off size)
  (define N (bitvector->natural size))
  (check-addr-off addr off)
  (define Bytes (for/list ([i (in-range N)])
    ((flat-memmgr-memory memmgr) (bvadd addr off (bv i (type-of addr))))))

  (apply concat (reverse Bytes)))

(define (make-flat-memmgr #:bitwidth [bitwidth #f])
  (when (equal? bitwidth #f)
    (set! bitwidth (target-pointer-bitwidth)))

  (define-symbolic* memory (~> (bitvector (target-pointer-bitwidth))
                               (bitvector 8)))
  (flat-memmgr memory bitwidth))

(struct flat-memmgr (memory bitwidth) #:transparent #:mutable
  #:methods gen:memmgr
  [(define memmgr-load flat-memmgr-load)
   (define memmgr-store! flat-memmgr-store!)
   (define (memmgr-bitwidth m) (flat-memmgr-bitwidth m))
   (define memmgr-invariants (lambda (mm) #t))])
