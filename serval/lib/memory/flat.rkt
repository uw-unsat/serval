#lang rosette

; Flat memory model. Assumes Little endian.
; Memory is a single uninterpreted function from bv -> bv8

(provide (all-defined-out))
(require "manager.rkt" "../bvarith.rkt" "../uf.rkt" "../debug.rkt")

(define (flat-memmgr-alloc! memmgr size alignment proc #:dbg dbg)
  (bug #:dbg dbg #:msg "flat memory manager: no allocator!"))

(define (check-addr-off addr off #:dbg dbg)
  (bug-on (! (equal? (bv-size (bvadd addr off)) (target-pointer-bitwidth)))
          #:msg "flat memory model: addr size wrong"
          #:dbg dbg))

(define (flat-memmgr-store! memmgr addr off data size #:dbg dbg)
  (define N (bitvector->natural size))
  (check-addr-off addr off #:dbg dbg)
  (for ([i (in-range N)])
    (define oldf (flat-memmgr-memory memmgr))
    (define newf (lambda (p)
      (if (equal? p (bvadd addr off (bv i (type-of addr))))
        (extract (- (* 8 (+ i 1)) 1) (* 8 i) data)
        (oldf p))))
    (set-flat-memmgr-memory! memmgr newf)))

(define (flat-memmgr-load memmgr addr off size #:dbg dbg)
  (define N (bitvector->natural size))
  (check-addr-off addr off #:dbg dbg)
  (define Bytes (for/list ([i (in-range N)])
    ((flat-memmgr-memory memmgr) (bvadd addr off (bv i (type-of addr))))))

  (apply concat (reverse Bytes)))

(define (flat-memmgr-memset! memmgr addr value len #:dbg dbg)
  (bug #:dbg dbg #:msg "flat memset not implemented yet"))

(define (make-flat-memmgr)
  (define-symbolic* memory (~> (bitvector (target-pointer-bitwidth))
                               (bitvector 8)))
  (flat-memmgr memory))

(struct flat-memmgr (memory) #:transparent #:mutable
  #:methods gen:memmgr
  [(define memmgr-alloc! flat-memmgr-alloc!)
   (define memmgr-load flat-memmgr-load)
   (define memmgr-store! flat-memmgr-store!)
   (define memmgr-memset! flat-memmgr-memset!)
   (define memmgr-invariants (lambda (mm) #t))])
