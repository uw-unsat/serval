#lang rosette

; Typed memory manager with bitvector pointers.
; Memory is divided into a set of disjoint regions of concrete base and size.

(require "mblock.rkt" "mregion.rkt" "manager.rkt" "../debug.rkt" "../bvarith.rkt")

(define (get-block-and-path memmgr addr off size)
  (define regions (typed-bv-memmgr-regions memmgr))

  (define mr (guess-mregion-from-addr regions addr off))

  (define start (mregion-start mr))
  (define end (mregion-end mr))
  (define name (mregion-name mr))
  (define block (mregion-block mr))

  (set! addr (bvadd addr off))

  (define offset (bvsub addr (integer->bitvector start (type-of addr))))
  (bug-on (! (mregion-inbounds? mr addr size))
             
              #:msg (format "get-block-and-path: address out of range:\n addr: ~e\n block: ~e" addr name))

  (define path (mblock-path block offset size))
  (values block path offset))

(define (typed-bv-memmgr-memset! memmgr addr value len)
  (define mregion (guess-mregion-from-addr (typed-bv-memmgr-regions memmgr) addr (bv 0 12)))
  (bug-on (eq? mregion #f) #:msg "typed-bv-memmgr-memset!: failed to guess mregion")

  (bug-on (! (mregion-inbounds? mregion addr len))
    #:msg "typed-bv-memmgr-memset!: address out of range")

  (define offset (bvsub addr (integer->bitvector (mregion-start mregion) (type-of addr))))

  (when (! (bvzero? len))
    (mblock-memset! (mregion-block mregion) value offset len)))

(define (typed-bv-memmgr-alloc! memmgr size alignment proc)
  (bug #:msg "typed-bv memory manager cannot allocate memory!"))

(define (typed-bv-memmgr-load memmgr addr off size)
  (define-values (block path offset) (get-block-and-path memmgr addr off size))
  (mblock-iload block path))

(define (typed-bv-memmgr-store! memmgr addr off data size)
  (define-values (block path offset) (get-block-and-path memmgr addr off size))
  (mblock-istore! block data path))

(define (make-typed-bv-memmgr symbols globals)
  (typed-bv-memmgr (create-mregions symbols globals)))

(struct typed-bv-memmgr (regions) #:transparent #:mutable
  #:methods gen:memmgr
  [(define memmgr-alloc! typed-bv-memmgr-alloc!)
   (define memmgr-load typed-bv-memmgr-load)
   (define memmgr-store! typed-bv-memmgr-store!)
   (define memmgr-memset! typed-bv-memmgr-memset!)
   (define (memmgr-bitwidth b) (target-pointer-bitwidth))
   (define memmgr-invariants (lambda (mm) #t))])

(provide
  make-typed-bv-memmgr
  (struct-out typed-bv-memmgr))