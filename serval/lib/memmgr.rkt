#lang rosette

(require "core.rkt" "debug.rkt")

(provide (all-defined-out))

(define-generics memmgr
  (memmgr-alloc! memmgr size alignment proc #:dbg dbg)
  (memmgr-load memmgr addr off size #:dbg dbg)
  (memmgr-store! memmgr addr off data size #:dbg dbg)
  (memmgr-memset! memmgr addr value len #:dbg dbg)
  (memmgr-invariants memmgr))

(define (get-block-and-path memmgr addr off size #:dbg dbg)
  (define regions (default-memmgr-regions memmgr))

  (define mr (guess-mregion-from-addr regions addr off #:dbg dbg))

  (define start (mregion-start mr))
  (define end (mregion-end mr))
  (define name (mregion-name mr))
  (define block (mregion-block mr))

  (set! addr (bvadd addr off))

  (define offset (bvsub addr (integer->bitvector start (type-of addr))))
  (bug-on (! (mregion-inbounds? mr addr size))
              #:dbg dbg
              #:msg (format "get-block-and-path: address out of range:\n addr: ~e\n block: ~e" addr name))

  (define path (mblock-path block offset size #:dbg dbg))
  (values block path offset))

(define (default-memmgr-memset! memmgr addr value len #:dbg dbg)
  (define mregion (guess-mregion-from-addr (default-memmgr-regions memmgr) addr (bv 0 12) #:dbg dbg))
  (bug-on (eq? mregion #f) #:msg "default-memmgr-memset!: failed to guess mregion" #:dbg dbg)

  (bug-on (! (mregion-inbounds? mregion addr len))
    #:dbg dbg #:msg "default-memmgr-memset!: address out of range")

  (define offset (bvsub addr (integer->bitvector (mregion-start mregion) (type-of addr))))

  (when (! (bvzero? len))
    (mblock-memset! (mregion-block mregion) value offset len #:dbg dbg)))

(define (default-memmgr-alloc! memmgr size alignment proc #:dbg dbg)
  (bug #:dbg dbg #:msg "Default memory manager cannot allocate memory!"))

(define (default-memmgr-load memmgr addr off size #:dbg dbg)
  (define-values (block path offset) (get-block-and-path memmgr addr off size #:dbg dbg))
  (mblock-iload block path))

(define (default-memmgr-store! memmgr addr off data size #:dbg dbg)
  (define-values (block path offset) (get-block-and-path memmgr addr off size #:dbg dbg))
  (mblock-istore! block data path))

(define (make-default-memmgr symbols globals)
  (default-memmgr (create-mregions symbols globals)))

(struct default-memmgr (regions) #:transparent #:mutable
  #:methods gen:memmgr
  [(define memmgr-alloc! default-memmgr-alloc!)
   (define memmgr-load default-memmgr-load)
   (define memmgr-store! default-memmgr-store!)
   (define memmgr-memset! default-memmgr-memset!)
   (define memmgr-invariants (lambda (mm) #t))])
