#lang rosette

(require "../debug.rkt" "../bvarith.rkt" "manager.rkt")

(provide (all-defined-out))

; Mblocks are low-level primitives for building typed memory models.
; They do not specify how separate are laid out in memory.
;
; An block is either an array, a struct, or a cell.
; Recursively, an array is a list of blocks
; of the same types; a struct is a list of named fields, where each
; field may further refer to a block.  A cell is a leaf node: it is
; represented using an uninterpreted function.
;
; To read and write with an offset into a block, one first invokes
; mblock-path to convert an offset to a list of indices ("path");
; mblock-iload and mblock-istore! uses a path to access the leaf cell.
;

(define enable-struct-crunch (make-parameter (if (getenv "DISABLE_STRUCT_CRUNCH") #f #t)))

(define (bvpointer?)
  (bitvector (target-pointer-bitwidth)))

(define (bvpointer x)
  (bv x (target-pointer-bitwidth)))

(define-generics mblock
  (mblock-size mblock)
  (mblock-init! mblock name [domain])
  (mblock-fini! mblock)
  (mblock-path mblock offset size)
  (mblock-resolve mblock path [indices])
  (mblock-load mblock offset size indices)
  (mblock-store! mblock value offset indices)
  (mblock-memset! mblock c offset size [preds])
  (mblock-copy mblock))

(define (mblock-inbounds? mblock offset size)
  (define block-size (bvpointer (mblock-size mblock)))
  (&& (bvule size block-size)
      (bvule offset (bvsub block-size size))))

; marray

(define (marray-size mblock)
  (* (marray-length mblock) (mblock-size (marray-elements mblock))))

(define (marray-init! mblock name [domain null])
  (mblock-init! (marray-elements mblock)
                name
                (cons (bvpointer?) domain)))

(define (marray-fini! mblock)
  (mblock-fini! (marray-elements mblock)))

(define (marray-path mblock offset size)
  (bug-on (! (mblock-inbounds? mblock offset size))
    #:msg (format "marray-path: offset ~e not in bounds ~e" offset size))
  (define element-size (bvpointer (mblock-size (marray-elements mblock))))
  (define element-index (bvudiv offset element-size))
  (define element-offset (bvurem offset element-size))
  (cons element-index
        (mblock-path (marray-elements mblock) element-offset size)))

(define (marray-resolve mblock path [indices null])
  (mblock-resolve (marray-elements mblock)
                (cdr path)
                (cons (car path) indices)))

; For simplicity, this supports two cases only:
; - memset within one element
; - memset over multiple (full) elements
; It doesn't allow memset over partial elements.
(define (marray-memset! mblock c offset size [preds null])
  (bug-on (! (mblock-inbounds? mblock offset size))
    #:msg "marray-memset!: [offset, offset + size) not in bounds")
  (define element-size (bvpointer (mblock-size (marray-elements mblock))))
  (define element-index (bvudiv offset element-size))
  (define element-offset (bvurem offset element-size))
  (if (bvule size element-size)
      ; memset within one element
      (begin
        ; No need to check offset <= element-size - size here;
        ; if offset is too large, the next level will fail.

        ; predicate: idx == element-index
        (define pred (lambda (idx) (equal? idx element-index)))
        (mblock-memset! (marray-elements mblock) c element-offset size
                        (cons pred preds)))
      ; memset across multiple full elements
      (begin
        ; alignment checks for offset and size
        (bug-on (! (bvzero? element-offset))
                #:msg "marray-memset!: offset not aligned")
        (bug-on (! (bvzero? (bvurem size element-size)))
                #:msg "marray-memset!: size not aligned")
        ; predicate: element-index <= idx < element-index + size / element-size
        (define pred (lambda (idx)
                         (&& (bvule element-index idx)
                             (bvult idx (bvadd element-index (bvudiv size element-size))))))
        (mblock-memset! (marray-elements mblock) c (bvpointer 0) element-size
                        (cons pred preds)))))

(define (marray-copy mblock)
  (struct-copy marray mblock
    [elements (mblock-copy (marray-elements mblock))]))

(struct marray (length elements) #:transparent
  #:methods gen:mblock
  [(define mblock-size marray-size)
   (define mblock-init! marray-init!)
   (define mblock-fini! marray-fini!)
   (define mblock-path marray-path)
   (define mblock-resolve marray-resolve)
   ; mblock-load
   ; mblock-store!
   (define mblock-memset! marray-memset!)
   (define mblock-copy marray-copy)])

; mstruct

(struct mfield (name offset element) #:transparent)

(define (mstruct-init! mblock name [domain null])
   (for-each (lambda (f)
               (mblock-init! (mfield-element f) (append name (list (mfield-name f))) domain))
             (mstruct-fields mblock)))

(define (mstruct-fini! mblock)
   (for-each (lambda (f) (mblock-fini! (mfield-element f)))
             (mstruct-fields mblock)))

; Symbolic offsets inside structs will kill Z3 in @findf.
; Instead, we do this ugly hack to patch the symbolic offset
; in the form (urem (add x (mul y z)) y) into just x.
(define (crunch-struct-offset offset)
  (define (kill-bvmul expr const)
    (match expr
      [(expression (== bvadd) x y)
       (let ([subexpr (kill-bvmul y const)])
         (if subexpr (bvadd x subexpr) #f))]

      [(expression (== bvmul) x y) #:when (and (! (term? x)) (bveq const x))
        (bv 0 (target-pointer-bitwidth))]

      [(expression (== bvshl) x y) #:when (and (! (term? y)) (bveq const (bvshl (bv 1 (target-pointer-bitwidth)) y)))
        (bv 0 (target-pointer-bitwidth))]

      [_ #f]))

  (if (enable-struct-crunch)
    (match offset
      [(expression (== bvurem) x y) #:when (! (term? y))
        (let ([subexpr (kill-bvmul x y)])
          (if subexpr subexpr offset))]
      [any any])
    offset))

(define (mstruct-field-by-name mblock name)
  (findf (lambda (x) (equal? (mfield-name x) name)) (mstruct-fields mblock)))

(define (mstruct-field-by-offset mblock offset)
  (define fields (map (lambda (x) (mfield (mfield-name x) (bvpointer (mfield-offset x)) (mfield-element x)))
                      (reverse (mstruct-fields mblock))))
  (findf (lambda (x) (bvuge offset (mfield-offset x))) fields))

(define (mstruct-path mblock offset size)
  (define offset2 (crunch-struct-offset offset))
  (bug-on (not (bveq offset offset2)) #:msg "mstruct-path: rewriting failed")

  (bug-on (not (mblock-inbounds? mblock offset2 size))
    #:msg "mstruct-path: offset not in bounds")

  (define f (mstruct-field-by-offset mblock offset2))
  (cons (mfield-name f)
        (mblock-path (mfield-element f) (bvsub offset2 (mfield-offset f)) size)))

(define (mstruct-resolve mblock path [indices null])
  (define f (mstruct-field-by-name mblock (car path)))
  (mblock-resolve (mfield-element f) (cdr path) indices))

; For simplicity, this supports two cases only:
; - memset within a single field
; - memset over the entire struct
; It doesn't allow memset over partial fields.
(define (mstruct-memset! mblock c offset size [preds null])
  (define offset2 (crunch-struct-offset offset))
  (bug-on (! (bveq offset offset2)) #:msg "mstruct-memset!: rewriting failed")

  (bug-on (! (mblock-inbounds? mblock offset2 size))
    #:msg "mstruct-memset!: [offset, offset + size) not in bounds")

  (define block-size (bvpointer (mblock-size mblock)))

  (if (equal? size block-size)
      ; memset over the entire struct
      (begin
        ; offset must be zero given the earlier bounds check
        ; recusrive memset over each element
        (define (recur f)
          (define element (mfield-element f))
          (mblock-memset! element c (bvpointer 0) (bvpointer (mblock-size element))
                          preds))
        (for-each recur (mstruct-fields mblock)))
      ; memset within one element
      (begin
        ; no need to check size: the next level will check
        (define f (mstruct-field-by-offset mblock offset2))
        (mblock-memset! (mfield-element f) c (bvsub offset2 (mfield-offset f)) size
                          preds))))

(define (mstruct-copy mblock)
  (define (copy-field f)
    (struct-copy mfield f [element (mblock-copy (mfield-element f))]))
  (struct-copy mstruct mblock
    [fields (map copy-field (mstruct-fields mblock))]))

(struct mstruct (size fields) #:transparent
  #:methods gen:mblock
  [(define (mblock-size mblock) (mstruct-size mblock))
   (define mblock-init! mstruct-init!)
   (define mblock-fini! mstruct-fini!)
   (define mblock-path mstruct-path)
   (define mblock-resolve mstruct-resolve)
   ; mblock-load
   ; mblock-store!
   (define mblock-memset! mstruct-memset!)
   (define mblock-copy mstruct-copy)])

; mcell

(define (mcell-init! mblock name [domain null])
  ; name should be unique, though we use define-symbolc*
  ; to make a unique name just to be safe
  (define-symbolic* mcell boolean?)
  (define sid (append name (list mcell)))
  ; convert from bytes to bits
  (define range (bitvector (* 8 (mcell-size mblock))))
  (define f (if (empty? domain)
                (lambda () (constant sid range))
                (constant sid (apply ~> (append domain (list range))))))
  (set-mcell-func! mblock f))

(define (mcell-fini! mblock)
  (set-mcell-free! mblock #t))

; returns a pair (offset, size) in the end for load/store to
; extract a smaller value
(define (mcell-path mblock offset size)
  (define block-size (bvpointer (mblock-size mblock)))
  ; block size must be a multiple of size - this check should be constant
  (bug-on (! (bvzero? (bvurem block-size size)))
          #:msg (format "mcell-path: block size ~a must be a multiple of size ~a\n" block-size size))
  ; offset must be a multiple of size
  (bug-on (! (bvzero? (bvurem offset size)))
          #:msg (format "mcell-path: offset ~a must be a multiple of size ~a\n" offset size))
  ; offset must be smaller than block size
  (bug-on (! (bvult offset block-size))
          #:msg (format "mcell-path: offset ~a must be smaller than block size ~a\n" offset block-size))
  (cons offset size))

(define (mcell-resolve mblock path [indices null])
  (if (empty? path)
      ; old-style path - insert extra offset and size
      (list mblock (bvpointer 0) (bvpointer (mblock-size mblock)) indices)
      ; new-style path - (offset, size) is the last element
      (list mblock (car path) (cdr path) indices)))

(define (mcell-load mblock offset size indices)
  (bug-on (mcell-free mblock) #:msg "mcell-load: use after free\n")
  (define f (mcell-func mblock))
  (apply f indices))

(define (mcell-store! mblock value offset indices)
  (bug-on (mcell-free mblock) #:msg "mcell-store!: use after free\n")
  (define oldf (mcell-func mblock))
  (define newf (lambda args
                 (if (andmap equal? args indices) value (apply oldf args))))
  (set-mcell-func! mblock newf))

(define (mcell-memset! mblock c offset size [preds null])
  (bug-on (! (bvzero? offset))
          #:msg "mcell-memset!: offset non-zero")
  (bug-on (! (equal? (bv-size c) 8))
          #:msg "mcell-memset!: not (bitvector 8)")
  ; not required: just leave it here for sanity checks
  (bug-on (! (bvzero? c))
          #:msg "mcell-memset!: must be 0")
  (unless (bvzero? size)
    (bug-on (! (equal? size (bvpointer (mcell-size mblock))))
            #:msg "mcell-memset!: size incorrect")
    (define value (apply concat (make-list (mcell-size mblock) c)))
    (define oldf (mcell-func mblock))
    (define newf (lambda args
                   (if (andmap (lambda (f x) (f x)) preds args)
                       value
                       (apply oldf args))))
    (set-mcell-func! mblock newf)))

(define (mcell-copy mblock)
  (define c (mcell (mcell-size mblock)))
  (set-mcell-func! c (mcell-func mblock))
  (set-mcell-free! c (mcell-free mblock))
  c)

; free is used only by stack memory; it is set to #t when the lifetime ends.
(struct mcell (size [func #:auto #:mutable] [free #:auto #:mutable]) #:transparent
  #:methods gen:mblock
  [(define (mblock-size mblock) (mcell-size mblock))
   (define mblock-init! mcell-init!)
   (define mblock-fini! mcell-fini!)
   (define mblock-path mcell-path)
   (define mblock-resolve mcell-resolve)
   (define mblock-load mcell-load)
   (define mblock-store! mcell-store!)
   (define mblock-memset! mcell-memset!)
   (define mblock-copy mcell-copy)])

; mblock utility operations

; iload and istore! are implemented using memblock-resolve, which returns
; (mblock, offset, size, indices).  The last component is used for UFs in
; mcell, a "reverse" path without struct indices.

(define (mblock-iload mblock path)
  (match (mblock-resolve mblock path)
    [(list leaf offset size indices) (mblock-load leaf offset size indices)]
    [any (bug #:msg (format "mblock-iload: invalid result from mblock-resolve: ~a\n" any))]))

(define (mblock-istore! mblock value path)
  (match (mblock-resolve mblock path)
    [(list leaf offset size indices) (mblock-store! leaf value offset indices)]
    [any (bug #:msg (format "mblock-istore!: invalid result from mblock-resolve: ~a\n" any))]))
