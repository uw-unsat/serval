#lang rosette

(require rosette/solver/smt/z3
         "debug.rkt"
         "unittest.rkt"
         (prefix-in racket: racket/base)
         (only-in rosette [list var]))

(provide (all-defined-out) var (all-from-out "debug.rkt"))

; solver setup

(current-solver (z3 #:options (hash ':auto-config 'false ':smt.relevancy 0)))

; parameters

(define target-endian (make-parameter 'little))
(define target-pointer-bitwidth (make-parameter 64))
(define enable-struct-crunch (make-parameter (if (getenv "DISABLE_STRUCT_CRUNCH") #f #t)))

; case splitting

(define (split-cases value cases func)
  (define newvalue
    (foldr (lambda (a result) (if (equal? a value) a result)) value cases))
  (for/all ([v newvalue #:exhaustive]) (begin
    (check-unsat? (verify (assert (equal? v value))))
    (func v))))

(define-syntax (split-pc stx)
  (define (build-name id . parts)
    (datum->syntax id
      (string->symbol
        (apply string-append
               (map (lambda (p)
                      (if (syntax? p)
                          (symbol->string (syntax-e p))
                          p))
                    parts)))
      id))
  (syntax-case stx ()
    [(_ (struct-name field) obj body ...)
      (with-syntax ([getter (build-name #'obj #'struct-name "-" #'field)]
                    [setter! (build-name #'obj "set-" #'struct-name "-" #'field "!")])
        (syntax/loc stx
          (for/all ([pc (getter obj) #:exhaustive])
            (begin
              (setter! obj pc)
              body ...))))]))


; primitive types

(define i8 (bitvector 8))
(define i16 (bitvector 16))
(define i32 (bitvector 32))
(define i64 (bitvector 64))

(define (make-arg type)
  (define-symbolic* symbolic-arg type)
  symbolic-arg)

(define (make-bv8)
  (make-arg (bitvector 8)))

(define (make-bv64)
  (make-arg (bitvector 64)))

(define (aligned? x alignment)
  (zero? (remainder x alignment)))

(define (bv-size x)
  (bitvector-size (type-of x)))

(define (msb x)
  (let ([pos (sub1 (bv-size x))])
    (extract pos pos x)))

(define (lsb x)
  (extract 0 0 x))

(define bv-sign msb)

(define (bvzero? x)
  (bveq x (bv 0 (type-of x))))

(define (bvsmax t)
  (bv (sub1 (arithmetic-shift 1 (sub1 (bitvector-size t)))) t))

(define (bvsmax? x)
  (bveq x (bvsmax (type-of x))))

(define (bvsmin t)
  (bv (- (arithmetic-shift 1 (sub1 (bitvector-size t)))) t))

(define (bvsmin? x)
  (bveq x (bvsmin (type-of x))))

(define (bvadd1 x)
  (bvadd x (bv 1 (type-of x))))

(define (bvsub1 x)
  (bvsub x (bv 1 (type-of x))))

(define (bool->bitvector x)
  (if x (bv 1 1) (bv 0 1)))

(define (bitvector->bool x)
  (not (bvzero? x)))

; list of (bitvector 8) -> (bitvector N), big endian
(define (list->bitvector/be data)
  (apply concat data))

; (bitvector N) -> list of (bitvector 8), big endian
(define (bitvector->list/be x)
  (define n (bv-size x))
  (if (= n 8)
      (list x)
      (cons (extract (sub1 n) (- n 8) x)
            (bitvector->list/be (extract (- n 9) 0 x)))))

; list of (bitvector 8) -> (bitvector N), little endian
(define (list->bitvector/le data)
  (list->bitvector/be (reverse data)))

; (bitvector N) -> list of (bitvector 8), little endian
(define (bitvector->list/le x)
  (reverse (bitvector->list/be x)))

(define (bv8 x)
  (bv x 8))

(define (bvpointer?)
  (bitvector (target-pointer-bitwidth)))

(define (bvpointer x)
  (bv x (target-pointer-bitwidth)))

(define (bvaligned? x alignment)
  (bvzero? (bvurem x (bvpointer alignment))))


; bitvector overflow detection, following the enoding in MSR-TR-2009-57:
;   Modular Bug-finding for Integer Overflows in the Large:
;   Sound, Efficient, Bit-precise Static Analysis

(define (bvsadd-overflow? x y [carry #f])
  (define n (bv-size x))
  (define result (bvadd x y (if carry (bv 1 n) (bv 0 n))))
  (&& (bveq (bv-sign x) (bv-sign y))
      (! (bveq (bv-sign x) (bv-sign result)))))

(define (bvuadd-overflow? x y [carry #f])
  (define n (bv-size x))
  (bveq (bv 1 1)
        (bv-sign (bvadd (zero-extend x (bitvector (add1 n)))
                        (zero-extend y (bitvector (add1 n)))
                        (if carry (bv 1 (add1 n)) (bv 0 (add1 n)))))))

(define (bvssub-overflow? x y [borrow #f])
  (define n (bv-size x))
  (define result (bvsub x y (if borrow (bv 1 n) (bv 0 n))))
  (&& (! (bveq (bv-sign x) (bv-sign y)))
      (! (bveq (bv-sign x) (bv-sign result)))))

(define (bvusub-overflow? x y [borrow #f])
  (if borrow (bvule x y) (bvult x y)))

; signed multiplication overflow:
; multiply x and y sign-extended to 2N bits and check whether
; the result fits in N bits.
(define (bvsmul-overflow? x y)
  (let [(n (bv-size x))]
    (! (bveq
      (if (bveq (bv-sign x) (bv-sign y))
          (bv 0 (add1 n))
          (bv -1 (add1 n)))
      (extract (sub1 (* n 2)) (sub1 n)
        (bvmul (sign-extend x (bitvector (* n 2)))
               (sign-extend y (bitvector (* n 2)))))))))

; unsigned multiplication overflow:
; multiply x and y zero-extended to 2N bits and check whether
; the result fits in N bits.
(define (bvumul-overflow? x y)
  (let [(n (bv-size x))]
    (! (bvzero?
      (extract (sub1 (* n 2)) n
        (bvmul (zero-extend x (bitvector (* n 2)))
               (zero-extend y (bitvector (* n 2)))))))))

(define (bvsdiv-overflow? x y)
  (&& (bvsmin? x) (bveq y (bv -1 (type-of y)))))

; update a function map with a path, which can be
; an index, a predicate, or a list of indices/predicates.
(define (update func path value)
  (define (normalize-path e)
    (if (procedure? e) e (lambda (x) (equal? x e))))
  (define pred (map normalize-path (if (list? path) path (list path))))
  (lambda args (if (apply && (map (lambda (f x) (f x)) pred args)) value (apply func args))))

(define (update-fn func indices newfn)
  (define (index-yes? args indices)
    (apply && (for/list ([x args] [y indices]) (equal? x y))))
  (define proc (if (list? indices) (lambda args (index-yes? args indices)) indices))
  (lambda args (if (apply proc args) (apply newfn args) (apply func args))))

; memory

; Memory is modeled as a list of blocks. Each block may be an array,
; a struct, or a cell.  Recursively, an array is a list of blocks
; of the same types; a struct is a list of named fields, where each
; field may further refer to a block.  A cell is a leaf node: it is
; represented using an uninterpreted function.
;
; To read and write with an offset into a block, one first invokes
; mblock-path to convert an offset to a list of indices ("path");
; mblock-iload and mblock-istore! uses a path to access the leaf cell.

(define-generics mblock
  (mblock-size mblock)
  (mblock-init! mblock name [domain])
  (mblock-fini! mblock)
  (mblock-path mblock offset size #:dbg dbg)
  (mblock-resolve mblock path [indices])
  (mblock-load mblock offset size indices)
  (mblock-store! mblock value offset indices)
  (mblock-memset! mblock c offset size [preds] #:dbg dbg)
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

(define (marray-path mblock offset size #:dbg dbg)
  (bug-on (not (mblock-inbounds? mblock offset size))
    #:msg (format "marray-path: offset ~e not in bounds ~e" offset size) #:dbg dbg)
  (define element-size (bvpointer (mblock-size (marray-elements mblock))))
  (define element-index (bvudiv offset element-size))
  (define element-offset (bvurem offset element-size))
  (cons element-index
        (mblock-path (marray-elements mblock) element-offset size #:dbg dbg)))

(define (marray-resolve mblock path [indices null])
  (mblock-resolve (marray-elements mblock)
                (cdr path)
                (cons (car path) indices)))

; For simplicity, this supports two cases only:
; - memset within one element
; - memset over multiple (full) elements
; It doesn't allow memset over partial elements.
(define (marray-memset! mblock c offset size [preds null] #:dbg dbg)
  (bug-on (not (mblock-inbounds? mblock offset size)) #:dbg dbg
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
                        (cons pred preds) #:dbg dbg))
      ; memset across multiple full elements
      (begin
        ; alignment checks for offset and size
        (bug-on (not (bvzero? element-offset)) #:dbg dbg
                #:msg "marray-memset!: offset not aligned")
        (bug-on (not (bvzero? (bvurem size element-size))) #:dbg dbg
                #:msg "marray-memset!: size not aligned")
        ; predicate: element-index <= idx < element-index + size / element-size
        (define pred (lambda (idx)
                         (&& (bvule element-index idx)
                             (bvult idx (bvadd element-index (bvudiv size element-size))))))
        (mblock-memset! (marray-elements mblock) c (bvpointer 0) element-size
                        (cons pred preds) #:dbg dbg))))

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

      [(expression (== bvmul) x y) #:when (and (not (term? x)) (bveq const x))
        (bv 0 (target-pointer-bitwidth))]

      [(expression (== bvshl) x y) #:when (and (not (term? y)) (bveq const (bvshl (bv 1 (target-pointer-bitwidth)) y)))
        (bv 0 (target-pointer-bitwidth))]

      [_ #f]))

  (if (enable-struct-crunch)
    (match offset
      [(expression (== bvurem) x y) #:when (not (term? y))
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

(define (mstruct-path mblock offset size #:dbg dbg)
  (define offset2 (crunch-struct-offset offset))
  (bug-on (not (bveq offset offset2)) #:dbg dbg #:msg "mstruct-path: rewriting failed")

  (bug-on (not (mblock-inbounds? mblock offset2 size))
    #:dbg dbg #:msg "mstruct-path: offset not in bounds")

  (define f (mstruct-field-by-offset mblock offset2))
  (cons (mfield-name f)
        (mblock-path (mfield-element f) (bvsub offset2 (mfield-offset f)) size #:dbg dbg)))


(define (mstruct-resolve mblock path [indices null])
  (define f (mstruct-field-by-name mblock (car path)))
  (mblock-resolve (mfield-element f) (cdr path) indices))


; For simplicity, this supports two cases only:
; - memset within a single field
; - memset over the entire struct
; It doesn't allow memset over partial fields.
(define (mstruct-memset! mblock c offset size [preds null] #:dbg dbg)
  (define offset2 (crunch-struct-offset offset))
  (bug-on (not (bveq offset offset2)) #:dbg dbg #:msg "mstruct-memset!: rewriting failed")

  (bug-on (not (mblock-inbounds? mblock offset2 size)) #:dbg dbg
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
                          preds #:dbg dbg))
        (for-each recur (mstruct-fields mblock)))
      ; memset within one element
      (begin
        ; no need to check size: the next level will check
        (define f (mstruct-field-by-offset mblock offset2))
        (mblock-memset! (mfield-element f) c (bvsub offset2 (mfield-offset f)) size
                          preds #:dbg dbg))))

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
(define (mcell-path mblock offset size #:dbg dbg)
  (define block-size (bvpointer (mblock-size mblock)))
  ; block size must be a multiple of size - this check should be constant
  (bug-on (not (bvzero? (bvurem block-size size))) #:dbg dbg
          #:msg (format "mcell-path: block size ~a must be a multiple of size ~a\n" block-size size))
  ; offset must be a multiple of size
  (bug-on (not (bvzero? (bvurem offset size))) #:dbg dbg
          #:msg (format "mcell-path: offset ~a must be a multiple of size ~a\n" offset size))
  ; offset must be smaller than block size
  (bug-on (not (bvult offset block-size)) #:dbg dbg
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

(define (mcell-memset! mblock c offset size [preds null] #:dbg dbg)
  (bug-on (not (bvzero? offset))
          #:msg "mcell-memset!: offset non-zero" #:dbg dbg)
  (bug-on (not (equal? (bv-size c) 8))
          #:msg "mcell-memset!: not (bitvector 8)" #:dbg dbg)
  ; not required: just leave it here for sanity checks
  (bug-on (not (bvzero? c))
          #:msg "mcell-memset!: must be 0" #:dbg dbg)
  (unless (bvzero? size)
    (bug-on (not (equal? size (bvpointer (mcell-size mblock))))
          #:msg "mcell-memset!: size incorrect" #:dbg dbg)
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
    [any (bug-on #t #:msg (format "mblock-iload: invalid result from mblock-resolve: ~a\n" any))]))

(define (mblock-istore! mblock value path)
  (match (mblock-resolve mblock path)
    [(list leaf offset size indices) (mblock-store! leaf value offset indices)]
    [any (bug-on #t #:msg (format "mblock-istore!: invalid result from mblock-resolve: ~a\n" any))]))


; An mregion is an mblock with information about its
; location in memory, since pointers are just integers
(struct mregion (start end name block) #:transparent)

(define (mregion-copy mr)
  (struct-copy mregion mr
    [block (mblock-copy (mregion-block mr))]))

(define (create-mregions symbols globals)
  (for/list ([entry symbols] #:when (member (list-ref entry 2) (list 'B 'R)))
    (match entry
      [(list start end _ name)
        (define block
          (if (and (not (null? globals)) (hash-has-key? globals name))
              ((hash-ref globals name))
              (marray (- end start) (mcell 1))))
        (bug-on (< end start) #:msg (format "create-mregions: end < start: ~e" name))
        (bug-on (< (- end start) (mblock-size block)) #:msg "create-mregions: not enough size for global")
        (mblock-init! block (list name))
        (mregion start end name block)]
      [_ (bug-on #t #:msg (format "create-mregions: bad symbol format: ~e" entry))])))

; Find an symbol by its name
(define (find-symbol-by-name ls name)
  (findf (match-lambda [(list _ _ _ nm) (equal? nm name)]) ls))

; Find an mregion by its name
(define (find-mregion-by-name ls name #:dbg [dbg #f])
  (define result (findf (lambda (mr) (equal? name (mregion-name mr))) ls))
  (bug-on (equal? result #f) #:msg (format "find-mregion-by-name: No such mregion ~e" name) #:dbg dbg)
  result)

(define (find-block-by-name mregions name)
  (mregion-block (find-mregion-by-name mregions name)))

(define (find-mregion-by-addr ls addr #:dbg [dbg #f])
  (bug-on (term? addr) #:msg "find-mregion-by-addr: symbolic addr" #:dbg dbg)
  (findf (lambda (mr)
    (&& (bvule (bvpointer (mregion-start mr)) addr)
        (bvult addr (bvpointer (mregion-end mr)))))
    ls))

; Guess which mregion corressponds to a given address.
(define (guess-mregion-from-addr ls addr off #:dbg [dbg #f])
  (match addr

    ; If it's a constant, we don't know where it could point
    [(constant _ _) #f]

    ; If it's an expression, unpack both and see what happens
    [(expression (== bvadd) x y)
      (let ([xm (guess-mregion-from-addr ls x off)])
        (if xm xm
          (let ([ym (guess-mregion-from-addr ls y off)])
            (if ym ym #f))))]

    ; Any term not caught above (including shift)
    [(term _ _ ) #f]

    [(? bv? literal)
      (let ([x (find-mregion-by-addr #:dbg dbg ls (bvadd (sign-extend off (bitvector (target-pointer-bitwidth))) literal))])
        (if x x
          (find-mregion-by-addr #:dbg dbg ls literal)))]

    [_ (bug-on #t #:dbg dbg #:msg "guess-mregion-from-addr: unknown term")]))

; check if [addr, add + size) falls in region mr
(define (mregion-inbounds? mr addr size)
  (&& (bvule (bvpointer (mregion-start mr)) addr)
      (bvule addr (bvsub (bvpointer (mregion-end mr)) size))))

; Find the first symbol that overlaps
; with a previous symbol. Assume symbol names
; are sorted by address by nm.
(define (find-overlapping-symbol symbols)
  (define (func symbols prevend)
    (match symbols
      [(cons (list start end _ name) syms)
       (cond
         ; ignore empty entries
         [(equal? start end) (func syms prevend)]
         ; overlap
         [(< start prevend) name]
         ; continue
         [else (func syms end)])]
      [_ #f]))

  (func symbols 0))


; pointer

; A pointer is a pair (base, offset).  There are two types of pointers:
; a global pointer (where base is a symbol) and a stack pointer (where
; base is an mblock).
(struct pointer (base offset) #:transparent)

(define (make-pointer base [offset (bvpointer 0)])
  (pointer base offset))

(define (pointer-add ptr off)
  (pointer (pointer-base ptr)
           (bvadd (pointer-offset ptr)
                  (sign-extend off (bvpointer?)))))
