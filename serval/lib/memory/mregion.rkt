#lang rosette

(require "mblock.rkt" "../debug.rkt" "manager.rkt")

(provide (all-defined-out))

; An mregion is an mblock with information about its
; location in memory, since pointers are just integers
(struct mregion (start end name block) #:transparent)

(define (mregion-copy mr)
  (struct-copy mregion mr
    [block (mblock-copy (mregion-block mr))]))

(define (create-mregions symbols globals)
  (for/list ([entry symbols] #:when (member (list-ref entry 2) (list 'b 'B 'R)))
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
      [_ (bug #:msg (format "create-mregions: bad symbol format: ~e" entry))])))

; Find an symbol by its name
(define (find-symbol-by-name ls name)
  (findf (match-lambda [(list _ _ _ nm) (equal? nm name)]) ls))

; Find an mregion by its name
(define (find-mregion-by-name ls name)
  (define result (findf (lambda (mr) (equal? name (mregion-name mr))) ls))
  (bug-on (equal? result #f) #:msg (format "find-mregion-by-name: No such mregion ~e" name))
  result)

(define (find-block-by-name mregions name)
  (mregion-block (find-mregion-by-name mregions name)))

(define (find-mregion-by-addr ls addr)
  (bug-on (term? addr) #:msg "find-mregion-by-addr: symbolic addr")
  (findf (lambda (mr)
    (&& (bvule (bvpointer (mregion-start mr)) addr)
        (bvult addr (bvpointer (mregion-end mr)))))
    ls))

; Guess which mregion corressponds to a given address.
(define (guess-mregion-from-addr ls addr off)
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
      (let ([x (find-mregion-by-addr ls (bvadd (sign-extend off (bitvector (target-pointer-bitwidth))) literal))])
        (if x x
          (find-mregion-by-addr ls literal)))]

    [_ (bug #:msg "guess-mregion-from-addr: unknown term")]))

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
