#lang rosette

; Helper procedures for dealing with uninterpreted functions.

(provide (all-defined-out))

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
