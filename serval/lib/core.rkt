#lang rosette

(require
  rosette/solver/smt/z3
  "bvarith.rkt"
  "debug.rkt"
  "unittest.rkt"
  "memory/mblock.rkt"
  "memory/mregion.rkt"
  (prefix-in racket: racket/base)
  (only-in rosette [list var]))

(provide
  (all-defined-out)
  (all-from-out "bvarith.rkt")
  (all-from-out "debug.rkt")
  (all-from-out "memory/mblock.rkt")
  (all-from-out "memory/mregion.rkt")
  var)

; solver setup

(current-solver (z3 #:options (hash ':auto-config 'false ':smt.relevancy 0)))

; parameters

(define target-endian (make-parameter 'little))

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

(define (aligned? x alignment)
  (zero? (remainder x alignment)))

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
