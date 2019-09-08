#lang rosette/safe

(require rosette/lib/roseunit)
(require racket/match)
(require racket/contract)
(require rackunit)
(require (only-in racket/base
  struct-type? struct-type-make-predicate struct-type-make-constructor
  error for/list string->symbol symbol->string hash-set! make-hash hash-ref
  hash-has-key? struct-copy for values))

(provide (except-out (all-from-out rosette/safe) struct forall exists))
(provide (all-from-out racket/contract))
(provide struct-copy values require)

(struct field (name type) #:transparent)
(define-values (prop:fields fields? fields-ref)
  (make-struct-type-property 'fields))


(define (make-fresh name type)
  (cond
    [(solvable? type)
      (constant (list name ((current-oracle) name)) type)]
    [(struct-type? type)
      (define fields
        (for/list ([f (fields-ref type)])
          (define n (string->symbol
                      (format "~a.~a"
                              (symbol->string name)
                              (symbol->string (field-name f)))))
          (make-fresh n (field-type f))))
      (apply (struct-type-make-constructor type) fields)]
    [else
      (error "Expected solvable or struct type for" name ", found" type)]))

(struct quantifier (qfr vars body) #:transparent)
(define quantifier-hash (make-hash))

(define-syntax (spec-quantifier stx)
  (syntax-case stx ()
    [(_ qfr ([name type] ...) body)
      (andmap identifier? (syntax->list #'(name ...)))
      (quasisyntax/loc stx
        (let ([b (make-fresh 'b boolean?)]
              [fnbody (lambda (name ...) body)]
              [vars (list (make-fresh 'name type) ...)])
          (hash-set! quantifier-hash b (quantifier qfr vars (apply fnbody vars)))
          b
        ))]))

(provide hash-ref quantifier-hash)

(define (expand-quantifiers expr)
  (match expr
    [b #:when (hash-has-key? quantifier-hash b)
      (define q (hash-ref quantifier-hash b))
      ((quantifier-qfr q) (symbolics (quantifier-vars q)) (expand-quantifiers (quantifier-body q)))]
    [(expression op e ...)
      (apply op (map expand-quantifiers e))]
    [_ expr]))

(define (skolemize expr)
  (match expr
    ; DeMorgan !(x \/ y) -> !x /\ !y
    [(expression (== !) (expression (== ||) e ...))
      (apply && (map skolemize (map ! e)))]
    ; DeMorgan !(x /\ y) -> !x \/ !y
    [(expression (== !) (expression (== &&) e ...))
      (apply || (map skolemize (map ! e)))]
    ; Double negation elimination
    [(expression (== !) (expression (== !) e))
      (skolemize e)]
    ; Throw away existentials
    [(expression (== exists) vars body) (skolemize body)]

    [b #:when (hash-has-key? quantifier-hash b)
      (define q (hash-ref quantifier-hash b))
      (cond
        [(equal? exists (quantifier-qfr q)) (skolemize (quantifier-body q))]
        [(equal? forall (quantifier-qfr q))
          ; FIXME safely recurse here.
          ; Skolemizing the body directly is not sound as it doesn't consider
          ; this binder.
          (forall (symbolics (quantifier-vars q))
                  (expand-quantifiers (quantifier-body q)))])]

    [(expression (== !) b) #:when (hash-has-key? quantifier-hash b)
      (define q (hash-ref quantifier-hash b))
      (define-symbolic* b2 boolean?)
      (cond
        ; ! exists ==> forall !
        [(equal? exists (quantifier-qfr q))
          (hash-set! quantifier-hash b2
            (struct-copy quantifier q
              [qfr forall]
              [body (! (quantifier-body q))]))]
        ; ! forall ==> exists !
        [(equal? forall (quantifier-qfr q))
          (hash-set! quantifier-hash b2
            (struct-copy quantifier q
              [qfr exists]
              [body (! (quantifier-body q))]))])
      (skolemize b2)]

    [_ expr]
  ))

(provide skolemize)

(define-syntax (spec-forall stx)
  (syntax-case stx ()
    ([_ e ...] (syntax/loc stx (spec-quantifier forall e ...)))))
(provide (rename-out [spec-forall forall]
                     [spec-forall ∀]))

(define-syntax (spec-exists stx)
  (syntax-case stx ()
    ([_ e ...] (syntax/loc stx (spec-quantifier exists e ...)))))
(provide (rename-out [spec-exists exists]
                     [spec-exists ∃]))

(define (prove thm)
  (define expr (skolemize (! thm)))
  (define asserted (asserts))
  (clear-asserts!)
  (check-equal? (asserts) null)
  (begin0
    (check-unsat (solve (assert expr)))
    (check-unsat (verify (assert (apply && asserted))))
    (for ([a asserted])
      (assert a))))

(provide prove)

(define-syntax (spec-struct stx)
  (syntax-case stx ()
    [(_ name ([fieldname type] ...))
     (let ([name-string (symbol->string (syntax->datum #'name))])
     (quasisyntax/loc stx
       (begin
         (struct name (fieldname ...)
           #:transparent
           #:property
            prop:fields
            (list (field 'fieldname type) ...)
           #:guard
            (struct-guard/c
              (let ([t type])
                (cond
                  [(struct-type? t)
                    (struct-type-make-predicate t)]
                  [(solvable? t) t]
                  [else (error "Expected solvable or struct type, found:" t)])) ...)
         ))))]))

(provide (rename-out [spec-struct struct]))
