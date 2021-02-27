#lang rosette

(require (for-syntax syntax/parse))
(provide (all-defined-out))

(define target-spectre (make-parameter #f))

(define (concrete?)
  (vc-true? (vc)))

(struct bug-info (file line message) #:transparent)

(define bug-db (make-parameter null))

(define (clear-bug-info!) (bug-db null))

(define (bug-format info sol)
  (define message (bug-info-message info))
  (define evaluated-message
    (cond
      [(string? message) message]
      [(procedure? message) (message sol)]
      [else "<unknown>"]))
  (format "~a:~a ~a"
          (bug-info-file info)
          (bug-info-line info)
          evaluated-message))

(define-syntax (bug-assert stx)
  (with-syntax ([line (syntax-line stx)]
                [file (path->string (syntax-source stx))])
    (syntax-parse stx
      [(_ condition:expr (~optional (~seq #:msg msg:expr)))
        #'(add-assert condition #:file file #:line line #:msg (~? msg #f))])))

(define-syntax (bug-on stx)
  (with-syntax ([line (syntax-line stx)]
                [file (syntax-source stx)])
    (syntax-parse stx
      [(_ condition:expr (~optional (~seq #:msg msg:expr)))
        #'(add-assert (! condition) #:file file #:line line #:msg (~? msg #f))])))

(define-syntax (bug stx)
  (with-syntax ([line (syntax-line stx)]
                [file (syntax-source stx)])
    (syntax-parse stx
      [(_ (~optional (~seq #:msg msg:expr)))
        #'(add-assert #f #:file file #:line line #:msg (~? msg #f))])))

(define (add-assert assertion #:file [file "<unknown>"] #:line [line "<unknown>"] #:msg [msg #f])
  ; Create debug information.
  (define info (bug-info file line msg))
  ; Extract current assumptions and assertions.
  (define asserts (vc-asserts (vc)))
  (define assumes (vc-assumes (vc)))
  ; Set vc to true to add debug info. If the assertion is false given prior assumptions and assertions,
  ; the expression evaluates to the debug info. Otherwise it evaluates to #f.
  (with-vc vc-true
    (bug-db (cons (if (&& asserts assumes (! assertion)) info #f) (bug-db))))
  ; show a concrete message if this is trivially false
  (define msg-str (bug-format info (sat)))
  (assert assertion msg-str))

(define (get-bug-info model)
  ; We want all assertion violates which are not unions (indicating incomplete solution)
  ; and are not concretely #f.
  (filter (lambda (i) (and (not (union? i)) (not (false? i)))) (evaluate (bug-db) model)))
