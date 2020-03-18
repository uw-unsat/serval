#lang rosette

(require "debug.rkt" "unittest.rkt")

(provide (all-defined-out))

; Generic symbolic optimizations for users of Serval library.

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
