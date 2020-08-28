#lang racket

(require
  racket/list
  racket/match
  racket/port
  racket/string
  syntax/strip-context)

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
    (literal-read-syntax #f in)))

(define (read-syms in)
  (datum->syntax #f
    (filter (compose not false?)
      (for/list ([l (filter non-empty-string? (port->lines in))])
        (match l
          ; Two addrs
          [(pregexp #px"^([0-9a-f]+) ([0-9a-f]+) (\\S) (\\S+)$" (list _ begin size type name))
             (with-syntax ([begin (string->number begin 16)]
                           [end (+ (string->number begin 16)
                                   (string->number size 16))]
                           [type (string->symbol type)]
                           [name (string->symbol name)])
               #'(begin end type name))]

          ; No size
          [(pregexp #px"^([0-9a-f]+) (\\S) (\\S+)$" (list _ begin type name))
             (with-syntax ([begin (string->number begin 16)]
                           [type (string->symbol type)]
                           [name (string->symbol name)])
               #'(begin begin type name))]
          [_ #f])))))


(define (literal-read-syntax src in)
  (strip-context
    #`(module anything racket/base
        (provide symbols)
        (define symbols '#,(read-syms in)))))
