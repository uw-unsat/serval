#lang rosette

(require
  (for-syntax (only-in racket/syntax format-id))
  "../base.rkt"
  "../decode.rkt"
  "../register.rkt"
  "encoding.rkt"
  (prefix-in core: serval/lib/core))

(provide
  (for-syntax format-id)
  (all-defined-out)
  (all-from-out
    "../base.rkt"
    "../register.rkt"
    "encoding.rkt"
    serval/lib/core))


; The main macro for defining instructions.

(define-syntax (define-insn stx)
  (syntax-case stx ()
    [(_ op (arg ...) #:decode [(pat ...) result] ... #:encode (encode ...) interp)
     #'(begin
         (add-decoder
           (lambda (lst)
             (define (ctor x) (if (list? x) (apply op x) x))
             (match lst
               [(list pat ... rest ___) (cons (ctor result) rest)] ...
               [_ #f])))
         (struct op (arg ...)
          #:transparent
          #:methods gen:instruction
          [(define (instruction-encode insn)
             (match-let ([(op arg ...) insn])
               (flatten (encode ...))))
           (define (instruction-run insn cpu)
             (match-let ([(op arg ...) insn])
               (interp cpu arg ...)))]))]))


; flags

(define (parity x)
  (bvnot (apply bvxor (for/list ([i (in-range (core:bv-size x))]) (bit i x)))))

(define (cpu-pf+zf+sf-set! cpu val)
  (cpu-flag-set! cpu 'PF (parity (extract 7 0 val)))
  (cpu-flag-set! cpu 'ZF (bool->bitvector (bvzero? val)))
  (cpu-flag-set! cpu 'SF (msb val)))
