#lang rosette

(require
  (for-syntax
    (only-in racket/syntax format-id))
  "../base.rkt")

(provide define-insn)


; The main macro for defining instructions.

(define-syntax (define-insn stx)
  (syntax-case stx ()
    [(_ (arg ...) #:encode encode [(field ...) op interp] ...)
     #'(begin
         (struct op (arg ...)
          #:transparent
          #:guard (lambda (arg ... name)
                    (values
                      ; split for type checking
                      (for/all ([arg arg #:exhaustive])
                        (guard arg)
                        arg) ...))
          #:methods gen:instruction
          [(define (instruction-encode insn)
             (define lst
               (match-let ([(op arg ...) insn])
                 ((lambda (arg ...) (encode field ...)) arg ...)))
             (apply concat (map (lambda (x) (if (box? x) (unbox x) x)) lst)))
           (define (instruction-run insn cpu)
             (match-let ([(op arg ...) insn])
               (interp cpu arg ...)))])
         ... )]))


; Type checking guards.

(define-syntax (guard stx)
  (syntax-case stx ()
    [(_ arg)
     (with-syntax ([ctor (format-id stx "guard-~a" (syntax-e #'arg))])
       #'(ctor arg))]))

(define-syntax (define-guard stx)
  (syntax-case stx ()
    [(_ name type)
     (with-syntax ([ctor (format-id stx "guard-~a" (syntax-e #'name))])
       #'(define (ctor v)
           (assert (type v) (format "~a: expected type ~a" v type))))]))

(define-guard cond (bitvector 4))
(define-guard hw (bitvector 2))
(define-guard imm6 (bitvector 6))
(define-guard immr (bitvector 6))
(define-guard imms (bitvector 6))
(define-guard imm12 (bitvector 12))
(define-guard imm16 (bitvector 16))
(define-guard imm19 (bitvector 19))
(define-guard imm26 (bitvector 26))
(define-guard op2 (bitvector 2))
(define-guard opc (bitvector 2))
(define-guard option (bitvector 3))
(define-guard sf (bitvector 1))
(define-guard sh (bitvector 1))
(define-guard shift (bitvector 2))
(define-guard size (bitvector 2))

(define-guard A (bitvector 1))
(define-guard N (bitvector 1))
(define-guard R (bitvector 1))
(define-guard Ra (bitvector 5))
(define-guard Rd (bitvector 5))
(define-guard Rm (bitvector 5))
(define-guard Rn (bitvector 5))
(define-guard Rs (bitvector 5))
(define-guard Rt (bitvector 5))
(define-guard S (bitvector 1))
