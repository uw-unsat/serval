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

(define (gpr? r)
  (and (box? r) ((bitvector 4) (unbox r))))

(define-guard P (bitvector 1))
(define-guard Rd gpr?)
(define-guard RdHi gpr?)
(define-guard RdLo gpr?)
(define-guard Rm gpr?)
(define-guard Rn gpr?)
(define-guard Rs gpr?)
(define-guard Rt gpr?)
(define-guard S (bitvector 1))
(define-guard U (bitvector 1))
(define-guard W (bitvector 1))

(define-guard imm4 (bitvector 4))
(define-guard imm4H (bitvector 4))
(define-guard imm4L (bitvector 4))
(define-guard imm5 (bitvector 5))
(define-guard imm12 (bitvector 12))
(define-guard imm24 (bitvector 24))
(define-guard rotate (bitvector 2))
(define-guard stype (bitvector 2))
(define-guard register_list (bitvector 16))
