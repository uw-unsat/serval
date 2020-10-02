#lang rosette

(require
  (for-syntax
    (only-in racket/syntax format-id))
  "../base.rkt"
  "../decode.rkt")

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
               (interp cpu arg ...)))]) ...
         (add-decoder op
           ((lambda (arg ...) (encode field ...)) (typeof arg) ...))
         ... )]))


; Type checking guards.

(define-syntax-rule (guard v)
  (let ([type (typeof v)])
    (define expr
      (cond
        [(box? type)
         (set! type (unbox type))
         (and (box? v) (type (unbox v)))]
        [else
         (type v)]))
    (assert expr (format "~a: expected type ~a" v type))))

(define-syntax (typeof stx)
  (syntax-case stx ()
    [(_ arg)
     (with-syntax ([type (format-id stx "typeof-~a" (syntax-e #'arg))])
       #'type)]))

(define typeof-cond (bitvector 4))
(define typeof-hw (bitvector 2))
(define typeof-imm6 (bitvector 6))
(define typeof-imm7 (bitvector 7))
(define typeof-immr (bitvector 6))
(define typeof-imms (bitvector 6))
(define typeof-imm12 (bitvector 12))
(define typeof-imm16 (bitvector 16))
(define typeof-imm19 (bitvector 19))
(define typeof-imm26 (bitvector 26))
(define typeof-op2 (bitvector 2))
(define typeof-opc (bitvector 2))
(define typeof-option (bitvector 3))
(define typeof-sf (bitvector 1))
(define typeof-sh (bitvector 1))
(define typeof-shift (bitvector 2))
(define typeof-size (bitvector 2))

(define typeof-A (bitvector 1))
(define typeof-N (bitvector 1))
(define typeof-R (bitvector 1))
(define typeof-Ra (box (bitvector 5)))
(define typeof-Rd (box (bitvector 5)))
(define typeof-Rm (box (bitvector 5)))
(define typeof-Rn (box (bitvector 5)))
(define typeof-Rs (box (bitvector 5)))
(define typeof-Rt (box (bitvector 5)))
(define typeof-Rt2 (box (bitvector 5)))
(define typeof-S (bitvector 1))
